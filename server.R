server <- function(input, output, session) {
  
  # ==========================================================================
  # 01 | LOCAL INPUT HELPERS
  # ==========================================================================
  
  saved <- function(id, default) {
    isolate(input[[id]]) %||% default
  }
  
  read_number <- function(id, label, lower = 0, upper = Inf) {
    x <- input[[id]]
    
    if (is.null(x) ||
        length(x) != 1L ||
        !is.finite(x) ||
        x < lower ||
        x > upper) {
      stop(paste("Enter a valid value for", label))
    }
    
    x
  }
  
  read_arm_count <- function() {
    n <- read_number(
      "n_arms",
      "number of initial arms",
      lower = 2
    )
    
    if (n != floor(n)) {
      stop("The number of initial arms must be an integer.")
    }
    
    n
  }
  
  safe_state <- function(expr) {
    tryCatch(
      list(
        ok = TRUE,
        value = force(expr)
      ),
      
      error = function(e) {
        list(
          ok = FALSE,
          message = conditionMessage(e)
        )
      }
    )
  }
  
  
  # ==========================================================================
  # 02 | TRUTH: COMMITTED TREATMENT AND STAGE DEFINITIONS
  # ==========================================================================
  
  definitions <- reactiveVal(list(
    drugs = c("Drug", "SoC"),
    stages = c("Induction", "Maintenance")
  ))
  
  observeEvent(input$apply_definitions, {
    result <- safe_state({
      drugs <- parse_names(input$drug_names)
      stages <- parse_names(input$stage_names)
      
      if (length(stages) > 2L) {
        stop("This version supports one or two stages.")
      }
      
      list(
        drugs = drugs,
        stages = stages
      )
    })
    
    if (result$ok) {
      definitions(result$value)
    } else {
      showNotification(
        result$message,
        type = "error"
      )
    }
  })
  
  
  # ==========================================================================
  # 03 | TRUTH: PARAMETER ROW DEFINITIONS
  # Independent of trial arms and allocation.
  # ==========================================================================
  
  parameter_rows <- reactive({
    defs <- definitions()
    
    first <- data.frame(
      stage = 1L,
      first = defs$drugs,
      current = defs$drugs,
      response = NA_integer_,
      stringsAsFactors = FALSE
    )
    
    first$id <- vapply(
      first$first,
      
      function(d) {
        parameter_id(1, d)
      },
      
      character(1)
    )
    
    if (length(defs$stages) == 1L) {
      return(first)
    }
    
    second <- expand.grid(
      first = defs$drugs,
      current = defs$drugs,
      
      response = if (isTRUE(input$response_enabled)) {
        c(0L, 1L)
      } else {
        NA_integer_
      },
      
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    )
    
    second$stage <- 2L
    
    second$id <- vapply(
      seq_len(nrow(second)),
      
      function(i) {
        parameter_id(
          2,
          second$first[i],
          second$current[i],
          
          if (!is.na(second$response[i])) {
            second$response[i]
          } else {
            NULL
          }
        )
      },
      
      character(1)
    )
    
    rbind(
      first,
      second[, names(first)]
    )
  })
  
  
  # ==========================================================================
  # 04 | TRUTH: RESPONSE CONTROLS
  # ==========================================================================
  
  output$response_controls <- renderUI({
    defs <- definitions()
    
    if (length(defs$stages) == 1L) {
      return(
        helpText(
          "Response after stage 1 requires a second stage."
        )
      )
    }
    
    if (!isTRUE(input$response_enabled)) {
      return(
        helpText(
          "Response is omitted from the current truth model."
        )
      )
    }
    
    tagList(
      helpText(
        "P(R=1), conditional on remaining eligible at assessment.",
        "Defined by initial treatment, regardless of trial arms."
      ),
      
      lapply(defs$drugs, function(d) {
        id <- paste0(
          "response_",
          drug_key(d)
        )
        
        numericInput(
          id,
          paste0(d, ": P(R=1)"),
          value = saved(id, 0.5),
          min = 0,
          max = 1,
          step = 0.05
        )
      })
    )
  })
  
  
  # ==========================================================================
  # 05 | TRUTH: SURVIVAL CONTROLS
  # Stage 1: initial treatment.
  # Stage 2: initial treatment x current treatment x response.
  # ==========================================================================
  
  output$hazard_controls <- renderUI({
    defs <- definitions()
    rows <- parameter_rows()
    
    tagList(
      lapply(seq_along(defs$stages), function(s) {
        stage_rows <- rows[
          rows$stage == s,
          ,
          drop = FALSE
        ]
        
        tags$details(
          open = if (s == 1L) NA else NULL,
          
          tags$summary(
            paste0(
              "Stage ", s, ": ",
              defs$stages[s]
            )
          ),
          
          br(),
          
          lapply(seq_len(nrow(stage_rows)), function(i) {
            row <- stage_rows[i, ]
            
            label <- if (s == 1L) {
              paste0(
                row$first,
                ": hazard"
              )
              
            } else {
              paste0(
                row$first,
                " → ",
                row$current,
                
                if (!is.na(row$response)) {
                  paste0(
                    " | R=",
                    row$response
                  )
                } else {
                  ""
                },
                
                ": hazard"
              )
            }
            
            numericInput(
              row$id,
              label,
              value = saved(row$id, 0.1),
              min = 0,
              step = 0.01
            )
          })
        )
      })
    )
  })
  
  
  # ==========================================================================
  # 06 | TRUTH: BUILD PARAMETER OBJECT
  # ==========================================================================
  
  truth_state <- reactive({
    safe_state({
      defs <- definitions()
      rows <- parameter_rows()
      
      rows$hazard <- vapply(
        rows$id,
        
        function(id) {
          read_number(
            id,
            "hazard"
          )
        },
        
        numeric(1)
      )
      
      use_response <- length(defs$stages) == 2L &&
        isTRUE(input$response_enabled)
      
      response <- NULL
      
      if (use_response) {
        response <- data.frame(
          treatment = defs$drugs,
          
          probability = vapply(
            defs$drugs,
            
            function(d) {
              read_number(
                paste0(
                  "response_",
                  drug_key(d)
                ),
                
                paste(
                  d,
                  "response probability"
                ),
                
                lower = 0,
                upper = 1
              )
            },
            
            numeric(1)
          ),
          
          stringsAsFactors = FALSE
        )
      }
      
      # --------------------------------------------------------------------------
      # Read independent random-censoring parameters
      # --------------------------------------------------------------------------
      
      censor_distribution <- input$censor_distribution %||% "none"
      
      positive_number <- function(id, label) {
        x <- read_number(id, label)
        
        if (x <= 0) {
          stop(paste(label, "must be positive."))
        }
        
        x
      }
      
      censor_parameters <- switch(
        censor_distribution,
        
        none = list(),
        
        exponential = list(
          rate = positive_number(
            "censor_rate",
            "Exponential censoring rate"
          )
        ),
        
        weibull = list(
          shape = positive_number(
            "censor_shape",
            "Weibull censoring shape"
          ),
          
          scale = positive_number(
            "censor_scale",
            "Weibull censoring scale"
          )
        ),
        
        uniform = {
          lower <- read_number(
            "censor_min",
            "Uniform censoring minimum"
          )
          
          upper <- read_number(
            "censor_max",
            "Uniform censoring maximum"
          )
          
          if (upper <= lower) {
            stop("Uniform censoring maximum must exceed its minimum.")
          }
          
          list(
            min = lower,
            max = upper
          )
        },
        
        stop("Select a valid censoring distribution.")
      )
      
      censoring <- list(
        mechanism = "independent",
        distribution = censor_distribution,
        parameters = censor_parameters
      )
      
      list(
        drugs = defs$drugs,
        stages = defs$stages,
        distribution = "exponential_within_stage",
        response_enabled = use_response,
        response = response,
        survival = rows,
        censoring = censoring
      )
    })
  })
  
  
  # ==========================================================================
  # 07 | DESIGN: INITIAL-ARM CONTROLS
  # Any integer arm count >= 2.
  # ==========================================================================
  
  output$initial_controls <- renderUI({
    defs <- definitions()
    
    count <- safe_state(
      read_arm_count()
    )
    
    if (!count$ok) {
      return(
        helpText(count$message)
      )
    }
    
    n <- count$value
    
    previous_ratio <- saved(
      "initial_ratio",
      ""
    )
    
    ratio_valid <- safe_state(
      parse_ratio(previous_ratio, n)
    )
    
    ratio_value <- if (ratio_valid$ok) {
      previous_ratio
    } else {
      paste(
        rep(1, n),
        collapse = ":"
      )
    }
    
    tagList(
      lapply(seq_len(n), function(i) {
        name_id <- paste0(
          "arm_name_",
          i
        )
        
        drug_id <- paste0(
          "arm_drug_",
          i
        )
        
        default_treatment <- defs$drugs[
          if (i <= 2L) 1L else min(2L, length(defs$drugs))
        ]
        previous <- saved(drug_id, default_treatment)
        
        tagList(
          textInput(
            name_id,
            paste("Arm", i, "label"),
            saved(
              name_id,
              if (i <= 3L && length(defs$drugs) >= 2L) {
                c(
                  paste(defs$drugs[1], defs$drugs[1], sep = " + "),
                  paste(defs$drugs[1], defs$drugs[2], sep = " + "),
                  paste(defs$drugs[2], defs$drugs[2], sep = " + ")
                )[i]
              } else {
                paste("Arm", i)
              }
            )
          ),
          
          selectInput(
            drug_id,
            paste("Arm", i, "initial treatment"),
            choices = defs$drugs,
            selected = intersect(
              previous,
              defs$drugs
            )
          )
        )
      }),
      
      textInput(
        "initial_ratio",
        "Initial allocation weights",
        value = ratio_value
      )
    )
  })
  
  
  # ==========================================================================
  # 08 | DESIGN: SECOND-STAGE CONTROLS
  # Every arm explicitly specifies its stage-2 treatment assignment.
  # No automatic continuation.
  # ==========================================================================
  
  output$stage2_controls <- renderUI({
    defs <- definitions()
    
    if (length(defs$stages) == 1L) {
      return(NULL)
    }
    
    count <- safe_state(
      read_arm_count()
    )
    
    if (!count$ok) {
      return(NULL)
    }
    
    n <- count$value
    
    tagList(
      h4(
        paste0(
          "Stage 2: ",
          defs$stages[2]
        )
      ),
      
      helpText(
        "For each arm, manually assign a treatment or re-randomize.",
        "Specified treatment means no re-randomization."
      ),
      
      lapply(seq_len(n), function(i) {
        prefix <- paste0(
          "s",
          i,
          "_"
        )
        
        rules <- c(
          "Everyone reaching stage 2" = "all"
        )
        
        if (isTRUE(input$response_enabled)) {
          rules <- c(
            rules,
            "Based on response" = "response"
          )
        }
        
        previous <- saved(
          paste0(prefix, "rule"),
          "all"
        )
        
        if (!previous %in% unname(rules)) {
          previous <- "all"
        }
        
        tags$details(
          open = NA,
          
          tags$summary(
            paste(
              "Stage-2 assignment: arm",
              i
            )
          ),
          
          selectInput(
            paste0(prefix, "rule"),
            "Assignment branches",
            choices = rules,
            selected = previous
          ),
          
          conditionalPanel(
            sprintf(
              "input['%srule'] === 'all'",
              prefix
            ),
            
            branch_ui(
              paste0(prefix, "all_"),
              "All eligible patients",
              defs$drugs,
              saved
            )
          ),
          
          conditionalPanel(
            sprintf(
              "input['%srule'] === 'response'",
              prefix
            ),
            
            branch_ui(
              paste0(prefix, "r1_"),
              "Responders (R=1)",
              defs$drugs,
              saved
            ),
            
            branch_ui(
              paste0(prefix, "r0_"),
              "Nonresponders (R=0)",
              defs$drugs,
              saved
            )
          )
        )
      })
    )
  })
  
  
  # ==========================================================================
  # 09 | DESIGN: STAGE TIMING
  # ==========================================================================
  
  output$timing_controls <- renderUI({
    defs <- definitions()
    
    if (length(defs$stages) == 1L) {
      return(
        helpText(
          "Single-stage model: no stage-transition time."
        )
      )
    }
    
    numericInput(
      "stage2_time",
      "Stage 2 start / response-assessment time",
      value = saved(
        "stage2_time",
        1
      ),
      min = 0.01,
      step = 0.1
    )
  })
  
  
  # ==========================================================================
  # 10 | DESIGN: READ SECOND-STAGE BRANCH
  # ==========================================================================
  
  read_branch <- function(prefix, name, drugs) {
    action <- input[[
      paste0(prefix, "action")
    ]]
    
    if (is.null(action)) {
      stop(
        "Waiting for second-stage controls."
      )
    }
    
    if (action == "fixed") {
      treatment <- input[[
        paste0(prefix, "fixed")
      ]]
      
      if (length(treatment) != 1L ||
          !treatment %in% drugs) {
        stop(
          "Select a valid second-stage treatment."
        )
      }
      
      return(
        list(
          name = name,
          action = "fixed",
          treatments = treatment,
          allocation = 1
        )
      )
    }
    
    treatments <- input[[
      paste0(prefix, "treatments")
    ]]
    
    if (length(treatments) < 2L ||
        any(!treatments %in% drugs)) {
      stop(
        "Select at least two treatments for re-randomization."
      )
    }
    
    list(
      name = name,
      action = "random",
      treatments = treatments,
      
      allocation = parse_ratio(
        input[[
          paste0(prefix, "ratio")
        ]] %||% "",
        
        length(treatments)
      )
    )
  }
  
  
  # ==========================================================================
  # 11 | DESIGN: BUILD DESIGN OBJECT
  # Stage 2 always uses explicitly selected assignment rules.
  # ==========================================================================
  
  design_state <- reactive({
    safe_state({
      defs <- definitions()
      n <- read_arm_count()
      
      # Initial sample size
      initial_n <- read_number(
        "initial_n",
        "initial sample size",
        lower = 2
      )
      
      if (initial_n != floor(initial_n)) {
        stop("Initial sample size must be an integer.")
      }
      
      # Common administrative follow-up cutoff
      admin_cutoff <- read_number(
        "admin_cutoff",
        "maximum follow-up"
      )
      
      if (admin_cutoff <= 0) {
        stop("Maximum follow-up must be positive.")
      }
      
      allocation <- parse_ratio(
        input$initial_ratio %||% "",
        n
      )
      
      arm_names <- vapply(
        seq_len(n),
        
        function(i) {
          trimws(
            input[[
              paste0("arm_name_", i)
            ]] %||% ""
          )
        },
        
        character(1)
      )
      
      if (any(!nzchar(arm_names)) ||
          anyDuplicated(arm_names)) {
        stop(
          "Arm labels must be nonempty and distinct."
        )
      }
      
      has_stage2 <- length(defs$stages) == 2L
      transition_time <- NULL
      
      if (has_stage2) {
        transition_time <- read_number(
          "stage2_time",
          "stage-2 time"
        )
        
        if (transition_time <= 0) {
          stop(
            "Stage-2 time must be positive."
          )
        }
      }
      
      if (has_stage2 && admin_cutoff <= transition_time) {
        stop("Maximum follow-up must exceed the stage-2 start time.")
      }
      
      arms <- lapply(seq_len(n), function(i) {
        treatment <- input[[
          paste0("arm_drug_", i)
        ]]
        
        if (length(treatment) != 1L ||
            !treatment %in% defs$drugs) {
          stop(
            "Select an initial treatment for every arm."
          )
        }
        
        prefix <- paste0(
          "s",
          i,
          "_"
        )
        
        branches <- list()
        response_based <- FALSE
        
        if (has_stage2) {
          rule <- input[[
            paste0(prefix, "rule")
          ]]
          
          if (is.null(rule)) {
            stop(
              "Waiting for second-stage assignment rules."
            )
          }
          
          response_based <- isTRUE(input$response_enabled) &&
            identical(rule, "response")
          
          if (response_based) {
            branches <- list(
              read_branch(
                paste0(prefix, "r1_"),
                "Responder",
                defs$drugs
              ),
              
              read_branch(
                paste0(prefix, "r0_"),
                "Nonresponder",
                defs$drugs
              )
            )
            
          } else {
            branches <- list(
              read_branch(
                paste0(prefix, "all_"),
                "All eligible",
                defs$drugs
              )
            )
          }
        }
        
        list(
          name = arm_names[i],
          treatment = treatment,
          response_based = response_based,
          branches = branches
        )
      })
      
      list(
        stages = defs$stages,
        transition_time = transition_time,
        allocation = allocation,
        arms = arms,
        
        recruitment = list(
          initial_n = as.integer(initial_n),
          additional_n = 0,
          entry_time = 0
        ),
        
        administrative_censoring = list(
          type = "fixed",
          cutoff = admin_cutoff
        )
      )
    })
  })
  
  
  # ==========================================================================
  # 12 | DISPLAY: VALIDATION AND FLOWCHART
  # ==========================================================================
  
  output$truth_status <- renderUI({
    state <- truth_state()
    
    if (!state$ok) {
      div(
        class = "alert alert-info",
        state$message
      )
    }
  })
  
  output$design_status <- renderUI({
    state <- design_state()
    
    if (!state$ok) {
      div(
        class = "alert alert-info",
        state$message
      )
    }
  })
  
  output$trial_flow <- renderGrViz({
    state <- design_state()
    
    validate(
      need(
        state$ok,
        state$message %||% "Complete the trial design."
      )
    )
    
    grViz(
      design_to_dot(state$value)
    )
  })
  
  
  # ==========================================================================
  # 13 | DISPLAY: INSPECT PARAMETER OBJECTS
  # ==========================================================================
  
  output$truth_object <- renderPrint({
    state <- truth_state()
    
    if (state$ok) {
      str(state$value)
    } else {
      cat(state$message)
    }
  })
  
  output$design_object <- renderPrint({
    state <- design_state()
    
    if (state$ok) {
      str(state$value)
    } else {
      cat(state$message)
    }
  })
  
  
  # ==========================================================================
  # 14 | DATA GENERATION — RESERVED
  # ==========================================================================
  
  
  # ==========================================================================
  # 15 | SIMULATION: IN-MEMORY COLLECTION AND RESULTS
  # ==========================================================================
  
  dataset_run <- reactiveVal(NULL)
  analysis_run <- reactiveVal(NULL)
  
  dataset_message <- reactiveVal("No datasets generated or loaded.")
  analysis_message <- reactiveVal("No analysis run.")
  
  comparison_count <- reactiveVal(1L)
  
  read_integer <- function(id, label, lower = 1,
                           upper = .Machine$integer.max) {
    x <- read_number(id, label, lower, upper)
    
    if (x != floor(x)) {
      stop(paste(label, "must be an integer."))
    }
    
    as.integer(x)
  }
  
  output_root <- function() {
    root <- trimws(input$sim_output_root %||% "")
    
    if (!nzchar(root)) {
      stop("Enter an output folder.")
    }
    
    path.expand(root)
  }
  
  
  # ==========================================================================
  # 16 | SIMULATION: RETAIN COLLECTION AND SAVE TO DISK
  # Memory remains available even if disk saving fails.
  # ==========================================================================
  
  retain_collection <- function(bundle, prefix = "run") {
    dataset_run(list(
      bundle = bundle,
      directory = NULL
    ))
    
    analysis_run(NULL)
    analysis_message("No analysis run for this collection.")
    
    updateNumericInput(
      session,
      "preview_dataset",
      value = 1,
      min = 1,
      max = length(bundle$datasets)
    )
    
    save_result <- safe_state({
      directory <- new_run_directory(
        output_root(),
        prefix = prefix
      )
      
      saveRDS(
        bundle,
        file.path(directory, "datasets.rds")
      )
      
      directory
    })
    
    if (save_result$ok) {
      dataset_run(list(
        bundle = bundle,
        directory = save_result$value
      ))
      
      dataset_message(paste0(
        length(bundle$datasets),
        " datasets retained in memory.\n",
        "Saved to:\n",
        save_result$value,
        "/datasets.rds"
      ))
      
    } else {
      dataset_message(paste0(
        length(bundle$datasets),
        " datasets retained in memory.\n",
        "Disk saving failed: ",
        save_result$message
      ))
      
      showNotification(
        "Datasets are in memory, but automatic saving failed.",
        type = "error",
        duration = NULL
      )
    }
  }
  
  
  # ==========================================================================
  # 17 | SIMULATION: GENERATE THE DATASET COLLECTION
  # Snapshot Truth and Design before generation.
  # ==========================================================================
  
  observeEvent(input$generate_datasets, {
    result <- safe_state({
      truth <- truth_state()
      design <- design_state()
      
      if (!truth$ok) stop(truth$message)
      if (!design$ok) stop(design$message)
      
      repetitions <- read_integer(
        "sim_reps",
        "Number of datasets"
      )
      
      seed <- read_integer(
        "sim_seed",
        "Random seed",
        lower = 0
      )
      
      truth_snapshot <- truth$value
      design_snapshot <- design$value
      
      if (is.null(design_snapshot$recruitment) ||
          is.null(design_snapshot$administrative_censoring)) {
        stop("Add initial sample size and administrative cutoff first.")
      }
      
      # Validate the requested output root before doing the work.
      output_root()
      
      set.seed(seed)
      
      datasets <- vector(
        "list",
        repetitions
      )
      
      withProgress(
        message = "Generating datasets",
        value = 0,
        
        {
          for (i in seq_len(repetitions)) {
            datasets[[i]] <- generate_trial(
              truth_snapshot,
              design_snapshot
            )
            
            if (i == 1L ||
                i %% max(1L, repetitions %/% 100L) == 0L ||
                i == repetitions) {
              setProgress(
                value = i / repetitions,
                detail = paste(i, "of", repetitions)
              )
            }
          }
        }
      )
      
      names(datasets) <- sprintf(
        "dataset_%05d",
        seq_len(repetitions)
      )
      
      list(
        format_version = "FDA_COP_datasets_v1",
        generated_at = Sys.time(),
        seed = seed,
        rng_kind = RNGkind(),
        repetitions = repetitions,
        truth = truth_snapshot,
        design = design_snapshot,
        datasets = datasets,
        session_info = sessionInfo()
      )
    })
    
    if (!result$ok) {
      showNotification(
        result$message,
        type = "error",
        duration = NULL
      )
      
      return()
    }
    
    retain_collection(result$value)
  })
  
  
  # ==========================================================================
  # 18 | SIMULATION: LOAD A PREVIOUS COLLECTION
  # ==========================================================================
  
  observeEvent(input$load_datasets, {
    req(input$load_datasets$datapath)
    
    result <- safe_state({
      bundle <- readRDS(
        input$load_datasets$datapath
      )
      
      if (!is.list(bundle) ||
          !identical(
            bundle$format_version,
            "FDA_COP_datasets_v1"
          ) ||
          !is.list(bundle$datasets) ||
          !length(bundle$datasets) ||
          length(bundle$design$arms) < 2L) {
        stop("This is not a supported FDA_COP dataset collection.")
      }
      
      required <- c(
        "id", "arm", "time", "event"
      )
      
      valid <- vapply(
        bundle$datasets,
        
        function(d) {
          if (!is.data.frame(d) ||
              !all(required %in% names(d)) ||
              !nrow(d)) {
            return(FALSE)
          }
          
          all(is.finite(d$time)) &&
            all(d$time >= 0) &&
            all(d$event %in% c(0, 1)) &&
            all(d$arm %in% seq_along(bundle$design$arms))
        },
        
        logical(1)
      )
      
      if (!all(valid)) {
        stop("One or more saved datasets are invalid.")
      }
      
      bundle
    })
    
    if (!result$ok) {
      showNotification(
        result$message,
        type = "error",
        duration = NULL
      )
      
      return()
    }
    
    retain_collection(
      result$value,
      prefix = "loaded"
    )
  })
  
  
  # ==========================================================================
  # 19 | ANALYZE: MULTIPLE COMPARISON CONTROLS
  # Choices come from the SAVED design, not current Design-tab inputs.
  # ==========================================================================
  
  observeEvent(input$add_comparison, {
    comparison_count(
      comparison_count() + 1L
    )
  })
  
  observeEvent(input$remove_comparison, {
    comparison_count(
      max(1L, comparison_count() - 1L)
    )
  })
  
  output$direct_comparison_controls <- renderUI({
    run <- dataset_run()
    
    if (is.null(run)) {
      return(
        helpText(
          "Generate or load datasets first."
        )
      )
    }
    
    arms <- run$bundle$design$arms
    ids <- as.character(seq_along(arms))
    
    choices <- setNames(
      ids,
      vapply(
        seq_along(arms),
        
        function(i) {
          paste0(i, ": ", arms[[i]]$name)
        },
        
        character(1)
      )
    )
    
    tagList(
      lapply(seq_len(comparison_count()), function(j) {
        ref_id <- paste0("cmp_ref_", j)
        cmp_id <- paste0("cmp_arm_", j)
        
        ref <- saved(ref_id, ids[1])
        cmp <- saved(cmp_id, ids[min(j + 1L, length(ids))])
        
        if (!ref %in% ids) ref <- ids[1]
        if (!cmp %in% ids) cmp <- ids[2]
        
        tagList(
          h5(paste("Comparison", j)),
          
          fluidRow(
            column(
              6,
              
              selectInput(
                ref_id,
                "Reference",
                choices,
                selected = ref
              )
            ),
            
            column(
              6,
              
              selectInput(
                cmp_id,
                "Comparison",
                choices,
                selected = cmp
              )
            )
          )
        )
      })
    )
  })
  
  
  # ==========================================================================
  # 20 | ANALYZE: READ DIRECT-ANALYSIS SETTINGS
  # ==========================================================================
  
  direct_settings <- reactive({
    safe_state({
      run <- dataset_run()
      
      if (is.null(run)) {
        stop("Generate or load datasets first.")
      }
      
      arms <- run$bundle$design$arms
      valid_ids <- as.character(seq_along(arms))
      
      comparisons <- do.call(
        rbind,
        
        lapply(seq_len(comparison_count()), function(j) {
          ref <- input[[paste0("cmp_ref_", j)]]
          cmp <- input[[paste0("cmp_arm_", j)]]
          
          if (length(ref) != 1L ||
              length(cmp) != 1L ||
              !ref %in% valid_ids ||
              !cmp %in% valid_ids) {
            stop("Select valid arms for every comparison.")
          }
          
          if (ref == cmp) {
            stop("Reference and comparison arms must differ.")
          }
          
          r <- as.integer(ref)
          c <- as.integer(cmp)
          
          data.frame(
            comparison_id = j,
            reference = r,
            comparison = c,
            
            label = paste0(
              arms[[c]]$name,
              " vs ",
              arms[[r]]$name
            ),
            
            stringsAsFactors = FALSE
          )
        })
      )
      
      keys <- paste(
        comparisons$reference,
        comparisons$comparison,
        sep = ":"
      )
      
      if (anyDuplicated(keys)) {
        stop("Remove duplicate comparison rows.")
      }
      
      alpha <- read_number(
        "analysis_alpha",
        "Significance level"
      )
      
      if (alpha <= 0 || alpha >= 1) {
        stop("Significance level must be between 0 and 1.")
      }
      
      test <- input$direct_test
      
      if (!test %in% c("cox", "logrank")) {
        stop("Select a supported direct analysis.")
      }
      
      if (test == "cox" &&
          input$direct_variance == "robust" &&
          input$direct_ties == "exact") {
        stop("Robust Cox variance requires Efron or Breslow ties.")
      }
      
      list(
        comparisons = comparisons,
        test = test,
        variance = input$direct_variance,
        ties = input$direct_ties,
        alpha = alpha,
        sided = input$analysis_sided,
        adjustment = input$direct_adjustment
      )
    })
  })
  
  
  # ==========================================================================
  # 21 | ANALYZE: RUN DIRECT ANALYSES ON EXISTING DATASETS
  # Does not call generate_trial() or change the random seed.
  # ==========================================================================
  
  observeEvent(input$run_direct_analysis, {
    result <- safe_state({
      run <- dataset_run()
      state <- direct_settings()
      
      if (!state$ok) stop(state$message)
      
      settings <- state$value
      comparisons <- settings$comparisons
      datasets <- run$bundle$datasets
      
      rows <- vector(
        "list",
        length(datasets)
      )
      
      withProgress(
        message = "Analyzing saved datasets",
        value = 0,
        
        {
          for (i in seq_along(datasets)) {
            one <- do.call(
              rbind,
              
              lapply(seq_len(nrow(comparisons)), function(j) {
                fit <- analyze_direct(
                  datasets[[i]],
                  reference = comparisons$reference[j],
                  comparison = comparisons$comparison[j],
                  settings = settings
                )
                
                fit$dataset_id <- i
                fit$comparison_id <- j
                fit$comparison <- comparisons$label[j]
                fit$alpha <- settings$alpha
                
                fit
              })
            )
            
            # Preserve the planned family size when a fit fails.
            # Failed tests remain NA in reported results.
            p_for_adjustment <- ifelse(
              one$ok & is.finite(one$p),
              one$p,
              1
            )
            
            one$p_adjusted <- p.adjust(
              p_for_adjustment,
              method = settings$adjustment
            )
            
            one$p_adjusted[
              !one$ok | !is.finite(one$p)
            ] <- NA_real_
            
            one$reject <- one$p_adjusted < settings$alpha
            
            rows[[i]] <- one
            
            if (i == 1L ||
                i %% max(1L, length(datasets) %/% 100L) == 0L ||
                i == length(datasets)) {
              setProgress(
                value = i / length(datasets),
                detail = paste(i, "of", length(datasets))
              )
            }
          }
        }
      )
      
      details <- do.call(
        rbind,
        rows
      )
      
      list(
        analyzed_at = Sys.time(),
        dataset_generated_at = run$bundle$generated_at,
        dataset_seed = run$bundle$seed,
        dataset_count = length(datasets),
        settings = settings,
        
        summary = summarize_direct(
          details,
          comparisons
        ),
        
        details = details,
        session_info = sessionInfo()
      )
    })
    
    if (!result$ok) {
      showNotification(
        result$message,
        type = "error",
        duration = NULL
      )
      
      return()
    }
    
    # Retain analysis even if saving fails.
    analysis_run(result$value)
    
    save_result <- safe_state({
      run <- dataset_run()
      
      # Retry collection saving if generation-time saving failed.
      if (is.null(run$directory)) {
        run$directory <- new_run_directory(
          output_root(),
          prefix = "run"
        )
        
        saveRDS(
          run$bundle,
          file.path(run$directory, "datasets.rds")
        )
        
        dataset_run(run)
        
        dataset_message(paste0(
          length(run$bundle$datasets),
          " datasets in memory.\nSaved to:\n",
          run$directory,
          "/datasets.rds"
        ))
      }
      
      directory <- new_run_directory(
        file.path(run$directory, "analyses"),
        prefix = "direct"
      )
      
      saveRDS(
        result$value,
        file.path(directory, "analysis.rds")
      )
      
      write.csv(
        result$value$summary,
        file.path(directory, "summary.csv"),
        row.names = FALSE
      )
      
      directory
    })
    
    if (save_result$ok) {
      analysis_message(paste0(
        "Analysis complete.\nSaved to:\n",
        save_result$value
      ))
      
    } else {
      analysis_message(paste0(
        "Analysis complete and retained in memory.\n",
        "Disk saving failed: ",
        save_result$message
      ))
    }
  })
  
  
  # ==========================================================================
  # 22 | DISPLAY: STATUS, DATA PREVIEW AND RESULTS
  # ==========================================================================
  
  output$dataset_status <- renderText({
    dataset_message()
  })
  
  output$analysis_status <- renderText({
    analysis_message()
  })
  
  output$dataset_preview <- renderTable({
    run <- dataset_run()
    req(run)
    
    i <- input$preview_dataset
    
    validate(
      need(
        length(i) == 1L &&
          is.finite(i) &&
          i == floor(i) &&
          i >= 1 &&
          i <= length(run$bundle$datasets),
        
        "Select an existing dataset number."
      )
    )
    
    head(
      run$bundle$datasets[[as.integer(i)]],
      20
    )
  }, digits = 4)
  
  output$direct_summary <- renderTable({
    result <- analysis_run()
    req(result)
    
    result$summary
  }, digits = 4)
  
  output$direct_details <- renderTable({
    result <- analysis_run()
    req(result)
    
    head(
      result$details,
      100
    )
  }, digits = 4)
  
  output$saved_settings <- renderPrint({
    run <- dataset_run()
    req(run)
    
    cat("DATASET GENERATION SETTINGS\n\n")
    
    str(list(
      generated_at = run$bundle$generated_at,
      seed = run$bundle$seed,
      repetitions = length(run$bundle$datasets),
      directory = run$directory,
      truth = run$bundle$truth,
      design = run$bundle$design
    ))
    
    result <- analysis_run()
    
    if (!is.null(result)) {
      cat("\n\nLAST COMPLETED ANALYSIS SETTINGS\n\n")
      str(result$settings)
    }
  })
  
  # ==========================================================================
  # 23 | DISPLAY: SHARED TRUE-SURVIVAL AND KAPLAN–MEIER PLOTS
  # Uses only the saved collection, not current Truth/Design edits.
  # ==========================================================================
  
  observeEvent(dataset_run(), {
    run <- dataset_run()
    req(run)
    
    updateNumericInput(
      session,
      "plot_dataset",
      value = 1,
      min = 1,
      max = length(run$bundle$datasets)
    )
  })
  
  
  output$shared_survival_plot <- plotly::renderPlotly({
    run <- dataset_run()
    
    validate(
      need(
        !is.null(run),
        "Generate or load datasets first."
      )
    )
    
    bundle <- run$bundle
    truth <- bundle$truth
    design <- bundle$design
    
    dataset_number <- input$plot_dataset
    
    validate(
      need(
        length(dataset_number) == 1L &&
          is.finite(dataset_number) &&
          dataset_number == floor(dataset_number) &&
          dataset_number >= 1 &&
          dataset_number <= length(bundle$datasets),
        
        "Select an existing dataset number."
      )
    )
    
    data <- bundle$datasets[[as.integer(dataset_number)]]
    
    cutoff <- design$administrative_censoring$cutoff
    n_arms <- length(design$arms)
    
    # Fixed color for each saved arm across both panels.
    colors <- grDevices::hcl.colors(
      n_arms,
      palette = "Dark 3"
    )
    
    # Include the transition exactly in the truth plotting grid.
    grid <- seq(
      0,
      cutoff,
      length.out = 501
    )
    
    if (length(design$stages) == 2L) {
      grid <- sort(unique(c(
        grid,
        design$transition_time
      )))
    }
    
    truth_plot <- plotly::plot_ly()
    km_plot <- plotly::plot_ly()
    
    for (a in seq_len(n_arms)) {
      label <- design$arms[[a]]$name
      group <- paste0("arm_", a)
      color <- colors[a]
      
      # ----------------------------------------------------------------------
      # True survival: population curve, unaffected by censoring
      # ----------------------------------------------------------------------
      
      true_survival <- true_arm_survival(
        time = grid,
        truth = truth,
        design = design,
        arm_index = a
      )
      
      truth_plot <- plotly::add_trace(
        truth_plot,
        
        x = grid,
        y = true_survival,
        
        type = "scatter",
        mode = "lines",
        name = label,
        legendgroup = group,
        showlegend = TRUE,
        
        line = list(
          color = color,
          width = 2
        ),
        
        text = rep(label, length(grid)),
        
        hovertemplate = paste0(
          "%{text}",
          "<br>Time: %{x:.3f}",
          "<br>True survival: %{y:.4f}",
          "<extra></extra>"
        )
      )
      
      # ----------------------------------------------------------------------
      # KM: estimate from one simulated dataset
      # ----------------------------------------------------------------------
      
      arm_data <- data[
        data$arm == a,
        ,
        drop = FALSE
      ]
      
      if (!nrow(arm_data)) {
        next
      }
      
      fit <- survival::survfit(
        survival::Surv(time, event) ~ 1,
        data = arm_data,
        conf.type = "none"
      )
      
      km_time <- c(0, fit$time)
      km_survival <- c(1, as.numeric(fit$surv))
      
      km_plot <- plotly::add_trace(
        km_plot,
        
        x = km_time,
        y = km_survival,
        
        type = "scatter",
        mode = "lines",
        name = label,
        legendgroup = group,
        showlegend = FALSE,
        
        line = list(
          color = color,
          width = 2,
          shape = "hv"
        ),
        
        text = rep(label, length(km_time)),
        
        hovertemplate = paste0(
          "%{text}",
          "<br>Time: %{x:.3f}",
          "<br>KM survival: %{y:.4f}",
          "<extra></extra>"
        )
      )
      
      # Censoring marks, linked to the same arm legend group.
      censored <- which(fit$n.censor > 0)
      
      if (length(censored)) {
        km_plot <- plotly::add_trace(
          km_plot,
          
          x = fit$time[censored],
          y = as.numeric(fit$surv)[censored],
          
          type = "scatter",
          mode = "markers",
          name = label,
          legendgroup = group,
          showlegend = FALSE,
          
          marker = list(
            color = color,
            symbol = "line-ns-open",
            size = 8
          ),
          
          text = rep(label, length(censored)),
          
          hovertemplate = paste0(
            "%{text}",
            "<br>Censoring time: %{x:.3f}",
            "<br>KM survival: %{y:.4f}",
            "<extra></extra>"
          )
        )
      }
    }
    
    # ------------------------------------------------------------------------
    # Matching axes
    # ------------------------------------------------------------------------
    
    truth_plot <- plotly::layout(
      truth_plot,
      
      xaxis = list(
        title = "Time",
        range = c(0, cutoff)
      ),
      
      yaxis = list(
        title = "True survival",
        range = c(0, 1.02)
      )
    )
    
    km_plot <- plotly::layout(
      km_plot,
      
      xaxis = list(
        title = "Time",
        range = c(0, cutoff)
      ),
      
      yaxis = list(
        title = "Kaplan–Meier survival",
        range = c(0, 1.02)
      )
    )
    
    figure <- plotly::subplot(
      truth_plot,
      km_plot,
      
      nrows = 2,
      shareX = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.07
    )
    
    # ------------------------------------------------------------------------
    # Stage-transition markers in both panels
    # ------------------------------------------------------------------------
    
    shapes <- list()
    
    if (length(design$stages) == 2L) {
      tau <- design$transition_time
      
      shapes <- list(
        list(
          type = "line",
          xref = "x",
          yref = "y",
          x0 = tau,
          x1 = tau,
          y0 = 0,
          y1 = 1,
          
          line = list(
            color = "#808080",
            dash = "dash",
            width = 1
          )
        ),
        
        list(
          type = "line",
          xref = "x2",
          yref = "y2",
          x0 = tau,
          x1 = tau,
          y0 = 0,
          y1 = 1,
          
          line = list(
            color = "#808080",
            dash = "dash",
            width = 1
          )
        )
      )
      
      # With shareX=TRUE, both panels use the same x axis.
      shapes[[2]]$xref <- "x"
    }
    
    figure <- plotly::layout(
      figure,
      
      title = list(
        text = paste0(
          "True survival and observed KM",
          " — dataset ",
          dataset_number
        )
      ),
      
      legend = list(
        title = list(text = "Trial arms"),
        groupclick = "togglegroup"
      ),
      
      shapes = shapes,
      
      margin = list(
        t = 70,
        b = 60,
        l = 80,
        r = 30
      )
    )
    
    plotly::config(
      figure,
      displaylogo = FALSE
    )
  })
  
  # ==========================================================================
  # 24 | LOK07: SAVED-DESIGN COMPARISON OPTIONS
  # ==========================================================================
  
  lok07_result <- reactiveVal(NULL)
  
  lok07_message <- reactiveVal(
    "No Lok07 analysis run."
  )
  
  lok07_options <- reactive({
    run <- dataset_run()
    
    if (is.null(run)) return(NULL)
    
    lok07_candidates(
      run$bundle$design
    )
  })
  
  # Discard displayed results when the dataset collection changes.
  observeEvent(dataset_run(), {
    lok07_result(NULL)
    
    lok07_message(
      "No Lok07 analysis run for this collection."
    )
  })
  
  output$lok07_comparison_controls <- renderUI({
    options <- lok07_options()
    
    if (is.null(options) || nrow(options) == 0L) {
      return(
        tagList(
          selectInput(
            "lok07_choice",
            "Viable comparison",
            choices = c("None" = "")
          ),
          
          helpText(
            "No supported comparison in the saved design.",
            "Requires two different initial treatments and",
            "two-option re-randomization with a common target",
            "maintenance probability in both selected arms.",
            "Eligibility must be all eligible patients in both arms,",
            "or responders only in both arms."
          )
        )
      )
    }
    
    choices <- setNames(
      as.character(seq_len(nrow(options))),
      options$label
    )
    
    selectInput(
      "lok07_choice",
      "Viable comparison",
      choices = choices,
      selected = "1"
    )
  })
  
  selected_lok07 <- reactive({
    options <- lok07_options()
    choice <- suppressWarnings(
      as.integer(input$lok07_choice)
    )
    
    if (is.null(options) ||
        nrow(options) == 0L ||
        length(choice) != 1L ||
        is.na(choice) ||
        choice < 1L ||
        choice > nrow(options)) {
      return(NULL)
    }
    
    as.list(
      options[choice, , drop = FALSE]
    )
  })
  
  output$lok07_probability <- renderText({
    specification <- selected_lok07()
    
    if (is.null(specification)) {
      return("No viable comparison selected.")
    }
    
    paste0(
      "Known target-maintenance probability π = ",
      format(specification$pi, digits = 4),
      "\nTarget-maintenance weight = ",
      format(1 / specification$pi, digits = 4),
      "\nOther randomized maintenance: weight 0",
      "\nNot re-randomized: weight 1"
    )
  })
  
  
  # ==========================================================================
  # 25 | LOK07: RUN ON THE EXISTING DATASET COLLECTION
  # ==========================================================================
  
  observeEvent(input$run_lok07, {
    result <- safe_state({
      run <- dataset_run()
      
      if (is.null(run)) {
        stop("Generate or load datasets first.")
      }
      
      specification <- selected_lok07()
      
      if (is.null(specification)) {
        stop("No viable Lok07 comparison is available.")
      }
      
      alpha <- read_number(
        "lok07_alpha",
        "Lok07 significance level"
      )
      
      if (alpha <= 0 || alpha >= 1) {
        stop("Significance level must be between 0 and 1.")
      }
      
      settings <- list(
        alpha = alpha,
        sided = input$lok07_sided,
        probability_source = "known_design",
        weights = "time_independent",
        test = "robust_pseudo_score",
        variance = "sandwich",
        ties = "breslow"
      )
      
      datasets <- run$bundle$datasets
      rows <- vector("list", length(datasets))
      
      withProgress(
        message = "Running Lok07",
        value = 0,
        
        {
          for (i in seq_along(datasets)) {
            one <- analyze_lok07(
              datasets[[i]],
              specification,
              settings
            )
            
            one$dataset_id <- i
            one$comparison <- specification$label
            one$alpha <- alpha
            one$reject <- ifelse(
              one$ok & is.finite(one$p),
              one$p < alpha,
              NA
            )
            
            rows[[i]] <- one
            
            if (i == 1L ||
                i %% max(1L, length(datasets) %/% 100L) == 0L ||
                i == length(datasets)) {
              setProgress(
                value = i / length(datasets),
                detail = paste(i, "of", length(datasets))
              )
            }
          }
        }
      )
      
      details <- do.call(
        rbind,
        rows
      )
      
      valid <- details$ok & is.finite(details$p)
      completed <- sum(valid)
      
      average <- function(x) {
        x <- x[is.finite(x)]
        
        if (length(x)) {
          mean(x)
        } else {
          NA_real_
        }
      }
      
      rate <- if (completed) {
        mean(details$reject[valid])
      } else {
        NA_real_
      }
      
      summary <- data.frame(
        comparison = specification$label,
        repetitions = nrow(details),
        valid_fits = completed,
        failed_fits = sum(!valid),
        
        mean_positive_weight_events = average(
          details$events_used
        ),
        
        mean_log_hr = average(
          details$estimate[valid]
        ),
        
        empirical_sd = if (completed > 1L) {
          sd(details$estimate[valid])
        } else {
          NA_real_
        },
        
        mean_se = average(
          details$se[valid]
        ),
        
        rejection_rate = rate,
        
        monte_carlo_se = if (completed) {
          sqrt(rate * (1 - rate) / completed)
        } else {
          NA_real_
        },
        
        stringsAsFactors = FALSE
      )
      
      list(
        analyzed_at = Sys.time(),
        method = "Lokhnygina_Helterbrand_2007",
        dataset_generated_at = run$bundle$generated_at,
        dataset_seed = run$bundle$seed,
        comparison = specification,
        settings = settings,
        summary = summary,
        details = details,
        session_info = sessionInfo()
      )
    })
    
    if (!result$ok) {
      showNotification(
        result$message,
        type = "error",
        duration = NULL
      )
      
      return()
    }
    
    lok07_result(result$value)
    
    
    # ========================================================================
    # 26 | LOK07: SAVE ANALYSIS UNDER THE DATASET-RUN DIRECTORY
    # ========================================================================
    
    save_result <- safe_state({
      run <- dataset_run()
      
      if (is.null(run$directory)) {
        run$directory <- new_run_directory(
          output_root(),
          prefix = "run"
        )
        
        saveRDS(
          run$bundle,
          file.path(
            run$directory,
            "datasets.rds"
          )
        )
        
        dataset_run(run)
      }
      
      directory <- new_run_directory(
        file.path(
          run$directory,
          "analyses"
        ),
        prefix = "lok07"
      )
      
      saveRDS(
        result$value,
        file.path(
          directory,
          "analysis.rds"
        )
      )
      
      write.csv(
        result$value$summary,
        file.path(
          directory,
          "summary.csv"
        ),
        row.names = FALSE
      )
      
      directory
    })
    
    # Updating dataset_run during a save retry can invalidate displayed results;
    # restore this completed analysis explicitly.
    lok07_result(result$value)
    
    if (save_result$ok) {
      lok07_message(paste0(
        "Lok07 analysis complete.\nSaved to:\n",
        save_result$value
      ))
      
    } else {
      lok07_message(paste0(
        "Analysis retained in memory.\nSaving failed: ",
        save_result$message
      ))
    }
  })
  
  
  # ==========================================================================
  # 27 | DISPLAY: LOK07 RESULTS
  # ==========================================================================
  
  output$lok07_status <- renderText({
    lok07_message()
  })
  
  output$lok07_summary <- renderTable({
    result <- lok07_result()
    req(result)
    
    result$summary
  }, digits = 4)
  
  output$lok07_details <- renderTable({
    result <- lok07_result()
    req(result)
    
    head(
      result$details,
      100
    )
  }, digits = 4)
  
}

server
