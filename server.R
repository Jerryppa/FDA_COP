server <- function(input, output, session) {
  
  # ==========================================================================
  # 01 | INITIAL-ARM CONTROLS
  # ==========================================================================
  
  output$initial_controls <- renderUI({
    n <- as.integer(input$n_arms)
    
    tagList(
      lapply(seq_len(n), function(i) {
        id <- paste0("arm_name_", i)
        
        textInput(
          id,
          label = paste("Initial arm", i),
          value = isolate(input[[id]]) %||% paste0("A", i)
        )
      }),
      
      textInput(
        "initial_ratio",
        "Initial allocation weights",
        value = paste(rep(1, n), collapse = ":")
      )
    )
  })
  
  
  # ==========================================================================
  # 02 | SECOND-STAGE CONTROLS
  # Different rules can be specified for each initial arm.
  # ==========================================================================
  
  output$stage2_controls <- renderUI({
    n <- as.integer(input$n_arms)
    
    lapply(seq_len(n), function(i) {
      prefix <- paste0("s", i, "_")
      
      tags$details(
        open = NA,
        
        tags$summary(
          paste("Second stage: initial arm", i)
        ),
        
        selectInput(
          paste0(prefix, "rule"),
          "Branching rule",
          choices = c(
            "Everyone reaching stage 2" = "all",
            "Based on response" = "response"
          )
        ),
        
        conditionalPanel(
          condition = sprintf(
            "input['%srule'] === 'all'", prefix
          ),
          
          branch_ui(
            paste0(prefix, "all_"),
            "All eligible patients"
          )
        ),
        
        conditionalPanel(
          condition = sprintf(
            "input['%srule'] === 'response'", prefix
          ),
          
          branch_ui(
            paste0(prefix, "r1_"),
            "Responders (R=1)"
          ),
          
          branch_ui(
            paste0(prefix, "r0_"),
            "Nonresponders (R=0)"
          )
        )
      )
    })
  })
  
  
  # ==========================================================================
  # 03 | READ ONE SECOND-STAGE BRANCH
  # ==========================================================================
  
  read_branch <- function(prefix, name) {
    action <- input[[paste0(prefix, "action")]]
    
    if (is.null(action)) {
      stop("Waiting for second-stage controls.")
    }
    
    if (action == "fixed") {
      treatment <- trimws(
        input[[paste0(prefix, "fixed")]] %||% ""
      )
      
      if (!nzchar(treatment)) {
        stop("Enter a treatment for each fixed assignment.")
      }
      
      return(list(
        name = name,
        action = "fixed",
        treatments = treatment,
        allocation = 1
      ))
    }
    
    n <- as.integer(input[[paste0(prefix, "n")]])
    
    if (length(n) != 1L || is.na(n)) {
      stop("Select the number of second-stage treatments.")
    }
    
    list(
      name = name,
      action = "random",
      
      treatments = parse_treatments(
        input[[paste0(prefix, "names")]] %||% "",
        n
      ),
      
      allocation = parse_ratio(
        input[[paste0(prefix, "ratio")]] %||% "",
        n
      )
    )
  }
  
  
  # ==========================================================================
  # 04 | BUILD THE SHARED DESIGN OBJECT
  # Future simulation code will use this same object.
  # ==========================================================================
  
  design_state <- reactive({
    tryCatch({
      
      n <- as.integer(input$n_arms)
      
      allocation <- parse_ratio(
        input$initial_ratio %||% "",
        n
      )
      
      arm_names <- vapply(
        seq_len(n),
        
        function(i) {
          trimws(
            input[[paste0("arm_name_", i)]] %||% ""
          )
        },
        
        character(1)
      )
      
      if (any(!nzchar(arm_names)) ||
          anyDuplicated(arm_names)) {
        stop("Initial arm names must be nonempty and distinct.")
      }
      
      arms <- lapply(seq_len(n), function(i) {
        prefix <- paste0("s", i, "_")
        
        response_based <- identical(
          input[[paste0(prefix, "rule")]],
          "response"
        )
        
        branches <- list()
        
        if (isTRUE(input$second_stage)) {
          if (response_based) {
            branches <- list(
              read_branch(
                paste0(prefix, "r1_"),
                "Responder"
              ),
              
              read_branch(
                paste0(prefix, "r0_"),
                "Nonresponder"
              )
            )
            
          } else {
            branches <- list(
              read_branch(
                paste0(prefix, "all_"),
                "All eligible"
              )
            )
          }
        }
        
        list(
          name = arm_names[i],
          response_based = response_based,
          branches = branches
        )
      })
      
      list(
        ok = TRUE,
        
        design = list(
          allocation = allocation,
          second_stage = isTRUE(input$second_stage),
          arms = arms
        )
      )
      
    }, error = function(e) {
      
      list(
        ok = FALSE,
        message = conditionMessage(e)
      )
    })
  })
  
  
  # ==========================================================================
  # 05 | FLOWCHART AND VALIDATION OUTPUTS
  # ==========================================================================
  
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
        "Complete the design fields to display the flowchart."
      )
    )
    
    grViz(
      design_to_dot(state$design)
    )
  })
  
  
  output$design_object <- renderPrint({
    state <- design_state()
    
    if (state$ok) {
      str(state$design)
    } else {
      cat(state$message)
    }
  })
  
  
  # ==========================================================================
  # 06 | INSERT: DATA GENERATION
  # ==========================================================================
  
  
  # ==========================================================================
  # 07 | INSERT: ANALYSIS AND SIMULATION RESULTS
  # ==========================================================================
  
}

server