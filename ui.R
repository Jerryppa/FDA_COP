# ============================================================================
# 01 | MAIN UI AND STYLE
# ============================================================================

ui <- navbarPage(
  title = "FDA_COP",
  id = "main_tab",
  
  header = tags$head(
    tags$style(HTML("
      .navbar-default {
        background: #0057ad;
        border: 0;
        border-radius: 0;
      }

      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white;
      }

      .navbar-default .navbar-nav > .active > a {
        background: #003f80;
        color: white;
      }

      .well {
        background: #f7f9fc;
      }

      details {
        border-bottom: 1px solid #d8e1eb;
        padding: 12px 0;
      }

      summary {
        font-weight: 600;
        cursor: pointer;
        color: #174a7e;
        font-size: 16px;
      }

      summary, h3, h4, h5 {
        background: #e7f0fb; border-left: 4px solid #337ab7;
        padding: 10px 12px; border-radius: 4px; color: #174a7e;
      }
      details { margin-bottom: 10px; }
      .nav-tabs { margin-bottom: 16px; }
      .chart-panel {
        position: sticky;
        top: 15px;
      }
    "))
  ),
  
  
  # ==========================================================================
  # 02 | TRUTH TAB
  # ==========================================================================
  
  tabPanel(
    "Truth",
    
    fluidRow(
      column(
        4,
        
        div(
          class = "well",
          
          tags$details(
            open = NA,
            tags$summary("Treatments and stages"),
            br(),
            
            textInput(
              "drug_names",
              "Treatment names, separated by semicolons",
              "Drug; SoC"
            ),
            
            textInput(
              "stage_names",
              "Stage names, separated by semicolons",
              "Induction; Maintenance"
            ),
            
            actionButton(
              "apply_definitions",
              "Apply definitions"
            ),
            
            helpText(
              "Any treatment can be used at either stage.",
              "This version supports one or two stages."
            ),
            
            helpText(
              "Renaming a treatment creates a new treatment definition.",
              "Its previous parameter values are not transferred automatically."
            )
          ),
          
          tags$details(
            open = NA,
            tags$summary("Response"),
            br(),
            
            checkboxInput(
              "response_enabled",
              "Model binary response after stage 1",
              TRUE
            ),
            
            uiOutput("response_controls")
          ),
          
          # --------------------------------------------------------------------------
          # TRUTH | RANDOM CENSORING
          # --------------------------------------------------------------------------
          
          tags$details(
            tags$summary("Random censoring"),
            br(),
            
            tabsetPanel(
              id = "censoring_tab",
              
              tabPanel(
                "Independent",
                
                selectInput(
                  "censor_distribution",
                  "Random censoring distribution",
                  choices = c(
                    "None" = "none",
                    "Exponential" = "exponential",
                    "Weibull" = "weibull",
                    "Uniform" = "uniform"
                  ),
                  selected = "none"
                ),
                
                helpText(
                  "Censoring time is measured from initial randomization.",
                  "It is generated independently of treatment, response and event time.",
                  "Administrative censoring still applies."
                ),
                
                conditionalPanel(
                  "input.censor_distribution === 'exponential'",
                  
                  numericInput(
                    "censor_rate",
                    "Exponential rate",
                    value = 0.05,
                    min = 0.000001,
                    step = 0.01
                  )
                ),
                
                conditionalPanel(
                  "input.censor_distribution === 'weibull'",
                  
                  numericInput(
                    "censor_shape",
                    "Weibull shape",
                    value = 1,
                    min = 0.000001,
                    step = 0.1
                  ),
                  
                  numericInput(
                    "censor_scale",
                    "Weibull scale (time units)",
                    value = 10,
                    min = 0.000001,
                    step = 0.5
                  )
                ),
                
                conditionalPanel(
                  "input.censor_distribution === 'uniform'",
                  
                  numericInput(
                    "censor_min",
                    "Uniform minimum",
                    value = 0,
                    min = 0,
                    step = 0.1
                  ),
                  
                  numericInput(
                    "censor_max",
                    "Uniform maximum",
                    value = 10,
                    min = 0.01,
                    step = 0.5
                  )
                )
              ),
              
              tabPanel(
                "Informative — later",
                
                p("Reserved for informative censoring models."),
                
                helpText(
                  "This tab does not enable informative censoring.",
                  "The Independent tab settings remain active."
                )
              )
            )
          ),
          
          tags$details(
            tags$summary("Covariates"),
            p("Reserved for a later addition.")
          )
        )
      ),
      
      column(
        8,
        
        h3("Survival assumptions"),
        
        helpText(
          "Exponential within each stage.",
          "With two stages, survival is piecewise exponential.",
          "All hazards use the same time unit."
        ),
        
        uiOutput("truth_status"),
        uiOutput("hazard_controls"),
        
        tags$details(
          tags$summary("Inspect truth object"),
          verbatimTextOutput("truth_object")
        )
      )
    )
  ),
  
  
  # ==========================================================================
  # 03 | DESIGN TAB
  # ==========================================================================
  
  tabPanel(
    "Design",
    
    fluidRow(
      column(
        4,
        
        div(
          class = "well",
          
          tags$details(
            open = NA,
            tags$summary("Trial design"),
            br(),
            
            numericInput(
              "n_arms",
              "Number of initial arms",
              value = 3,
              min = 2,
              step = 1
            ),
            
            uiOutput("initial_controls"),
            uiOutput("stage2_controls")
          ),
          
          tags$details(
            open = NA,
            tags$summary("Stage timing"),
            br(),
            
            uiOutput("timing_controls")
          ),
          
          tags$details(
            open = NA,
            tags$summary("Treatment continuation decisions"),
            br(),
            helpText("Stopping treatment is not loss to follow-up. OS continues until death or ordinary censoring."),
            tabsetPanel(
              tabPanel("Induction → consolidation", uiOutput("continuation1_controls")),
              tabPanel("Consolidation → maintenance", uiOutput("continuation2_controls"))
            )
          ),

          # --------------------------------------------------------------------------
          # DESIGN | INITIAL SAMPLE SIZE AND ADMINISTRATIVE CENSORING
          # --------------------------------------------------------------------------
          
          tags$details(
            open = NA,
            tags$summary("Sample size and follow-up"),
            br(),
            
            numericInput(
              "initial_n",
              "Initial sample size",
              value = 500,
              min = 2,
              step = 1
            ),
            
            numericInput(
              "admin_cutoff",
              "Maximum follow-up from initial randomization",
              value = 5,
              min = 0.01,
              step = 0.1
            ),
            
            helpText(
              "One initial cohort; no additional recruitment.",
              "Everyone enters at time 0 and has the same administrative cutoff.",
              "Use the same time unit as stage timing and survival hazards."
            )
          )
        )
      ),
      
      column(
        8,
        
        div(
          class = "chart-panel",
          
          h3("Trial flow"),
          uiOutput("design_status"),
          
          grVizOutput(
            "trial_flow",
            height = "700px"
          ),
          
          tags$details(
            tags$summary("Inspect design object"),
            verbatimTextOutput("design_object")
          )
        )
      )
    )
  ),
  
  
  # ==========================================================================
  # 04 | ANALYZE TAB
  # ==========================================================================
  
  tabPanel(
    "Analyze",
    
    fluidRow(
      
      # ----------------------------------------------------------------------
      # LEFT | Generate and save datasets
      # ----------------------------------------------------------------------
      
      column(
        4,
        
        div(
          class = "well",
          
          h4("Simulate data"),
          
          numericInput(
            "sim_reps",
            "Number of datasets",
            value = 1000,
            min = 1,
            step = 100
          ),
          
          numericInput(
            "sim_seed",
            "Random seed",
            value = 1234,
            min = 0,
            max = .Machine$integer.max,
            step = 1
          ),
          
          textInput(
            "sim_output_root",
            "Output folder",
            value = "simulation_runs"
          ),
          
          helpText(
            "Relative paths are relative to the app's working directory.",
            "Datasets and generation settings are automatically saved",
            "in a timestamped subfolder and kept in memory."
          ),
          
          actionButton(
            "generate_datasets",
            "Generate datasets",
            class = "btn-primary"
          ),
          
          tags$hr(),
          
          h4("Load an earlier collection"),
          
          fileInput(
            "load_datasets",
            "Select a previously saved datasets.rds",
            accept = ".rds"
          ),
          
          helpText(
            "A loaded collection is copied into a new timestamped folder.",
            "Its saved Truth and Design are used for analysis."
          ),
          
          tags$hr(),
          
          verbatimTextOutput("dataset_status")
        )
      ),
      
      
      # ----------------------------------------------------------------------
      # RIGHT | Analyze the collection currently in memory
      # ----------------------------------------------------------------------
      
      column(
        8,
        
        h3("Analyze generated datasets"),
        
        helpText(
          "Analysis uses the saved dataset collection and its arm definitions.",
          "Editing Truth or Design does not change existing datasets.",
          "Generate again to use updated generation settings."
        ),
        
        tabsetPanel(
          id = "method_tab",
          
          tabPanel(
            "Direct arm comparison",
            value = "direct",
            
            br(),
            
            h4("Comparisons"),
            
            uiOutput("direct_comparison_controls"),
            
            actionButton(
              "add_comparison",
              "Add comparison"
            ),
            
            actionButton(
              "remove_comparison",
              "Remove last comparison"
            ),
            
            tags$hr(),
            
            selectInput(
              "direct_test",
              "Analysis",
              choices = c(
                "Cox regression: Wald test" = "cox",
                "Log-rank test" = "logrank"
              )
            ),
            
            conditionalPanel(
              "input.direct_test === 'cox'",
              
              selectInput(
                "direct_variance",
                "Variance estimator",
                c(
                  "Conventional" = "conventional",
                  "Robust sandwich" = "robust"
                )
              ),
              
              selectInput(
                "direct_ties",
                "Handling tied event times",
                c(
                  "Efron" = "efron",
                  "Breslow" = "breslow",
                  "Exact" = "exact"
                )
              )
            ),
            
            numericInput(
              "analysis_alpha",
              "Significance level",
              value = 0.05,
              min = 0.001,
              max = 0.999,
              step = 0.005
            ),
            
            selectInput(
              "analysis_sided",
              "Test direction",
              c(
                "Two-sided" = "two",
                "One-sided: HR < 1" = "less",
                "One-sided: HR > 1" = "greater"
              )
            ),
            
            selectInput(
              "direct_adjustment",
              "Multiplicity adjustment across comparisons",
              c(
                "None" = "none",
                "Holm" = "holm",
                "Bonferroni" = "bonferroni"
              )
            ),
            
            helpText(
              "HR is comparison arm relative to reference arm.",
              "Tests use baseline arm assignment and follow-up from time 0.",
              "Adjustment is applied within each simulated dataset.",
              "Cox confidence intervals are marginal, two-sided intervals."
            ),
            
            actionButton(
              "run_direct_analysis",
              "Run direct analysis",
              class = "btn-primary"
            )
          ),
          
          tabPanel(
            "Phase-specific regression",
            
            br(),
            
            p("Reserved; not implemented yet.")
          ),
          
          tabPanel(
            "IPW (Lokhnygina 2007), Stage I only",
            value = "lok07",
            
            br(),
            
            uiOutput("lok07_comparison_controls"),
            
            helpText(
              "Compares different initial treatments while fixing",
              "the maintenance treatment among patients eligible",
              "for re-randomization.",
              "Nonresponders retain their specified treatment paths."
            ),
            
            verbatimTextOutput("lok07_probability"),
            
            numericInput(
              "lok07_alpha",
              "Significance level",
              value = 0.05,
              min = 0.001,
              max = 0.999,
              step = 0.005
            ),
            
            selectInput(
              "lok07_sided",
              "Test direction",
              choices = c(
                "Two-sided" = "two",
                "One-sided: HR < 1" = "less",
                "One-sided: HR > 1" = "greater"
              )
            ),
            
            helpText(
              "Known randomization probabilities from the saved design.",
              "Time-independent IPW; robust pseudo-score test;",
              "sandwich SE and marginal two-sided Wald CI.",
              "The HR interpretation assumes proportional strategy hazards."
            ),
            
            actionButton(
              "run_lok07",
              "Run Lok07 analysis",
              class = "btn-primary"
            ),
            
            tags$hr(),
            
            verbatimTextOutput("lok07_status"),
            
            h4("Summary"),
            tableOutput("lok07_summary"),
            
            h4("Individual results"),
            helpText(
              "First 100 rows shown; all results are saved in analysis.rds.",
              "P-values use the robust pseudo-score test;",
              "confidence intervals use the sandwich Wald calculation."
            ),
            
            tableOutput("lok07_details")
          )
        ),
        
        tags$hr(),
        
        verbatimTextOutput("analysis_status"),
        
        tabsetPanel(
          id = "results_tab",
          
          # ------------------------------------------------------------------
          # SHARED VISUALIZATION | Independent of analysis method
          # ------------------------------------------------------------------
          
          tabPanel(
            "Survival plots",
            
            br(),
            
            numericInput(
              "plot_dataset",
              "Dataset number for Kaplan–Meier curves",
              value = 1,
              min = 1,
              step = 1
            ),
            
            helpText(
              "Top: true arm survival from saved Truth + Design.",
              "Bottom: Kaplan–Meier estimates from the selected dataset.",
              "Click a legend entry to hide/show an arm in both panels.",
              "Double-click to isolate an arm.",
              "Dashed lines mark maintenance start and, when enabled, the consolidation decision."
            ),
            
            plotly::plotlyOutput(
              "shared_survival_plot",
              height = "800px"
            )
          ),
          
          tabPanel(
            "Summary",
            
            br(),
            
            helpText(
              "Rejection rates use successful analyses only;",
              "failed analyses are reported separately.",
              "Interpret rejection as power only under an appropriate alternative,",
              "or as Type I error under the corresponding null.",
              "A single true HR need not exist under this piecewise model."
            ),
            
            tableOutput("direct_summary")
          ),
          
          tabPanel(
            "Individual results",
            
            br(),
            
            helpText(
              "First 100 rows shown. All rows are saved in analysis.rds."
            ),
            
            tableOutput("direct_details")
          ),
          
          tabPanel(
            "Dataset preview",
            
            br(),
            
            numericInput(
              "preview_dataset",
              "Dataset number",
              value = 1,
              min = 1,
              step = 1
            ),
            
            tableOutput("dataset_preview")
          ),
          
          tabPanel(
            "Saved settings",
            
            br(),
            
            verbatimTextOutput("saved_settings")
          )
        )
      )
    )
  ),

  # ========================================================================
  # 05 | REVIEW TAB: EDITABLE METHOD / QUESTION MATRIX
  # ========================================================================
  tabPanel(
    "Review",
    br(),
    h3("Method suitability review"),
    p("Select Yes, No, or Not sure in each cell. Ratings are editable and reflect methodological fit, not whether the current trial design meets the prerequisites."),
    tags$style(HTML("\n      .review-matrix { width: 100%; border-collapse: collapse; }\n      .review-matrix th, .review-matrix td { padding: 14px; border: 1px solid #dbe3ec; text-align: center; }\n      .review-matrix th:first-child { text-align: left; width: 32%; }\n      .review-status { width: 130px; padding: 7px; border-radius: 6px; font-weight: 600; border: 1px solid #cbd5e1; }\n      .review-status.yes { background: #dcfce7; color: #166534; }\n      .review-status.no { background: #fee2e2; color: #991b1b; }\n      .review-status.unsure { background: #fef3c7; color: #92400e; }\n    ")),
    local({
      cell <- function(id, initial) {
        tags$select(
          id = id,
          class = paste("review-status", initial),
          onchange = "this.className = 'review-status ' + this.value;",
          tags$option(value = "yes", selected = if (initial == "yes") NA else NULL, "Yes"),
          tags$option(value = "no", selected = if (initial == "no") NA else NULL, "No"),
          tags$option(value = "unsure", selected = if (initial == "unsure") NA else NULL, "Not sure")
        )
      }
      tags$table(
        class = "review-matrix",
        tags$thead(tags$tr(
          tags$th("Research question"), tags$th("Different allocation ratios"),
          tags$th("Time-dependent Cox"), tags$th("IPW (Lok07 / related)")
        )),
        tags$tbody(
          tags$tr(tags$th("Stage 1 comparison"), tags$td(cell("review_s1_ratio", "yes")), tags$td(cell("review_s1_cox", "yes")), tags$td(cell("review_s1_ipw", "yes"))),
          tags$tr(tags$th("Stage 2 comparison"), tags$td(cell("review_s2_ratio", "yes")), tags$td(cell("review_s2_cox", "yes")), tags$td(cell("review_s2_ipw", "unsure"))),
          tags$tr(tags$th("Overall arm comparison"), tags$td(cell("review_overall_ratio", "yes")), tags$td(cell("review_overall_cox", "yes")), tags$td(cell("review_overall_ipw", "unsure"))),
          tags$tr(tags$th("Dynamic treatment regime / SMART"), tags$td(cell("review_smart_ratio", "unsure")), tags$td(cell("review_smart_cox", "unsure")), tags$td(cell("review_smart_ipw", "yes")))
        )
      )
    }),
    br(),
    helpText(
      "Different allocation ratios concern design and precision/power, not an effect estimator.",
      "Time-dependent Cox can model stage-specific associations, but causal interpretations depend on design and assumptions.",
      "Lok07 IPW specifically targets an initial-treatment comparison adjusted for re-randomization; broader regime-level IPW requires other specifications."
    )
  )
)

ui
