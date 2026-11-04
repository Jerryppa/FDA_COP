# ============================================================================
# 01 | MAIN UI
# ============================================================================

ui <- navbarPage(
  title = "Trial Simulator",
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

      .chart-panel {
        position: sticky;
        top: 15px;
      }
    "))
  ),
  
  
  # ==========================================================================
  # 02 | DESIGN TAB
  # ==========================================================================
  
  tabPanel(
    "Design",
    
    fluidRow(
      
      # ----------------------------------------------------------------------
      # LEFT: parameter sections
      # ----------------------------------------------------------------------
      
      column(
        width = 4,
        
        div(
          class = "well",
          
          tags$details(
            open = NA,
            
            tags$summary("Trial design"),
            br(),
            
            selectInput(
              "n_arms",
              "Number of initial arms",
              choices = c(2, 3),
              selected = 2
            ),
            
            uiOutput("initial_controls"),
            
            checkboxInput(
              "second_stage",
              "Include a second assignment stage",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.second_stage",
              uiOutput("stage2_controls")
            )
          ),
          
          # INSERT: outcome assumptions controls
          tags$details(
            tags$summary("Outcome assumptions"),
            p("To be added: hazards and response probabilities.")
          ),
          
          # INSERT: timing, enrollment and sample-size controls
          tags$details(
            tags$summary("Study timing and sample size"),
            p("To be added: timing, enrollment and follow-up.")
          )
        )
      ),
      
      
      # ----------------------------------------------------------------------
      # RIGHT: reactive flowchart
      # ----------------------------------------------------------------------
      
      column(
        width = 8,
        
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
  # 03 | ANALYZE TAB
  # ==========================================================================
  
  tabPanel(
    "Analyze",
    
    h3("Analysis"),
    
    # INSERT: comparisons, methods, simulation controls and results
    p("Analysis controls will be added here.")
  )
)

ui