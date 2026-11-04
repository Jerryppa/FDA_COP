# ============================================================================
# 01 | PACKAGES
# Install once:
# install.packages(c("shiny", "DiagrammeR"))
# ============================================================================

library(shiny)
library(DiagrammeR)

`%||%` <- function(x, fallback) {
  if (is.null(x)) fallback else x
}


# ============================================================================
# 02 | INPUT UTILITIES
# ============================================================================

parse_ratio <- function(text, n) {
  weights <- suppressWarnings(
    as.numeric(trimws(strsplit(text, ":", fixed = TRUE)[[1]]))
  )
  
  if (length(weights) != n ||
      any(!is.finite(weights)) ||
      any(weights <= 0)) {
    stop(sprintf(
      "Enter %d positive allocation weights, separated by colons.",
      n
    ))
  }
  
  weights / sum(weights)
}


parse_treatments <- function(text, n) {
  treatments <- trimws(
    strsplit(text, ",", fixed = TRUE)[[1]]
  )
  
  if (length(treatments) != n ||
      any(!nzchar(treatments)) ||
      anyDuplicated(treatments)) {
    stop(sprintf(
      "Enter %d distinct treatment names, separated by commas.",
      n
    ))
  }
  
  treatments
}


# ============================================================================
# 03 | REUSABLE UI: SECOND-STAGE BRANCH
# prefix gives every branch its own unique input IDs.
# ============================================================================

branch_ui <- function(prefix, title) {
  tagList(
    h5(title),
    
    selectInput(
      paste0(prefix, "action"),
      "Assignment",
      choices = c(
        "Specified treatment" = "fixed",
        "Randomize" = "random"
      ),
      selected = "random"
    ),
    
    conditionalPanel(
      condition = sprintf(
        "input['%saction'] === 'fixed'", prefix
      ),
      
      textInput(
        paste0(prefix, "fixed"),
        "Treatment / assigned sequence",
        value = "Continue treatment"
      )
    ),
    
    conditionalPanel(
      condition = sprintf(
        "input['%saction'] === 'random'", prefix
      ),
      
      selectInput(
        paste0(prefix, "n"),
        "Number of treatment options",
        choices = c(2, 3),
        selected = 2
      ),
      
      textInput(
        paste0(prefix, "names"),
        "Treatment names, separated by commas",
        value = "B1, B2"
      ),
      
      textInput(
        paste0(prefix, "ratio"),
        "Allocation weights",
        value = "1:1"
      ),
      
      helpText(
        "For three options, enter three names and three weights."
      )
    )
  )
}


# ============================================================================
# 04 | FLOWCHART FUNCTION
# Ordinary R function: design object -> Graphviz DOT text.
# No Shiny inputs or reactive expressions inside this function.
# ============================================================================

dot_quote <- function(text) {
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub('"', '\\"', text, fixed = TRUE)
  text <- gsub("\n", "\\n", text, fixed = TRUE)
  
  paste0('"', text, '"')
}


design_to_dot <- function(design) {
  lines <- c(
    "digraph trial {",
    'graph [
      rankdir = TB,
      bgcolor = "transparent",
      nodesep = 0.35,
      ranksep = 0.55
    ];',
    'node [
      shape = box,
      style = "rounded,filled",
      fillcolor = "#EAF2FB",
      color = "#2463A0",
      fontname = "Arial",
      fontsize = 12
    ];',
    'edge [
      color = "#62748A",
      fontname = "Arial",
      fontsize = 10
    ];'
  )
  
  add_node <- function(id, label, shape = "box") {
    lines <<- c(
      lines,
      sprintf(
        "%s [label=%s, shape=%s];",
        id, dot_quote(label), shape
      )
    )
  }
  
  add_edge <- function(from, to, label = "") {
    lines <<- c(
      lines,
      sprintf(
        "%s -> %s [label=%s];",
        from, to, dot_quote(label)
      )
    )
  }
  
  percent <- function(p) {
    paste0(format(round(100 * p, 1), trim = TRUE), "%")
  }
  
  add_node("initial", "Initial randomization", "diamond")
  
  for (i in seq_along(design$arms)) {
    arm <- design$arms[[i]]
    arm_id <- paste0("arm", i)
    
    add_node(arm_id, arm$name)
    
    add_edge(
      "initial",
      arm_id,
      percent(design$allocation[i])
    )
    
    if (!design$second_stage) {
      end_id <- paste0(arm_id, "_end")
      
      add_node(end_id, "Continue assigned sequence")
      add_edge(arm_id, end_id)
      
      next
    }
    
    origin <- arm_id
    
    if (arm$response_based) {
      origin <- paste0(arm_id, "_assessment")
      
      add_node(origin, "Response assessment", "diamond")
      add_edge(arm_id, origin)
    }
    
    for (j in seq_along(arm$branches)) {
      branch <- arm$branches[[j]]
      branch_id <- paste0(arm_id, "_branch", j)
      
      branch_label <- if (arm$response_based) {
        branch$name
      } else {
        ""
      }
      
      if (branch$action == "fixed") {
        add_node(branch_id, branch$treatments[1])
        add_edge(origin, branch_id, branch_label)
        
      } else {
        add_node(branch_id, "Re-randomize", "diamond")
        add_edge(origin, branch_id, branch_label)
        
        for (k in seq_along(branch$treatments)) {
          treatment_id <- paste0(branch_id, "_t", k)
          
          add_node(
            treatment_id,
            branch$treatments[k]
          )
          
          add_edge(
            branch_id,
            treatment_id,
            percent(branch$allocation[k])
          )
        }
      }
    }
  }
  
  paste(c(lines, "}"), collapse = "\n")
}