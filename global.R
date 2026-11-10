# ============================================================================
# 01 | PACKAGES AND BASIC UTILITIES
# ============================================================================

library(shiny)
library(DiagrammeR)

`%||%` <- function(x, fallback) {
  if (is.null(x)) fallback else x
}

parse_names <- function(text) {
  x <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  
  if (!length(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    stop("Enter distinct, nonempty names separated by semicolons.")
  }
  
  x
}

parse_ratio <- function(text, n) {
  x <- suppressWarnings(
    as.numeric(trimws(strsplit(text, ":", fixed = TRUE)[[1]]))
  )
  
  if (length(x) != n || any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf(
      "Enter %d positive allocation weights separated by colons.", n
    ))
  }
  
  x / sum(x)
}

# Treatment-based IDs: reordering names does not change parameter IDs.
drug_key <- function(x) {
  paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(x)))),
        collapse = "")
}

parameter_id <- function(stage, first, current = NULL, response = NULL) {
  paste(
    c("haz", stage, drug_key(first),
      if (!is.null(current)) drug_key(current),
      if (!is.null(response)) paste0("r", response)),
    collapse = "_"
  )
}


# ============================================================================
# 02 | REUSABLE SECOND-STAGE UI
# ============================================================================

branch_ui <- function(prefix, title, drugs, get_saved) {
  selected <- get_saved(paste0(prefix, "treatments"), head(drugs, 2))
  selected <- intersect(selected, drugs)

  # Default paths: Drug -> Drug, Drug -> SoC, SoC -> SoC.
  # This affects only initial control values, never saved user selections.
  arm_index <- suppressWarnings(as.integer(sub("^s([0-9]+)_.*$", "\\1", prefix)))
  default_fixed <- if (!is.na(arm_index) && arm_index >= 2L && length(drugs) >= 2L) {
    drugs[2]
  } else {
    drugs[1]
  }
  
  tagList(
    h5(title),
    
    selectInput(
      paste0(prefix, "action"),
      "Assignment",
      c("Specified treatment" = "fixed", "Randomize" = "random"),
      selected = get_saved(paste0(prefix, "action"), "fixed")
    ),
    
    conditionalPanel(
      sprintf("input['%saction'] === 'fixed'", prefix),
      
      selectInput(
        paste0(prefix, "fixed"),
        "Treatment",
        choices = drugs,
        selected = intersect(
          get_saved(paste0(prefix, "fixed"), default_fixed), drugs
        )
      )
    ),
    
    conditionalPanel(
      sprintf("input['%saction'] === 'random'", prefix),
      
      selectInput(
        paste0(prefix, "treatments"),
        "Treatment options",
        choices = drugs,
        selected = selected,
        multiple = TRUE
      ),
      
      textInput(
        paste0(prefix, "ratio"),
        "Allocation weights, in selected-treatment order",
        get_saved(
          paste0(prefix, "ratio"),
          paste(rep(1, length(selected)), collapse = ":")
        )
      )
    )
  )
}


# ============================================================================
# 03 | FLOWCHART UTILITIES
# ============================================================================

dot_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  paste0('"', x, '"')
}

design_to_dot <- function(design) {
  lines <- c(
    "digraph trial {",
    'graph [rankdir=TB, bgcolor="transparent",
            nodesep=0.35, ranksep=0.55];',
    'node [shape=box, style="rounded,filled",
           fillcolor="#EAF2FB", color="#2463A0",
           fontname="Arial", fontsize=12];',
    'edge [color="#62748A", fontname="Arial", fontsize=10];'
  )
  
  # Colors belong to the initial randomized arm; decisions retain default styling.
  arm_fills <- c("#DBEAFE", "#FFEDD5", "#D1FAE5", "#EDE9FE", "#FCE7F3", "#CFFAFE", "#FEF3C7", "#F1F5F9")
  arm_borders <- c("#2563EB", "#C2410C", "#047857", "#7C3AED", "#BE185D", "#0E7490", "#A16207", "#475569")
  if (length(design$arms) > length(arm_fills)) {
    arm_fills <- grDevices::hcl.colors(length(design$arms), "Pastel 1")
    arm_borders <- grDevices::hcl.colors(length(design$arms), "Dark 3")
  }
  node <- function(id, label, shape = "box", arm_color = NULL) {
    lines <<- c(lines, sprintf(
      "%s [label=%s, shape=%s%s];", id, dot_quote(label), shape,
      if (is.null(arm_color)) "" else sprintf(
        ", fillcolor=%s, color=%s, fontcolor=%s",
        dot_quote(arm_fills[arm_color]), dot_quote(arm_borders[arm_color]), dot_quote("#172B4D")
      )
    ))
  }
  
  edge <- function(from, to, label = "") {
    lines <<- c(lines, sprintf(
      "%s -> %s [label=%s];", from, to, dot_quote(label)
    ))
  }
  
  pct <- function(p) {
    paste0(format(round(100 * p, 1), trim = TRUE), "%")
  }
  
  node("start", "Initial randomization", "diamond")
  
  for (i in seq_along(design$arms)) {
    arm <- design$arms[[i]]
    a <- paste0("arm", i)
    
    node(a, paste0(
      arm$name, "\n", design$stages[1], ": ", arm$treatment
    ), arm_color = i)
    
    edge("start", a, pct(design$allocation[i]))
    
    if (length(design$stages) == 1L) next
    
    pathway <- a
    gates <- design$continuation
    if (!is.null(gates) && isTRUE(gates[[1]]$enabled)) {
      g <- gates[[1]]
      decision <- paste0(a, "_cons_decision")
      cons <- paste0(a, "_consolidation")
      stop_node <- paste0(a, "_stop1")
      node(decision, paste0("Continue to consolidation?\nt = ", g$time), "diamond")
      edge(a, decision)
      node(cons, "Consolidation")
      edge(decision, cons, paste0(g$continue_label, "\n", pct(g$probability[i])))
      node(stop_node, paste0("Stop treatment\n", g$stop_label, "\nContinue OS follow-up"))
      edge(decision, stop_node, pct(1 - g$probability[i]))
      pathway <- cons
    }
    origin <- paste0(a, "_assessment")
    
    node(
      origin,
      paste0(
        "Continue to maintenance?",
        "\nt = ", design$transition_time
      ),
      "diamond"
    )
    
    edge(pathway, origin)
    if (!is.null(gates) && isTRUE(gates[[2]]$enabled)) {
      g <- gates[[2]]
      stop_node <- paste0(a, "_stop2")
      node(stop_node, paste0("Stop treatment\n", g$stop_label, "\nContinue OS follow-up"))
      edge(origin, stop_node, pct(1 - g$probability[i]))
    }
    
    for (j in seq_along(arm$branches)) {
      branch <- arm$branches[[j]]
      b <- paste0(a, "_branch", j)
      
      label <- if (arm$response_based) branch$name else "Eligible and continuing"
      if (!is.null(gates) && isTRUE(gates[[2]]$enabled))
        label <- paste0(gates[[2]]$continue_label, "\n", pct(gates[[2]]$probability[i]),
                        if (arm$response_based) paste0("; ", branch$name) else "")
      
      if (branch$action == "fixed") {
        node(b, paste0(
          design$stages[2], ": ", branch$treatments[1]
        ), arm_color = i)
        edge(origin, b, label)
        
      } else {
        node(b, "Re-randomize", "diamond")
        edge(origin, b, label)
        
        for (k in seq_along(branch$treatments)) {
          t <- paste0(b, "_t", k)
          
          node(t, paste0(
            design$stages[2], ": ", branch$treatments[k]
          ), arm_color = i)
          
          edge(b, t, pct(branch$allocation[k]))
        }
      }
    }
  }
  
  paste(c(lines, "}"), collapse = "\n")
}



# ============================================================================
# 04 | SIMULATION: SUPPORT FUNCTIONS
# ============================================================================

new_run_directory <- function(root, prefix = "run") {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(root)) {
    stop("Cannot create output directory: ", root)
  }
  
  # Timestamp plus unique suffix prevents overwriting.
  path <- tempfile(
    pattern = paste0(
      prefix, "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_"
    ),
    tmpdir = root
  )
  
  if (!dir.create(path)) {
    stop("Cannot create run directory.")
  }
  
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


exponential_time <- function(rate) {
  if (any(!is.finite(rate)) || any(rate < 0)) {
    stop("Hazards must be finite and nonnegative.")
  }
  
  result <- rep(Inf, length(rate))
  positive <- rate > 0
  
  result[positive] <- rexp(
    sum(positive),
    rate = rate[positive]
  )
  
  result
}


draw_censoring <- function(n, specification) {
  p <- specification$parameters
  
  switch(
    specification$distribution,
    
    none = rep(Inf, n),
    
    exponential = rexp(
      n,
      rate = p$rate
    ),
    
    weibull = rweibull(
      n,
      shape = p$shape,
      scale = p$scale
    ),
    
    uniform = runif(
      n,
      min = p$min,
      max = p$max
    ),
    
    stop("Unsupported censoring distribution.")
  )
}


lookup_hazard <- function(truth, stage, first,
                          current = NULL, response = NULL) {
  rows <- truth$survival
  
  keep <- rows$stage == stage &
    rows$first == first
  
  if (stage == 2L) {
    keep <- keep & rows$current == current
    
    if (truth$response_enabled) {
      keep <- keep & !is.na(rows$response) &
        rows$response == response
    }
  }
  
  answer <- rows$hazard[keep]
  
  if (length(answer) != 1L ||
      !is.finite(answer) ||
      answer < 0) {
    stop("Missing or invalid survival parameter.")
  }
  
  answer
}


# ============================================================================
# 05 | SIMULATION: GENERATE ONE TRIAL
# One initial cohort; independent censoring; one or two stages.
# ============================================================================

generate_trial <- function(truth, design) {
  n <- as.integer(design$recruitment$initial_n)
  cutoff <- design$administrative_censoring$cutoff
  has_stage2 <- length(design$stages) == 2L
  
  arm <- sample.int(
    length(design$arms),
    size = n,
    replace = TRUE,
    prob = design$allocation
  )
  
  first_treatment <- vapply(
    arm,
    function(a) design$arms[[a]]$treatment,
    character(1)
  )
  
  first_hazard <- vapply(
    first_treatment,
    function(d) lookup_hazard(truth, 1L, d),
    numeric(1)
  )
  
  first_event <- exponential_time(first_hazard)
  
  random_censor <- draw_censoring(n, truth$censoring)
  censor_time <- pmin(random_censor, cutoff)
  
  # Initialize follow-up using stage-1 event times.
  observed_time <- pmin(first_event, censor_time)
  event <- as.integer(first_event <= censor_time)
  
  response <- rep(NA_integer_, n)
  second_treatment <- rep(NA_character_, n)
  second_probability <- rep(NA_real_, n)
  reached_stage2 <- rep(FALSE, n)
  
  continuation1 <- continuation2 <- rep(NA, n)
  stop_time <- rep(NA_real_, n)
  stop_decision <- rep(NA_integer_, n)
  if (has_stage2) {
    gates <- design$continuation
    for (g in seq_along(gates)) {
      gate <- gates[[g]]
      if (!isTRUE(gate$enabled)) next
      candidates <- which(first_event > gate$time & censor_time > gate$time & is.na(stop_time))
      pass <- rbinom(length(candidates), 1L, gate$probability[arm[candidates]]) == 1L
      if (g == 1L) continuation1[candidates] <- pass else continuation2[candidates] <- pass
      stopped <- candidates[!pass]
      stop_time[stopped] <- gate$time
      stop_decision[stopped] <- g
      death <- gate$time + exponential_time(first_hazard[stopped] * gate$stop_hr)
      observed_time[stopped] <- pmin(death, censor_time[stopped])
      event[stopped] <- as.integer(death <= censor_time[stopped])
    }
    tau <- design$transition_time
    
    # Only observed, event-free patients undergo stage-2 assignment.
    reached_stage2 <- first_event > tau & censor_time > tau & is.na(stop_time)
    eligible <- which(reached_stage2)
    
    for (i in eligible) {
      a <- design$arms[[arm[i]]]
      r <- NA_integer_
      
      if (truth$response_enabled) {
        p_response <- truth$response$probability[
          match(first_treatment[i], truth$response$treatment)
        ]
        
        if (length(p_response) != 1L || is.na(p_response)) {
          stop("Missing response probability.")
        }
        
        r <- rbinom(1, size = 1, prob = p_response)
        response[i] <- r
      }
      
      branch <- if (a$response_based) {
        # Design stores responder first, nonresponder second.
        a$branches[[if (r == 1L) 1L else 2L]]
      } else {
        a$branches[[1L]]
      }
      
      k <- if (branch$action == "fixed") {
        1L
      } else {
        sample.int(
          length(branch$treatments),
          size = 1,
          prob = branch$allocation
        )
      }
      
      second_treatment[i] <- branch$treatments[k]
      second_probability[i] <- branch$allocation[k]
      
      second_hazard <- lookup_hazard(
        truth,
        stage = 2L,
        first = first_treatment[i],
        current = second_treatment[i],
        response = r
      )
      
      event_time <- tau + exponential_time(second_hazard)
      
      observed_time[i] <- min(event_time, censor_time[i])
      event[i] <- as.integer(event_time <= censor_time[i])
    }
  }
  
  censor_reason <- ifelse(
    event == 1L,
    "event",
    ifelse(random_censor < cutoff, "random", "administrative")
  )
  
  data.frame(
    id = seq_len(n),
    arm = arm,
    
    arm_label = vapply(
      arm,
      function(a) design$arms[[a]]$name,
      character(1)
    ),
    
    continued_to_consolidation = continuation1,
    continued_to_maintenance = continuation2,
    treatment_stop_time = stop_time,
    treatment_stop_decision = stop_decision,
    treatment1 = first_treatment,
    initial_probability = design$allocation[arm],
    reached_stage2 = reached_stage2,
    response = response,
    treatment2 = second_treatment,
    second_probability = second_probability,
    time = observed_time,
    event = event,
    censor_reason = censor_reason,
    
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# 06 | ANALYSIS: ONE DIRECT COMPARISON
# HR is comparison arm relative to reference arm.
# ============================================================================

analyze_direct <- function(data, reference, comparison, settings) {
  d <- data[
    data$arm %in% c(reference, comparison),
    ,
    drop = FALSE
  ]
  
  d$x <- as.integer(d$arm == comparison)
  
  warnings <- character()
  
  result <- tryCatch(
    withCallingHandlers({
      if (!all(c(0L, 1L) %in% d$x)) {
        stop("One comparison arm has no patients.")
      }
      
      if (sum(d$event) == 0L) {
        stop("No observed events.")
      }
      
      beta <- se <- hr <- lower <- upper <- NA_real_
      
      if (settings$test == "cox") {
        if (settings$variance == "robust" &&
            settings$ties == "exact") {
          stop("Use Efron or Breslow ties with robust variance.")
        }
        
        fit <- survival::coxph(
          survival::Surv(time, event) ~ x,
          data = d,
          ties = settings$ties,
          robust = settings$variance == "robust"
        )
        
        beta <- unname(stats::coef(fit)[1])
        se <- sqrt(unname(stats::vcov(fit)[1, 1]))
        
        if (!is.finite(beta) ||
            !is.finite(se) ||
            se <= 0) {
          stop("Invalid coefficient or standard error.")
        }
        
        z <- beta / se
        hr <- exp(beta)
        
        # Marginal two-sided CI; not multiplicity-adjusted.
        critical <- qnorm(1 - settings$alpha / 2)
        lower <- exp(beta - critical * se)
        upper <- exp(beta + critical * se)
        
      } else {
        fit <- survival::survdiff(
          survival::Surv(time, event) ~ x,
          data = d,
          rho = 0
        )
        
        v <- fit$var[2, 2]
        
        if (!is.finite(v) || v <= 0) {
          stop("Invalid log-rank variance.")
        }
        
        # Positive z means excess events in the comparison arm.
        z <- unname(
          (fit$obs[2] - fit$exp[2]) / sqrt(v)
        )
      }
      
      p <- switch(
        settings$sided,
        two = 2 * pnorm(-abs(z)),
        less = pnorm(z),
        greater = pnorm(z, lower.tail = FALSE)
      )
      
      list(
        ok = TRUE,
        estimate = beta,
        se = se,
        hr = hr,
        lower = lower,
        upper = upper,
        z = z,
        p = p,
        message = ""
      )
      
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    
    error = function(e) {
      list(
        ok = FALSE,
        estimate = NA_real_,
        se = NA_real_,
        hr = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        z = NA_real_,
        p = NA_real_,
        message = conditionMessage(e)
      )
    }
  )
  
  # Do not silently treat convergence warnings as successful fits.
  if (result$ok && length(warnings)) {
    result$ok <- FALSE
    result$p <- NA_real_
    result$message <- paste(unique(warnings), collapse = "; ")
  }
  
  data.frame(
    ok = result$ok,
    n = nrow(d),
    events = sum(d$event),
    estimate = result$estimate,
    se = result$se,
    hr = result$hr,
    lower = result$lower,
    upper = result$upper,
    z = result$z,
    p = result$p,
    message = result$message,
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# 07 | ANALYSIS: SUMMARIZE REPEATED DIRECT COMPARISONS
# ============================================================================

summarize_direct <- function(results, comparisons) {
  average <- function(x) {
    x <- x[is.finite(x)]
    if (length(x)) mean(x) else NA_real_
  }
  
  do.call(
    rbind,
    
    lapply(seq_len(nrow(comparisons)), function(j) {
      d <- results[results$comparison_id == j, , drop = FALSE]
      valid <- d$ok & is.finite(d$p_adjusted)
      completed <- sum(valid)
      
      rate <- if (completed) {
        mean(d$p_adjusted[valid] < d$alpha[valid])
      } else {
        NA_real_
      }
      
      data.frame(
        comparison = comparisons$label[j],
        repetitions = nrow(d),
        successful = completed,
        failed = sum(!valid),
        mean_events = average(d$events),
        mean_log_hr = average(d$estimate[valid]),
        empirical_sd = if (completed > 1L) {
          sd(d$estimate[valid])
        } else {
          NA_real_
        },
        mean_se = average(d$se[valid]),
        rejection_rate = rate,
        monte_carlo_se = if (completed) {
          sqrt(rate * (1 - rate) / completed)
        } else {
          NA_real_
        },
        
        stringsAsFactors = FALSE
      )
    })
  )
}


# ============================================================================
# 08 | VISUALIZATION: TRUE ARM SURVIVAL
# Uses saved Truth + Design, including response/treatment mixtures.
# ============================================================================

true_arm_survival <- function(time, truth, design, arm_index) {
  arm <- design$arms[[arm_index]]
  
  lambda1 <- lookup_hazard(
    truth,
    stage = 1L,
    first = arm$treatment
  )
  
  survival <- exp(-lambda1 * time)
  
  if (length(design$stages) == 1L) {
    return(survival)
  }
  
  tau <- design$transition_time
  after <- time > tau
  

  
  elapsed <- time[after] - tau
  mixture <- numeric(length(elapsed))
  
  response_values <- if (truth$response_enabled) {
    c(0L, 1L)
  } else {
    NA_integer_
  }
  
  if (truth$response_enabled) {
    p_response <- truth$response$probability[
      match(arm$treatment, truth$response$treatment)
    ]
    
    if (length(p_response) != 1L ||
        !is.finite(p_response)) {
      stop("Missing response probability.")
    }
  }
  
  for (r in response_values) {
    response_weight <- if (truth$response_enabled) {
      if (r == 1L) p_response else 1 - p_response
    } else {
      1
    }
    
    branch <- if (arm$response_based) {
      arm$branches[[if (r == 1L) 1L else 2L]]
    } else {
      arm$branches[[1L]]
    }
    
    for (k in seq_along(branch$treatments)) {
      lambda2 <- lookup_hazard(
        truth,
        stage = 2L,
        first = arm$treatment,
        current = branch$treatments[k],
        response = r
      )
      
      weight <- response_weight * branch$allocation[k]
      
      mixture <- mixture +
        weight * exp(-lambda2 * elapsed)
    }
  }
  
  survival[after] <- exp(-lambda1 * tau) * mixture
  gates <- design$continuation
  enabled <- Filter(function(g) isTRUE(g$enabled), gates)
  if (length(enabled)) {
    # Base curve describes patients who pass all decisions. Add disjoint stop paths.
    result <- survival
    for (q in seq_along(time)) {
      mass <- 1
      stopped_survival <- 0
      for (gate in enabled) {
        if (time[q] <= gate$time) next
        p <- gate$probability[arm_index]
        stopped_survival <- stopped_survival + mass * (1 - p) *
          exp(-lambda1 * gate$time - lambda1 * gate$stop_hr * (time[q] - gate$time))
        mass <- mass * p
      }
      result[q] <- mass * survival[q] + stopped_survival
    }
    survival <- result
  }
  
  survival
}

# ============================================================================
# 09 | LOK07: ENUMERATE SUPPORTED COMPARISONS
# Known, common second-randomization probability.
# ============================================================================

lok07_candidates <- function(design) {
  empty <- data.frame(
    reference = integer(),
    comparison = integer(),
    maintenance = character(),
    pi = numeric(),
    eligibility = character(),
    label = character(),
    stringsAsFactors = FALSE
  )
  
  if (length(design$stages) != 2L) {
    return(empty)
  }
  
  arm_specification <- function(arm) {
    if (isTRUE(arm$response_based)) {
      if (length(arm$branches) != 2L) return(NULL)
      
      randomized <- arm$branches[[1L]]
      nonresponders <- arm$branches[[2L]]
      
      if (nonresponders$action != "fixed") return(NULL)
      
      eligibility <- "responders"
      
    } else {
      if (length(arm$branches) != 1L) return(NULL)
      
      randomized <- arm$branches[[1L]]
      eligibility <- "all_eligible"
    }
    
    if (randomized$action != "random" ||
        length(randomized$treatments) != 2L ||
        length(randomized$allocation) != 2L ||
        any(!is.finite(randomized$allocation)) ||
        any(randomized$allocation <= 0) ||
        any(randomized$allocation >= 1) ||
        abs(sum(randomized$allocation) - 1) > 1e-8) {
      return(NULL)
    }
    
    list(
      eligibility = eligibility,
      treatments = randomized$treatments,
      probabilities = randomized$allocation
    )
  }
  
  specs <- lapply(design$arms, arm_specification)
  output <- list()
  
  for (i in seq_along(design$arms)) {
    for (j in seq_along(design$arms)) {
      if (i == j) next
      
      first <- design$arms[[i]]
      second <- design$arms[[j]]
      
      if (first$treatment == second$treatment) next
      
      si <- specs[[i]]
      sj <- specs[[j]]
      
      if (is.null(si) || is.null(sj)) next
      
      if (si$eligibility != sj$eligibility) next
      
      common <- intersect(
        si$treatments,
        sj$treatments
      )
      
      for (maintenance in common) {
        pi_i <- si$probabilities[
          match(maintenance, si$treatments)
        ]
        
        pi_j <- sj$probabilities[
          match(maintenance, sj$treatments)
        ]
        
        # Main paper assumes a common maintenance randomization probability.
        if (abs(pi_i - pi_j) > 1e-8) next
        
        label <- paste0(
          second$name, " [", second$treatment, "]",
          " vs ",
          first$name, " [", first$treatment, "]",
          " | maintenance: ", maintenance,
          " | ", si$eligibility
        )
        
        output[[length(output) + 1L]] <- data.frame(
          reference = i,
          comparison = j,
          maintenance = maintenance,
          pi = pi_i,
          eligibility = si$eligibility,
          label = label,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (!length(output)) return(empty)
  
  do.call(rbind, output)
}


# ============================================================================
# 10 | LOK07: KNOWN-PROBABILITY, TIME-INDEPENDENT WEIGHTS
# R here means observed second randomization, not simply biological response.
#
# Never re-randomized:                         weight = 1
# Re-randomized to target maintenance:         weight = 1/pi
# Re-randomized to other maintenance:          weight = 0
# ============================================================================

lok07_weights <- function(data, specification) {
  rerandomized <- if (
    specification$eligibility == "all_eligible"
  ) {
    data$reached_stage2
    
  } else {
    data$reached_stage2 &
      !is.na(data$response) &
      data$response == 1L
  }
  
  target <- !is.na(data$treatment2) &
    data$treatment2 == specification$maintenance
  
  weights <- rep(1, nrow(data))
  
  weights[rerandomized] <- 0
  
  weights[rerandomized & target] <-
    1 / specification$pi
  
  weights
}


# ============================================================================
# 11 | LOK07: WEIGHTED COX ESTIMATION AND ROBUST PSEUDO-SCORE TEST
#
# Weighted Cox estimate and sandwich SE.
# Test: U_w(0) / sqrt(B(beta_hat)), as specified on page 424.
# Breslow ties; one record per patient.
# ============================================================================

analyze_lok07 <- function(data, specification, settings) {
  warnings <- character()
  
  answer <- tryCatch(
    withCallingHandlers({
      selected <- data$arm %in% c(
        specification$reference,
        specification$comparison
      )
      
      d <- data[selected, , drop = FALSE]
      
      d$x <- as.integer(
        d$arm == specification$comparison
      )
      
      if (!all(c(0L, 1L) %in% d$x)) {
        stop("One initial arm has no patients.")
      }
      
      d$weight <- lok07_weights(
        d,
        specification
      )
      
      n_initial <- nrow(d)
      n_zero <- sum(d$weight == 0)
      
      # Zero-weight patients do not enter this estimator's risk sets.
      d <- d[d$weight > 0, , drop = FALSE]
      
      if (!all(c(0L, 1L) %in% d$x)) {
        stop("One initial arm has no positive-weight patients.")
      }
      
      if (sum(d$event) == 0L) {
        stop("No positive-weight observed events.")
      }
      
      fit <- survival::coxph(
        survival::Surv(time, event) ~ x,
        data = d,
        weights = weight,
        robust = TRUE,
        ties = "breslow"
      )
      
      beta <- unname(stats::coef(fit)[1])
      robust_variance <- unname(stats::vcov(fit)[1, 1])
      naive_variance <- unname(fit$naive.var[1, 1])
      
      if (!is.finite(beta) ||
          !is.finite(robust_variance) ||
          !is.finite(naive_variance) ||
          robust_variance <= 0 ||
          naive_variance <= 0) {
        stop("Invalid coefficient or variance estimate.")
      }
      
      se <- sqrt(robust_variance)
      
      # Robust covariance = inverse-information * B *
      #                     inverse-information.
      # With one coefficient:
      B_hat <- robust_variance / naive_variance^2
      
      # Compute weighted pseudo-score at beta = 0.
      # Descending time allows cumulative risk-set totals.
      ordering <- order(d$time, decreasing = TRUE)
      ordered_time <- d$time[ordering]
      
      risk_weight <- cumsum(
        d$weight[ordering]
      )
      
      risk_x_weight <- cumsum(
        d$weight[ordering] * d$x[ordering]
      )
      
      # End of each tied-time group includes everyone at risk at that time.
      group_end <- which(
        !duplicated(ordered_time, fromLast = TRUE)
      )
      
      risk_times <- ordered_time[group_end]
      mean_x <- risk_x_weight[group_end] /
        risk_weight[group_end]
      
      events <- which(d$event == 1L)
      
      event_mean_x <- mean_x[
        match(d$time[events], risk_times)
      ]
      
      U_zero <- sum(
        d$weight[events] *
          (d$x[events] - event_mean_x)
      )
      
      z <- U_zero / sqrt(B_hat)
      
      p <- switch(
        settings$sided,
        two = 2 * pnorm(-abs(z)),
        less = pnorm(z),
        greater = pnorm(z, lower.tail = FALSE),
        stop("Select a valid test direction.")
      )
      
      critical <- qnorm(
        1 - settings$alpha / 2
      )
      
      data.frame(
        ok = TRUE,
        n_initial = n_initial,
        n_used = nrow(d),
        n_zero_weight = n_zero,
        events_used = sum(d$event),
        estimate = beta,
        se = se,
        hr = exp(beta),
        lower = exp(beta - critical * se),
        upper = exp(beta + critical * se),
        score = U_zero,
        score_variance = B_hat,
        z = z,
        p = p,
        message = "",
        stringsAsFactors = FALSE
      )
      
    }, warning = function(w) {
      warnings <<- c(
        warnings,
        conditionMessage(w)
      )
      
      invokeRestart("muffleWarning")
    }),
    
    error = function(e) {
      data.frame(
        ok = FALSE,
        n_initial = NA_integer_,
        n_used = NA_integer_,
        n_zero_weight = NA_integer_,
        events_used = NA_integer_,
        estimate = NA_real_,
        se = NA_real_,
        hr = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        score = NA_real_,
        score_variance = NA_real_,
        z = NA_real_,
        p = NA_real_,
        message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
  
  if (answer$ok && length(warnings)) {
    answer$ok <- FALSE
    answer$p <- NA_real_
    
    answer$message <- paste(
      unique(warnings),
      collapse = "; "
    )
  }
  
  answer
}
