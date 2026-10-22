# Save as src/analysis.R. Run from the project root.
# Install dependencies separately:
# install.packages(c("ggplot2", "sandwich"))
# source("src/analysis.R")
# results <- run_analysis("/Users/lauramarch/Desktop/r_course/project/pips-project/alternative_dataset.csv")


# Save as src/analysis.R. Run from the project root.
# Install dependencies separately:
# install.packages(c("ggplot2", "sandwich"))
# source("src/analysis.R")
# results <- run_analysis("data/alternative_dataset.csv")
# source("src/analysis.R")
# results <- run_analysis("data/alternative_dataset.csv")



check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

# Read and validate the supplied dataset. No silent deletion or imputation.
# Returns a data.frame with explicitly coded categorical variables.
read_screen_data <- function(path) {
  if (!file.exists(path)) stop("Data file not found: ", path, call. = FALSE)
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("User_ID", "Age", "Gender", "Daily_Screen_Time_Hours",
                "Activity_Type", "Late_Night_Usage", "Social_Comparison_Trigger",
                "Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
  absent <- setdiff(required, names(d))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "))
  if (anyNA(d[required])) stop("Required columns contain missing values; resolve explicitly.")
  if (anyDuplicated(d$User_ID)) stop("Repeated IDs: this analysis assumes independent records.")
  if (any(trimws(d$User_ID) == "")) stop("User_ID cannot be empty.")
  numeric_columns <- setdiff(required, c("User_ID", "Gender", "Activity_Type"))
  for (column in numeric_columns) {
    if (!is.numeric(d[[column]]) || any(!is.finite(d[[column]]))) {
      stop(column, " must contain finite numeric values.")
    }
  }
  bounds <- list(Age = c(0, 120), Daily_Screen_Time_Hours = c(0, 24),
                 Sleep_Duration_Hours = c(0, 24), GAD_7_Score = c(0, 21),
                 PHQ_9_Score = c(0, 27))
  for (column in names(bounds)) {
    if (any(d[[column]] < bounds[[column]][1] | d[[column]] > bounds[[column]][2])) {
      stop("Out-of-range values in ", column)
    }
  }
  for (column in c("GAD_7_Score", "PHQ_9_Score")) {
    if (any(d[[column]] != floor(d[[column]]))) stop(column, " must be integer-valued.")
  }
  for (column in c("Late_Night_Usage", "Social_Comparison_Trigger")) {
    if (!all(d[[column]] %in% c(0, 1))) stop(column, " must be coded 0/1.")
  }
  for (column in c("Gender", "Activity_Type")) {
    if (any(trimws(d[[column]]) == "")) stop(column, " contains empty categories.")
    d[[column]] <- factor(d[[column]], levels = sort(unique(d[[column]])))
    if (nlevels(d[[column]]) < 2) stop(column, " needs at least two categories.")
  }
  if (nrow(d) < 50) stop("This workflow requires at least 50 independent records.")
  d
}

# Three exploratory linear models. Sleep is the primary outcome;
# symptom totals are secondary. No causal effects are estimated.
# Sleep is NOT adjusted for in symptom models because its causal role is unknown.
fit_screen_models <- function(d) {
  check_packages("sandwich")
  outcomes <- c("Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
  rhs <- paste("Daily_Screen_Time_Hours + Age + Gender + Late_Night_Usage +",
               "Social_Comparison_Trigger + Activity_Type")
  models <- lapply(outcomes, function(outcome) {
    fit <- stats::lm(stats::as.formula(paste(outcome, "~", rhs)), data = d)
    if (anyNA(stats::coef(fit))) stop("Rank-deficient model: inspect redundant predictors.")
    if (any(stats::hatvalues(fit) >= 0.999)) stop("Extreme leverage prevents HC3 estimation.")
    fit
  })
  names(models) <- outcomes
  models
}

# HC3 heteroskedasticity-robust coefficient intervals and approximate t tests.
robust_coefficients <- function(model) {
  check_packages("sandwich")
  b <- stats::coef(model)
  se <- sqrt(diag(sandwich::vcovHC(model, type = "HC3")))
  df <- stats::df.residual(model)
  critical <- stats::qt(0.975, df)
  data.frame(term = names(b), estimate = unname(b), robust_se = unname(se),
             conf_low = unname(b - critical * se),
             conf_high = unname(b + critical * se),
             p_value = unname(2 * stats::pt(-abs(b / se), df)), row.names = NULL)
}

# Robust 95% confidence intervals for conditional MEAN outcomes.
# These are not individual prediction intervals. Bounds are not clipped to
# questionnaire limits: impossible estimates are evidence of model limitations.
predict_mean <- function(model, newdata) {
  check_packages("sandwich")
  X <- stats::model.matrix(stats::delete.response(stats::terms(model)),
                          newdata, contrasts.arg = model$contrasts,
                          xlev = model$xlevels)
  X <- X[, names(stats::coef(model)), drop = FALSE]
  estimate <- as.vector(X %*% stats::coef(model))
  V <- sandwich::vcovHC(model, type = "HC3")
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  critical <- stats::qt(0.975, stats::df.residual(model))
  data.frame(estimate = estimate, conf_low = estimate - critical * se,
             conf_high = estimate + critical * se)
}

# A reproducible 80/20 split gives a basic out-of-sample prediction check.
# This is internal validation, not evidence of applicability to new populations.
validate_models <- function(d, seed = 2026) {
  set.seed(seed)
  train <- sample.int(nrow(d), floor(0.8 * nrow(d)))
  training <- d[train, , drop = FALSE]
  testing <- d[-train, , drop = FALSE]
  fits <- fit_screen_models(training)
  rows <- lapply(names(fits), function(outcome) {
    predicted <- as.numeric(stats::predict(fits[[outcome]], newdata = testing))
    observed <- testing[[outcome]]
    baseline <- mean(training[[outcome]])
    data.frame(outcome = outcome, test_n = nrow(testing),
               rmse = sqrt(mean((observed - predicted)^2)),
               mae = mean(abs(observed - predicted)),
               baseline_rmse = sqrt(mean((observed - baseline)^2)),
               test_r_squared = 1 - sum((observed - predicted)^2) /
                 sum((observed - baseline)^2))
  })
  do.call(rbind, rows)
}

# Screen-time effect plot: points are estimates per additional daily hour;
# horizontal bars are HC3 95% confidence intervals. Separate outcome scales.
plot_screen_effects <- function(effect_table) {
  check_packages("ggplot2")
  labels <- c(Sleep_Duration_Hours = "Sleep duration (hours)",
              GAD_7_Score = "Anxiety symptoms (GAD-7 points)",
              PHQ_9_Score = "Depressive symptoms (PHQ-9 points)")
  effect_table$outcome_label <- factor(unname(labels[effect_table$outcome]),
                                     levels = unname(labels))
  ggplot2::ggplot(effect_table, ggplot2::aes(x = estimate, y = "Screen time")) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_segment(ggplot2::aes(x = conf_low, xend = conf_high,
                                     yend = "Screen time"), linewidth = 1,
                          colour = "#176B87") +
    ggplot2::geom_point(size = 3, colour = "#176B87") +
    ggplot2::facet_wrap(~outcome_label, scales = "free_x", ncol = 1) +
    ggplot2::labs(title = "Screen time and psychological outcomes",
      subtitle = "Adjusted association per additional daily hour; 95% robust confidence intervals",
      x = "Outcome-unit difference per hour of screen time", y = NULL,
      caption = "Adjusted for age, gender, late-night use, social comparison and activity type.\nObservational associations do not establish causation.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

# Binned descriptive plot avoids hiding the spread behind a fitted line.
plot_sleep_pattern <- function(d) {
  check_packages("ggplot2")
  d$screen_bin <- floor(d$Daily_Screen_Time_Hours)
  groups <- split(d, d$screen_bin)
  summary_data <- do.call(rbind, lapply(groups, function(g) {
    data.frame(screen_hours = mean(g$Daily_Screen_Time_Hours), n = nrow(g),
               median_sleep = stats::median(g$Sleep_Duration_Hours),
               q25 = unname(stats::quantile(g$Sleep_Duration_Hours, 0.25)),
               q75 = unname(stats::quantile(g$Sleep_Duration_Hours, 0.75)))
  }))
  ggplot2::ggplot(summary_data, ggplot2::aes(screen_hours, median_sleep)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = q25, ymax = q75),
                            linewidth = 1, colour = "#176B87") +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = "#176B87") +
    ggplot2::scale_size_continuous(range = c(2, 6), name = "Records in bin") +
    ggplot2::labs(title = "Sleep across daily screen-time levels",
      subtitle = "One-hour bins: median and middle 50% of sleep duration",
      x = "Mean daily screen time within bin (hours)", y = "Sleep duration (hours)",
      caption = "Unadjusted description. Bars show variability, not confidence intervals.") +
    ggplot2::theme_minimal(base_size = 12)
}

# High-level entry point. Returns data, models, tables and plots; writes outputs.
# Seed controls the train/test split and diagnostic subsampling.
run_analysis <- function(data_path = "data/alternative_dataset.csv",
                         output_dir = "outputs", seed = 2026) {
  check_packages(c("ggplot2", "sandwich"))
  d <- read_screen_data(data_path)
  models <- fit_screen_models(d)
  coefficients <- do.call(rbind, lapply(names(models), function(outcome) {
    tab <- robust_coefficients(models[[outcome]])
    tab$outcome <- outcome
    tab
  }))
  rownames(coefficients) <- NULL
  effects <- coefficients[coefficients$term == "Daily_Screen_Time_Hours", ]
  # Holm adjustment covers the three screen-time/outcome tests only.
  effects$p_holm <- stats::p.adjust(effects$p_value, method = "holm")
  validation <- validate_models(d, seed)
  effect_plot <- plot_screen_effects(effects)
  sleep_plot <- plot_sleep_pattern(d)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(coefficients, file.path(output_dir, "all_coefficients.csv"), row.names = FALSE)
  utils::write.csv(effects, file.path(output_dir, "screen_time_effects.csv"), row.names = FALSE)
  utils::write.csv(validation, file.path(output_dir, "held_out_performance.csv"), row.names = FALSE)
  ggplot2::ggsave(file.path(output_dir, "adjusted_effects.png"), effect_plot,
                  width = 9, height = 7, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "sleep_pattern.png"), sleep_plot,
                  width = 8, height = 5, dpi = 300)
  # Inspect residual-vs-fitted and Q-Q plots before finalising interpretation.
  grDevices::pdf(file.path(output_dir, "model_diagnostics.pdf"), width = 9, height = 5)
  tryCatch({
    graphics::par(mfrow = c(1, 2))
    set.seed(seed)
    for (outcome in names(models)) {
      fit <- models[[outcome]]
      ii <- sample.int(nrow(d), min(2000L, nrow(d)))
      graphics::plot(stats::fitted(fit)[ii], stats::residuals(fit)[ii],
        xlab = "Fitted outcome", ylab = "Residual", main = outcome,
        pch = 16, col = grDevices::adjustcolor("#176B87", alpha.f = 0.15))
      graphics::abline(h = 0, lty = 2)
      graphics::lines(stats::lowess(stats::fitted(fit), stats::residuals(fit)),
                      col = "#B34444", lwd = 2)
      stats::qqnorm(stats::residuals(fit)[ii], main = paste(outcome, "Q-Q"),
                    pch = 16, cex = 0.5)
      stats::qqline(stats::residuals(fit)[ii], col = "#B34444")
    }
  }, finally = grDevices::dev.off())
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "session_info.txt"))
  cat("\nExploratory adjusted screen-time associations:\n")
  print(effects, row.names = FALSE)
  cat("\nHeld-out performance (compare model RMSE with baseline RMSE):\n")
  print(validation, row.names = FALSE)
  for (i in seq_len(nrow(effects))) {
    r <- effects[i, ]
    cat(sprintf("\n%s: one additional screen-time hour is associated with %.3f outcome units (95%% HC3 CI %.3f to %.3f).\n",
                r$outcome, r$estimate, r$conf_low, r$conf_high))
  }
  cat("\nThese are associations, not causal effects or clinical diagnoses.\n",
      "The source/design of the dataset must be verified; synthetic data cannot establish real health risks.\n",
      "Bounded symptom totals, model nonlinearity and unknown sampling limit interpretation.\n",
      "Review diagnostics; robust standard errors do not fix a misspecified mean model.\n", sep = "")
  print(effect_plot)
  print(sleep_plot)
  invisible(list(data = d, models = models, coefficients = coefficients,
                 screen_effects = effects, validation = validation,
                 plots = list(effects = effect_plot, sleep = sleep_plot)))
}

# Also supports: Rscript src/analysis.R
if (sys.nframe() == 0L) run_analysis()

check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

# Read and validate the supplied dataset. No silent deletion or imputation.
# Returns a data.frame with explicitly coded categorical variables.
read_screen_data <- function(path) {
  if (!file.exists(path)) stop("Data file not found: ", path, call. = FALSE)
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("User_ID", "Age", "Gender", "Daily_Screen_Time_Hours",
                "Activity_Type", "Late_Night_Usage", "Social_Comparison_Trigger",
                "Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
  absent <- setdiff(required, names(d))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "))
  if (anyNA(d[required])) stop("Required columns contain missing values; resolve explicitly.")
  if (anyDuplicated(d$User_ID)) stop("Repeated IDs: this analysis assumes independent records.")
  if (any(trimws(d$User_ID) == "")) stop("User_ID cannot be empty.")
  numeric_columns <- setdiff(required, c("User_ID", "Gender", "Activity_Type"))
  for (column in numeric_columns) {
    if (!is.numeric(d[[column]]) || any(!is.finite(d[[column]]))) {
      stop(column, " must contain finite numeric values.")
    }
  }
  bounds <- list(Age = c(0, 120), Daily_Screen_Time_Hours = c(0, 24),
                 Sleep_Duration_Hours = c(0, 24), GAD_7_Score = c(0, 21),
                 PHQ_9_Score = c(0, 27))
  for (column in names(bounds)) {
    if (any(d[[column]] < bounds[[column]][1] | d[[column]] > bounds[[column]][2])) {
      stop("Out-of-range values in ", column)
    }
  }
  for (column in c("GAD_7_Score", "PHQ_9_Score")) {
    if (any(d[[column]] != floor(d[[column]]))) stop(column, " must be integer-valued.")
  }
  for (column in c("Late_Night_Usage", "Social_Comparison_Trigger")) {
    if (!all(d[[column]] %in% c(0, 1))) stop(column, " must be coded 0/1.")
  }
  for (column in c("Gender", "Activity_Type")) {
    if (any(trimws(d[[column]]) == "")) stop(column, " contains empty categories.")
    d[[column]] <- factor(d[[column]], levels = sort(unique(d[[column]])))
    if (nlevels(d[[column]]) < 2) stop(column, " needs at least two categories.")
  }
  if (nrow(d) < 50) stop("This workflow requires at least 50 independent records.")
  d
}

# Three exploratory linear models. Sleep is the primary outcome;
# symptom totals are secondary. No causal effects are estimated.
# Sleep is NOT adjusted for in symptom models because its causal role is unknown.
fit_screen_models <- function(d) {
  check_packages("sandwich")
  outcomes <- c("Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
  rhs <- paste("Daily_Screen_Time_Hours + Age + Gender + Late_Night_Usage +",
               "Social_Comparison_Trigger + Activity_Type")
  models <- lapply(outcomes, function(outcome) {
    fit <- stats::lm(stats::as.formula(paste(outcome, "~", rhs)), data = d)
    if (anyNA(stats::coef(fit))) stop("Rank-deficient model: inspect redundant predictors.")
    if (any(stats::hatvalues(fit) >= 0.999)) stop("Extreme leverage prevents HC3 estimation.")
    fit
  })
  names(models) <- outcomes
  models
}

# HC3 heteroskedasticity-robust coefficient intervals and approximate t tests.
robust_coefficients <- function(model) {
  check_packages("sandwich")
  b <- stats::coef(model)
  se <- sqrt(diag(sandwich::vcovHC(model, type = "HC3")))
  df <- stats::df.residual(model)
  critical <- stats::qt(0.975, df)
  data.frame(term = names(b), estimate = unname(b), robust_se = unname(se),
             conf_low = unname(b - critical * se),
             conf_high = unname(b + critical * se),
             p_value = unname(2 * stats::pt(-abs(b / se), df)), row.names = NULL)
}

# Robust 95% confidence intervals for conditional MEAN outcomes.
# These are not individual prediction intervals. Bounds are not clipped to
# questionnaire limits: impossible estimates are evidence of model limitations.
predict_mean <- function(model, newdata) {
  check_packages("sandwich")
  X <- stats::model.matrix(stats::delete.response(stats::terms(model)),
                           newdata, contrasts.arg = model$contrasts,
                           xlev = model$xlevels)
  X <- X[, names(stats::coef(model)), drop = FALSE]
  estimate <- as.vector(X %*% stats::coef(model))
  V <- sandwich::vcovHC(model, type = "HC3")
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  critical <- stats::qt(0.975, stats::df.residual(model))
  data.frame(estimate = estimate, conf_low = estimate - critical * se,
             conf_high = estimate + critical * se)
}

# A reproducible 80/20 split gives a basic out-of-sample prediction check.
# This is internal validation, not evidence of applicability to new populations.
validate_models <- function(d, seed = 2026) {
  set.seed(seed)
  train <- sample.int(nrow(d), floor(0.8 * nrow(d)))
  training <- d[train, , drop = FALSE]
  testing <- d[-train, , drop = FALSE]
  fits <- fit_screen_models(training)
  rows <- lapply(names(fits), function(outcome) {
    predicted <- as.numeric(stats::predict(fits[[outcome]], newdata = testing))
    observed <- testing[[outcome]]
    baseline <- mean(training[[outcome]])
    data.frame(outcome = outcome, test_n = nrow(testing),
               rmse = sqrt(mean((observed - predicted)^2)),
               mae = mean(abs(observed - predicted)),
               baseline_rmse = sqrt(mean((observed - baseline)^2)),
               test_r_squared = 1 - sum((observed - predicted)^2) /
                 sum((observed - baseline)^2))
  })
  do.call(rbind, rows)
}

# Screen-time effect plot: points are estimates per additional daily hour;
# horizontal bars are HC3 95% confidence intervals. Separate outcome scales.
plot_screen_effects <- function(effect_table) {
  check_packages("ggplot2")
  labels <- c(Sleep_Duration_Hours = "Sleep duration (hours)",
              GAD_7_Score = "Anxiety symptoms (GAD-7 points)",
              PHQ_9_Score = "Depressive symptoms (PHQ-9 points)")
  effect_table$outcome_label <- factor(unname(labels[effect_table$outcome]),
                                       levels = unname(labels))
  ggplot2::ggplot(effect_table, ggplot2::aes(x = estimate, y = "Screen time")) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_segment(ggplot2::aes(x = conf_low, xend = conf_high,
                                       yend = "Screen time"), linewidth = 1,
                          colour = "#176B87") +
    ggplot2::geom_point(size = 3, colour = "#176B87") +
    ggplot2::facet_wrap(~outcome_label, scales = "free_x", ncol = 1) +
    ggplot2::labs(title = "Screen time and psychological outcomes",
                  subtitle = "Adjusted association per additional daily hour; 95% robust confidence intervals",
                  x = "Outcome-unit difference per hour of screen time", y = NULL,
                  caption = "Adjusted for age, gender, late-night use, social comparison and activity type.\nObservational associations do not establish causation.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

# Binned descriptive plot avoids hiding the spread behind a fitted line.
plot_sleep_pattern <- function(d) {
  check_packages("ggplot2")
  d$screen_bin <- floor(d$Daily_Screen_Time_Hours)
  groups <- split(d, d$screen_bin)
  summary_data <- do.call(rbind, lapply(groups, function(g) {
    data.frame(screen_hours = mean(g$Daily_Screen_Time_Hours), n = nrow(g),
               median_sleep = stats::median(g$Sleep_Duration_Hours),
               q25 = unname(stats::quantile(g$Sleep_Duration_Hours, 0.25)),
               q75 = unname(stats::quantile(g$Sleep_Duration_Hours, 0.75)))
  }))
  ggplot2::ggplot(summary_data, ggplot2::aes(screen_hours, median_sleep)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = q25, ymax = q75),
                            linewidth = 1, colour = "#176B87") +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = "#176B87") +
    ggplot2::scale_size_continuous(range = c(2, 6), name = "Records in bin") +
    ggplot2::labs(title = "Sleep across daily screen-time levels",
                  subtitle = "One-hour bins: median and middle 50% of sleep duration",
                  x = "Mean daily screen time within bin (hours)", y = "Sleep duration (hours)",
                  caption = "Unadjusted description. Bars show variability, not confidence intervals.") +
    ggplot2::theme_minimal(base_size = 12)
}

# High-level entry point. Returns data, models, tables and plots; writes outputs.
# Seed controls the train/test split and diagnostic subsampling.
run_analysis <- function(data_path = "data/alternative_dataset.csv",
                         output_dir = "outputs", seed = 2026) {
  check_packages(c("ggplot2", "sandwich"))
  d <- read_screen_data(data_path)
  models <- fit_screen_models(d)
  coefficients <- do.call(rbind, lapply(names(models), function(outcome) {
    tab <- robust_coefficients(models[[outcome]])
    tab$outcome <- outcome
    tab
  }))
  rownames(coefficients) <- NULL
  effects <- coefficients[coefficients$term == "Daily_Screen_Time_Hours", ]
  # Holm adjustment covers the three screen-time/outcome tests only.
  effects$p_holm <- stats::p.adjust(effects$p_value, method = "holm")
  validation <- validate_models(d, seed)
  effect_plot <- plot_screen_effects(effects)
  sleep_plot <- plot_sleep_pattern(d)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(coefficients, file.path(output_dir, "all_coefficients.csv"), row.names = FALSE)
  utils::write.csv(effects, file.path(output_dir, "screen_time_effects.csv"), row.names = FALSE)
  utils::write.csv(validation, file.path(output_dir, "held_out_performance.csv"), row.names = FALSE)
  ggplot2::ggsave(file.path(output_dir, "adjusted_effects.png"), effect_plot,
                  width = 9, height = 7, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "sleep_pattern.png"), sleep_plot,
                  width = 8, height = 5, dpi = 300)
  # Inspect residual-vs-fitted and Q-Q plots before finalising interpretation.
  grDevices::pdf(file.path(output_dir, "model_diagnostics.pdf"), width = 9, height = 5)
  tryCatch({
    graphics::par(mfrow = c(1, 2))
    set.seed(seed)
    for (outcome in names(models)) {
      fit <- models[[outcome]]
      ii <- sample.int(nrow(d), min(2000L, nrow(d)))
      graphics::plot(stats::fitted(fit)[ii], stats::residuals(fit)[ii],
                     xlab = "Fitted outcome", ylab = "Residual", main = outcome,
                     pch = 16, col = grDevices::adjustcolor("#176B87", alpha.f = 0.15))
      graphics::abline(h = 0, lty = 2)
      graphics::lines(stats::lowess(stats::fitted(fit), stats::residuals(fit)),
                      col = "#B34444", lwd = 2)
      stats::qqnorm(stats::residuals(fit)[ii], main = paste(outcome, "Q-Q"),
                    pch = 16, cex = 0.5)
      stats::qqline(stats::residuals(fit)[ii], col = "#B34444")
    }
  }, finally = grDevices::dev.off())
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "session_info.txt"))
  cat("\nExploratory adjusted screen-time associations:\n")
  print(effects, row.names = FALSE)
  cat("\nHeld-out performance (compare model RMSE with baseline RMSE):\n")
  print(validation, row.names = FALSE)
  for (i in seq_len(nrow(effects))) {
    r <- effects[i, ]
    cat(sprintf("\n%s: one additional screen-time hour is associated with %.3f outcome units (95%% HC3 CI %.3f to %.3f).\n",
                r$outcome, r$estimate, r$conf_low, r$conf_high))
  }
  cat("\nThese are associations, not causal effects or clinical diagnoses.\n",
      "The source/design of the dataset must be verified; synthetic data cannot establish real health risks.\n",
      "Bounded symptom totals, model nonlinearity and unknown sampling limit interpretation.\n",
      "Review diagnostics; robust standard errors do not fix a misspecified mean model.\n", sep = "")
  print(effect_plot)
  print(sleep_plot)
  invisible(list(data = d, models = models, coefficients = coefficients,
                 screen_effects = effects, validation = validation,
                 plots = list(effects = effect_plot, sleep = sleep_plot)))
}

# Also supports: Rscript src/analysis.R
if (sys.nframe() == 0L) run_analysis()
