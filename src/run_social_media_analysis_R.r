# Save as src/run_social_media_analysis.R.
# source() defines this function only; main.Rmd must call it explicitly.

#' Run a reproducible simulated social media and health analysis
#'
#' @description
#' Simulates fictional app screen time and health outcomes, fits four multiple
#' linear regression models, and saves tables, plots, diagnostics, session
#' information, and a readable statistical findings report in one folder.
#'
#' @param n Number of simulated participants (integer, at least 50).
#' @param seed Integer random seed. The caller's random state is restored.
#' @param output_folder Output directory. Created if needed. Existing files
#'   with matching names are overwritten; unrelated files are not deleted.
#' @param print_plots Print the five ggplot figures in the active graphics
#'   device, including main.Rmd. All figures are always saved to disk.
#' @param verbose Print principal results tables and completion message.
#' @param alpha Significance threshold for Holm-adjusted app tests.
#'
#' @return Invisibly returns a list of data, descriptive tables, models,
#'   coefficients, robust inference, model fit, VIFs, plots, report text,
#'   output directory, and paths of files written during this run.
#'
#' @details
#' Data and imposed effects are fictional. Outcome scores are illustrative,
#' not validated clinical measures. Models include all five app exposures,
#' age, and gender; app coefficients are per additional hour/day.
#' Primary inference uses ordinary least squares with Holm adjustment over
#' all 20 app-by-outcome tests. Within-outcome Holm adjustments are also saved.
#' HC3 robust inference is a sensitivity analysis with its own Holm adjustment
#' over the same 20 tests. Confidence intervals are pointwise, not simultaneous.
#' Dependencies: MASS, ggplot2, tidyr, broom, car, sandwich, lmtest.
#' Install dependencies separately, not while knitting main.Rmd.
#' No packages are attached, no working directory is changed, and no global
#' plotting theme or interactive data viewer is used.
#'
#' @examples
#' # In main.Rmd, located in the project root:
#' # source("src/run_social_media_analysis.R")
#' # social_results <- run_social_media_analysis(verbose = FALSE)
#' # knitr::kable(social_results$app_results, digits = 3)
run_social_media_analysis <- function(
    n = 500L,
    seed = 2026L,
    output_folder = "social_media_analysis_outputs",
    print_plots = TRUE,
    verbose = TRUE,
    alpha = 0.05) {

  # 1. Validate arguments and check dependencies.
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
      n < 50 || n != floor(n) || n > .Machine$integer.max) {
    stop("n must be an integer between 50 and the R integer limit.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed != floor(seed) || seed < 0 || seed > .Machine$integer.max) {
    stop("seed must be a non-negative integer within the R integer limit.",
         call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("alpha must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(print_plots) || length(print_plots) != 1L ||
      is.na(print_plots) || !is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) {
    stop("print_plots and verbose must each be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(output_folder) || length(output_folder) != 1L ||
      is.na(output_folder) || !nzchar(output_folder)) {
    stop("output_folder must be a non-empty directory path.", call. = FALSE)
  }
  packages <- c("MASS", "ggplot2", "tidyr", "broom", "car", "sandwich", "lmtest")
  missing_packages <- packages[!vapply(
    packages, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(missing_packages)) {
    stop("Install these packages before running: ",
         paste(missing_packages, collapse = ", "), call. = FALSE)
  }

  # Preserve the caller's RNG state, including on error.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  n <- as.integer(n)

  # 2. Set up one output directory and local saving helpers.
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_folder)) {
    stop("Could not create output directory: ", output_folder, call. = FALSE)
  }
  output_folder <- normalizePath(output_folder, mustWork = TRUE)
  files_written <- character()
  save_csv <- function(table, filename) {
    path <- file.path(output_folder, filename)
    utils::write.csv(table, path, row.names = FALSE, na = "")
    files_written <<- c(files_written, path)
  }
  save_text <- function(text, filename) {
    path <- file.path(output_folder, filename)
    writeLines(text, path)
    files_written <<- c(files_written, path)
  }
  save_plot <- function(plot, filename, width, height) {
    path <- file.path(output_folder, filename)
    ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
    files_written <<- c(files_written, path)
    if (print_plots) print(plot)
  }
  show_table <- function(title, table) {
    if (verbose) {
      cat("\n--- ", title, " ---\n", sep = "")
      print(as.data.frame(table), row.names = FALSE)
    }
  }

  # 3. Simulate the same fictional data-generating mechanism as the original.
  app_names <- c("snapchat_hours", "instagram_hours", "tiktok_hours",
                 "facebook_hours", "pinterest_hours")
  app_labels <- setNames(c("Snapchat", "Instagram", "TikTok", "Facebook",
                          "Pinterest"), app_names)
  medians <- c(35, 55, 45, 20, 15)
  sigmas <- c(0.75, 0.70, 0.80, 0.85, 0.90)
  correlation_matrix <- matrix(c(
    1.00, 0.48, 0.42, 0.25, 0.22,
    0.48, 1.00, 0.50, 0.30, 0.32,
    0.42, 0.50, 1.00, 0.22, 0.25,
    0.25, 0.30, 0.22, 1.00, 0.28,
    0.22, 0.32, 0.25, 0.28, 1.00
  ), nrow = 5, byrow = TRUE)
  covariance_matrix <- diag(sigmas) %*% correlation_matrix %*% diag(sigmas)
  z <- MASS::mvrnorm(n, mu = rep(0, 5), Sigma = covariance_matrix)
  app_minutes <- round(pmin(exp(sweep(z, 2, log(medians), "+")), 300))
  colnames(app_minutes) <- app_names
  age <- sample(18:60, n, replace = TRUE)
  gender <- sample(c("Woman", "Man", "Non-binary / another identity"),
                   n, replace = TRUE, prob = c(0.50, 0.47, 0.03))
  dat <- data.frame(participant_id = seq_len(n), age = age,
                    gender = factor(gender), app_minutes, check.names = FALSE)
  app_hours <- as.matrix(dat[app_names]) / 60
  age_c <- (age - 30) / 10
  gender_effect <- ifelse(gender == "Woman", 0.25,
                          ifelse(gender == "Man", 0, 0.15))
  dat$depression_score <- as.numeric(round(pmin(27, pmax(0,
    5 + app_hours %*% c(0.35, 0.45, 0.60, 0.15, 0.05) +
      0.15 * age_c + gender_effect + stats::rnorm(n, 0, 3.2)))))
  dat$anxiety_score <- as.numeric(round(pmin(21, pmax(0,
    4.5 + app_hours %*% c(0.40, 0.45, 0.55, 0.10, 0.05) +
      0.10 * age_c + 0.7 * gender_effect + stats::rnorm(n, 0, 3.0)))))
  dat$sleep_problems_score <- as.numeric(round(pmin(21, pmax(0,
    5 + app_hours %*% c(0.25, 0.40, 0.55, 0.10, 0.05) -
      0.10 * age_c + stats::rnorm(n, 0, 2.8)))))
  dat$physical_activity_min_day <- as.numeric(round(pmin(180, pmax(0,
    42 - app_hours %*% c(1.2, 1.5, 2.0, 0.5, 0.3) +
      0.25 * age_c + stats::rnorm(n, 0, 13)))))
  dat[app_names] <- dat[app_names] / 60
  if (nlevels(dat$gender) < 2L) {
    stop("Simulation generated fewer than two gender groups. Use another seed.",
         call. = FALSE)
  }
  save_csv(dat, "simulated_social_media_health_data.csv")

  outcome_names <- c("physical_activity_min_day", "sleep_problems_score",
                     "depression_score", "anxiety_score")
  outcome_labels <- setNames(c("Physical activity (min/day)", "Sleep problems score",
                               "Depression score", "Anxiety score"), outcome_names)
  model_names <- setNames(c("Physical activity", "Sleep problems", "Depression",
                            "Anxiety"), outcome_names)

  # 4. Descriptive statistics, group counts, and missingness.
  continuous_vars <- c(app_names, "age", outcome_names)
  descriptives <- do.call(rbind, lapply(continuous_vars, function(variable) {
    x <- dat[[variable]]
    data.frame(variable = variable, n = length(x), mean = mean(x),
               sd = stats::sd(x), median = stats::median(x), IQR = stats::IQR(x),
               min = min(x), max = max(x))
  }))
  gender_summary <- as.data.frame(table(dat$gender), stringsAsFactors = FALSE)
  names(gender_summary) <- c("gender", "n")
  gender_summary$percent <- 100 * gender_summary$n / n
  missing_summary <- data.frame(
    variable = names(dat),
    missing_n = vapply(dat, function(x) sum(is.na(x)), numeric(1))
  )
  missing_summary$total_n <- n
  missing_summary$missing_percent <- 100 * missing_summary$missing_n / n
  save_csv(descriptives, "descriptive_statistics.csv")
  save_csv(gender_summary, "gender_counts.csv")
  save_csv(missing_summary, "missing_data_summary.csv")
  show_table("DESCRIPTIVE STATISTICS", descriptives)
  show_table("GENDER COUNTS", gender_summary)

  # 5. Fit all five app exposures simultaneously, adjusting for age and gender.
  models <- setNames(lapply(outcome_names, function(outcome) {
    stats::lm(stats::reformulate(c(app_names, "age", "gender"), response = outcome),
              data = dat)
  }), unname(model_names))
  if (any(vapply(models, function(m) anyNA(stats::coef(m)), logical(1)))) {
    stop("A model is rank deficient; choose another seed or increase n.",
         call. = FALSE)
  }
  tidy_results <- do.call(rbind, lapply(names(models), function(outcome) {
    table <- as.data.frame(broom::tidy(models[[outcome]], conf.int = TRUE))
    table$outcome <- outcome
    table
  }))
  rownames(tidy_results) <- NULL
  app_results <- tidy_results[tidy_results$term %in% app_names, , drop = FALSE]
  app_results$app <- unname(app_labels[app_results$term])
  app_results$p_holm_within_outcome <- ave(
    app_results$p.value, app_results$outcome,
    FUN = function(p) stats::p.adjust(p, method = "holm")
  )
  app_results$p_holm_all_app_tests <- stats::p.adjust(app_results$p.value, "holm")
  app_results <- app_results[c("outcome", "app", "term", "estimate", "std.error",
                                "statistic", "p.value", "p_holm_within_outcome",
                                "p_holm_all_app_tests", "conf.low", "conf.high")]
  model_fit <- do.call(rbind, lapply(names(models), function(outcome) {
    table <- as.data.frame(broom::glance(models[[outcome]]))
    table$outcome <- outcome
    table[c("outcome", "nobs", "r.squared", "adj.r.squared", "sigma",
            "statistic", "p.value", "AIC", "BIC")]
  }))
  rownames(model_fit) <- NULL
  save_csv(tidy_results, "regression_all_coefficients.csv")
  save_csv(app_results, "regression_app_coefficients.csv")
  save_csv(model_fit, "regression_model_fit.csv")
  show_table("ADJUSTED APP COEFFICIENTS", app_results)
  show_table("MODEL FIT", model_fit)

  # 6. HC3 sensitivity analysis: unchanged estimates, robust SEs and inference.
  robust_results <- do.call(rbind, lapply(names(models), function(outcome) {
    m <- models[[outcome]]
    ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC3"))
    critical <- stats::qt(0.975, df = stats::df.residual(m))
    data.frame(outcome = outcome, term = rownames(ct), estimate = ct[, 1],
               robust_se_HC3 = ct[, 2], t_value = ct[, 3], p_value_HC3 = ct[, 4],
               conf_low_HC3 = ct[, 1] - critical * ct[, 2],
               conf_high_HC3 = ct[, 1] + critical * ct[, 2], row.names = NULL)
  }))
  robust_results <- robust_results[robust_results$term %in% app_names, , drop = FALSE]
  robust_results$app <- unname(app_labels[robust_results$term])
  robust_results$p_holm_all_app_tests_HC3 <- stats::p.adjust(
    robust_results$p_value_HC3, method = "holm"
  )
  save_csv(robust_results, "regression_HC3_robust_results.csv")
  show_table("HC3 ROBUST SENSITIVITY ANALYSIS", robust_results)

  vif_results <- do.call(rbind, lapply(names(models), function(outcome) {
    v <- car::vif(models[[outcome]])
    if (is.matrix(v)) {
      data.frame(model = outcome, term = rownames(v), GVIF = v[, "GVIF"],
                 Df = v[, "Df"],
                 adjusted_GVIF = v[, "GVIF"]^(1 / (2 * v[, "Df"])),
                 row.names = NULL)
    } else {
      data.frame(model = outcome, term = names(v), GVIF = as.numeric(v), Df = 1,
                 adjusted_GVIF = sqrt(as.numeric(v)), row.names = NULL)
    }
  }))
  save_csv(vif_results, "vif_results.csv")

  # 7. Preserve the original five main figures without changing global themes.
  local_theme <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )
  outcome_long <- tidyr::pivot_longer(
    dat[outcome_names], cols = tidyr::everything(),
    names_to = "outcome", values_to = "score"
  )
  outcome_long$outcome <- unname(outcome_labels[outcome_long$outcome])
  p_outcome <- ggplot2::ggplot(outcome_long, ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(bins = 25, color = "white", fill = "#3977A8") +
    ggplot2::facet_wrap(~ outcome, scales = "free", ncol = 2) +
    ggplot2::labs(title = "Distributions of simulated health outcomes",
                  x = NULL, y = "Number of participants") + local_theme
  screen_long <- tidyr::pivot_longer(
    dat[app_names], cols = tidyr::everything(),
    names_to = "app", values_to = "hours_day"
  )
  screen_long$app <- unname(app_labels[screen_long$app])
  p_screen <- ggplot2::ggplot(screen_long, ggplot2::aes(x = hours_day)) +
    ggplot2::geom_histogram(bins = 25, color = "white", fill = "#2A9D8F") +
    ggplot2::facet_wrap(~ app, scales = "free", ncol = 3) +
    ggplot2::labs(title = "Daily screen time by social media app",
                  x = "Screen time (hours/day)", y = "Number of participants") +
    local_theme
  scatter_data <- tidyr::pivot_longer(
    dat[c(app_names, outcome_names)], cols = tidyr::all_of(app_names),
    names_to = "app", values_to = "hours_day"
  )
  scatter_data <- tidyr::pivot_longer(
    scatter_data, cols = tidyr::all_of(outcome_names),
    names_to = "outcome", values_to = "score"
  )
  scatter_data$app <- unname(app_labels[scatter_data$app])
  scatter_data$outcome <- unname(outcome_labels[scatter_data$outcome])
  p_scatter <- ggplot2::ggplot(
    scatter_data, ggplot2::aes(x = hours_day, y = score)
  ) +
    ggplot2::geom_point(alpha = 0.28, size = 1.2, color = "#264653") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                          color = "#E76F51", linewidth = 0.8) +
    ggplot2::facet_grid(outcome ~ app, scales = "free") +
    ggplot2::labs(
      title = "Unadjusted associations between app time and outcomes",
      subtitle = "Separate simple linear fits; no covariate adjustment",
      x = "Screen time (hours/day)", y = NULL
    ) + local_theme
  cor_matrix <- stats::cor(dat[app_names])
  cor_long <- as.data.frame(as.table(cor_matrix), stringsAsFactors = FALSE)
  names(cor_long) <- c("app_1", "app_2", "correlation")
  cor_long$app_1 <- unname(app_labels[cor_long$app_1])
  cor_long$app_2 <- unname(app_labels[cor_long$app_2])
  save_csv(cor_long, "app_time_correlations.csv")
  p_cor <- ggplot2::ggplot(
    cor_long, ggplot2::aes(x = app_1, y = app_2, fill = correlation)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 3.5) +
    ggplot2::scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Correlations between social media screen-time variables",
                  x = NULL, y = NULL, fill = "Pearson r") + local_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  p_coef <- ggplot2::ggplot(
    app_results,
    ggplot2::aes(x = estimate, y = factor(app, levels = rev(unname(app_labels))))
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                           orientation = "y", width = 0.18, color = "#3977A8") +
    ggplot2::geom_point(size = 2.6, color = "orange") +
    ggplot2::facet_wrap(~ outcome, scales = "free_x", ncol = 2) +
    ggplot2::labs(
      title = "Adjusted associations between app screen time and outcomes",
      subtitle = "Coefficients and pointwise 95% confidence intervals",
      x = "Change in outcome score/minutes per additional hour/day",
      y = "Social media app"
    ) + local_theme
  plots <- list(outcome_distributions = p_outcome,
                screen_time_distributions = p_screen,
                unadjusted_associations = p_scatter,
                app_correlations = p_cor, adjusted_coefficients = p_coef)
  save_plot(p_outcome, "01_outcome_distributions.png", 10, 7)
  save_plot(p_screen, "02_screen_time_distributions.png", 10, 6)
  save_plot(p_scatter, "03_unadjusted_scatterplots.png", 15, 10)
  save_plot(p_cor, "04_app_time_correlations.png", 8, 7)
  save_plot(p_coef, "05_adjusted_coefficient_plot.png", 12, 8)

  # 8. Save four-panel diagnostics on private devices, closed even on error.
  save_diagnostics <- function(model, model_name) {
    safe_name <- gsub("[^A-Za-z0-9]+", "_", tolower(model_name))
    path <- file.path(output_folder, paste0("diagnostics_", safe_name, ".png"))
    grDevices::png(path, width = 1400, height = 1200, res = 160)
    device_id <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(which = device_id), add = TRUE)
    graphics::par(mfrow = c(2, 2))
    graphics::plot(model, which = c(1, 2, 3, 5), main = model_name)
    files_written <<- c(files_written, path)
  }
  for (model_name in names(models)) {
    save_diagnostics(models[[model_name]], model_name)
  }

  # 9. Generate a plain-language report from the calculated results.
  format_p <- function(p) {
    if (is.na(p)) "NA" else if (p < 0.001) "<0.001" else sprintf("%.3f", p)
  }
  is_significant <- !is.na(app_results$p_holm_all_app_tests) &
    app_results$p_holm_all_app_tests < alpha
  robust_significant <- !is.na(robust_results$p_holm_all_app_tests_HC3) &
    robust_results$p_holm_all_app_tests_HC3 < alpha
  # Explicitly match sensitivity rows rather than relying on row ordering.
  match_key <- paste(app_results$outcome, app_results$term, sep = "::")
  robust_index <- match(
    match_key, paste(robust_results$outcome, robust_results$term, sep = "::")
  )
  disagreements <- which(is_significant != robust_significant[robust_index])
  report <- capture.output({
    cat("SIMULATED SOCIAL MEDIA AND HEALTH: ANALYSIS REPORT\n")
    cat("=================================================\n\n")
    cat("IMPORTANT: All data and imposed effects are fictional.\n")
    cat("These findings describe a simulation, not real-world health effects.\n\n")
    cat("1. DATA AND METHODS\n")
    cat("Participants: ", n, "; random seed: ", seed, ".\n", sep = "")
    cat("Age range: ", min(dat$age), "-", max(dat$age), " years; mean age: ",
        sprintf("%.1f", mean(dat$age)), ".\n", sep = "")
    cat("Missing values across the dataset: ", sum(missing_summary$missing_n),
        ".\n", sep = "")
    cat("Four multiple linear regressions include all five apps, age, and gender.\n")
    cat("Gender reference group: ", levels(dat$gender)[1L], ".\n", sep = "")
    cat("App coefficients describe change per additional hour/day, holding\n")
    cat("other included predictors constant. Higher health scores indicate more\n")
    cat("simulated problems; physical activity is measured in minutes/day.\n")
    cat("Primary significance uses Holm-adjusted p-values across all 20 app tests\n")
    cat("with alpha = ", alpha, ". Confidence intervals are pointwise 95% CIs.\n\n",
        sep = "")
    cat("2. MAIN FINDINGS\n")
    cat(sum(is_significant), " of 20 app associations meet the primary threshold.\n",
        sep = "")
    for (outcome in names(models)) {
      rows <- app_results[app_results$outcome == outcome, , drop = FALSE]
      fit <- model_fit[model_fit$outcome == outcome, , drop = FALSE]
      selected <- rows[!is.na(rows$p_holm_all_app_tests) &
                         rows$p_holm_all_app_tests < alpha, , drop = FALSE]
      cat("\n", outcome, "\n", sep = "")
      cat("Model n = ", fit$nobs, "; R-squared = ",
          sprintf("%.3f", fit$r.squared), "; adjusted R-squared = ",
          sprintf("%.3f", fit$adj.r.squared), ".\n", sep = "")
      cat("Overall model F-test p = ", format_p(fit$p.value),
          " (unadjusted; tests all predictors together).\n", sep = "")
      if (!nrow(selected)) {
        cat("No app association meets the Holm-adjusted threshold.\n")
        cat("This is not evidence that the true simulated association is zero.\n")
      } else {
        selected <- selected[order(selected$p_holm_all_app_tests), , drop = FALSE]
        for (i in seq_len(nrow(selected))) {
          r <- selected[i, ]
          unit <- if (outcome == "Physical activity") "minutes/day" else "score points"
          cat("- ", r$app, ": ", sprintf("%.3f", abs(r$estimate)), " ", unit,
              if (r$estimate >= 0) " higher" else " lower",
              " per additional hour/day; b = ", sprintf("%.3f", r$estimate),
              ", 95% CI [", sprintf("%.3f", r$conf.low), ", ",
              sprintf("%.3f", r$conf.high), "]; raw p = ", format_p(r$p.value),
              "; Holm p = ", format_p(r$p_holm_all_app_tests), ".\n", sep = "")
        }
      }
    }
    cat("\n3. HC3 ROBUST SENSITIVITY ANALYSIS\n")
    cat("HC3 keeps coefficients unchanged but recalculates standard errors,\n")
    cat("confidence intervals, and p-values. Holm adjustment again covers 20 tests.\n")
    cat(sum(robust_significant), " of 20 associations meet the HC3 threshold.\n",
        sep = "")
    if (!length(disagreements)) {
      cat("Primary and HC3 significance classifications agree for all app tests.\n")
    } else {
      cat("Classifications differ for:\n")
      for (i in disagreements) {
        r <- app_results[i, ]
        rr <- robust_results[robust_index[i], ]
        cat("- ", r$outcome, " / ", r$app,
            ": primary Holm p = ", format_p(r$p_holm_all_app_tests),
            "; HC3 Holm p = ", format_p(rr$p_holm_all_app_tests_HC3), ".\n",
            sep = "")
      }
    }
    cat("\n4. DIAGNOSTICS AND LIMITATIONS\n")
    cat("Largest adjusted GVIF: ", sprintf("%.3f", max(vif_results$adjusted_GVIF)),
        ". Review the complete VIF table; no single cutoff proves validity.\n", sep = "")
    cat("Adjusted GVIF is GVIF^(1/(2*Df)); for one-df terms it is sqrt(VIF).\n")
    cat("Diagnostic figures contain residuals vs fitted, Q-Q, scale-location,\n")
    cat("and residuals vs leverage panels. They require visual inspection;\n")
    cat("this report does not claim that regression assumptions are satisfied.\n")
    cat("Outcomes are bounded and rounded, and screen time is capped at 5 hours/app.\n")
    cat("Scores are illustrative, not validated diagnoses. Results are influenced\n")
    cat("by the simulation's imposed effects and random seed. No causal conclusions\n")
    cat("or generalisations to actual populations are justified.\n")
    cat("Raw coefficients across different outcome scales are not directly comparable.\n")
    cat("Unadjusted scatterplot lines differ from the adjusted regression estimates.\n")
    cat("\n5. COMPLETE APP COEFFICIENT TABLE\n")
    print(app_results, row.names = FALSE)
    cat("\n6. MODEL FIT TABLE\n")
    print(model_fit, row.names = FALSE)
    cat("\n7. DESCRIPTIVE STATISTICS\n")
    print(descriptives, row.names = FALSE)
    cat("\nGENDER COUNTS\n")
    print(gender_summary, row.names = FALSE)
    cat("\n8. FULL MODEL SUMMARIES\n")
    for (outcome in names(models)) {
      cat("\n", outcome, "\n", sep = "")
      print(summary(models[[outcome]]))
    }
  })
  save_text(report, "analysis_report.txt")
  save_text(capture.output(utils::sessionInfo()), "R_session_info.txt")
  # Manifest lists the files from this run, not any older/unrelated files.
  manifest_path <- file.path(output_folder, "output_manifest.csv")
  save_csv(data.frame(filename = basename(c(files_written, manifest_path))),
           "output_manifest.csv")
  if (verbose) {
    cat("\nANALYSIS COMPLETE\nSaved ", length(files_written),
        " files to: ", output_folder,
        "\nMain findings report: analysis_report.txt\n", sep = "")
  }

  invisible(list(
    data = dat,
    descriptives = descriptives,
    gender_summary = gender_summary,
    missing_data_summary = missing_summary,
    models = models,
    all_coefficients = tidy_results,
    app_results = app_results,
    model_fit = model_fit,
    robust_results = robust_results,
    vif_results = vif_results,
    app_correlations = cor_long,
    plots = plots,
    report_text = report,
    output_folder = output_folder,
    output_files = files_written
  ))
}
