# ============================================================
# SCREEN TIME, SLEEP, PLATFORM AND MENTAL HEALTH
# Original statistical analysis; two simplified reader-facing figures.
# Regular runnable script, NOT an analysis wrapper function.
# Outputs: screen_time_analysis_results, including analysis_report.txt.
# Associations are not causal. Review residual diagnostics for OLS suitability.
# ============================================================

# 1. PACKAGES AND DATA -----------------------------------------
required_packages <- c("tidyverse", "broom", "car")
for (pkg in required_packages) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
}
library(tidyverse)
library(broom)
library(car)
actualdata <- read.csv(paste0(
  "https://raw.githubusercontent.com/laura-gsm/pips-project/",
  "c075d4ef653730e296b8c88f17539d9b260b8d35/",
  "social_media_mental_health%202.csv"))
# Replace read.csv() above if using a local CSV.

# 2. VARIABLES AND PLATFORM PREPARATION ------------------------
df <- actualdata
social_vars <- "Daily_Screen_Time_Hours"
outcome_vars <- c("Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
all_vars <- c(social_vars, outcome_vars)
platform_var <- NULL
min_group_n <- 30
output_dir <- "screen_time_analysis_results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
missing_vars <- setdiff(all_vars, names(df))
if (length(missing_vars)) stop("Variables not found: ", paste(missing_vars, collapse = ", "))
non_numeric_vars <- all_vars[!vapply(df[all_vars], is.numeric, logical(1))]
if (length(non_numeric_vars)) {
  stop("Review nonnumeric variables: ", paste(non_numeric_vars, collapse = ", "),
       ". Do not automatically convert category codes to numeric values.")
}
if (is.null(platform_var)) {
  candidates <- grep("platform", names(df), ignore.case = TRUE, value = TRUE)
  if (length(candidates) != 1) {
    stop("Set platform_var manually. Candidates: ", paste(candidates, collapse = ", "))
  }
  platform_var <- candidates
}
message("Platform variable used: ", platform_var)
platform_raw <- str_squish(as.character(df[[platform_var]]))
platform_raw[platform_raw == ""] <- NA
if (any(str_detect(platform_raw, "[,;/|]"), na.rm = TRUE)) {
  warning("Several platforms may be listed per respondent. Each combination is treated as its own category.")
}
platform_table_raw <- sort(table(platform_raw), decreasing = TRUE)
keep_levels <- names(platform_table_raw)[platform_table_raw >= min_group_n]
if (length(keep_levels) < length(platform_table_raw)) {
  message("Platforms set to NA (fewer than ", min_group_n, " respondents): ",
          paste(setdiff(names(platform_table_raw), keep_levels), collapse = ", "))
}
df$platform <- factor(ifelse(platform_raw %in% keep_levels, platform_raw, NA), levels = keep_levels)
use_platform <- nlevels(df$platform) >= 2
ref_platform <- if (use_platform) levels(df$platform)[1] else NA_character_
if (use_platform) message("Reference platform: ", ref_platform) else {
  warning("Fewer than 2 usable platforms: platform analyses will be skipped.")
}
model_predictors <- if (use_platform) c(social_vars, "platform") else social_vars
write_csv(tibble(variable = c(all_vars, "platform"),
  variable_group = c("Predictor (numeric)", rep("Outcome", length(outcome_vars)),
                     "Predictor (categorical)")), file.path(output_dir, "variable_list.csv"))
write_csv(tibble(total_rows = nrow(df), total_columns = ncol(df)),
          file.path(output_dir, "dataset_information.csv"))
cat("\nDataset rows:", nrow(df), "\n")

# 3. MISSINGNESS AND DESCRIPTIVES -------------------------------
check_vars <- c(all_vars, "platform")
missing_summary <- tibble(variable = check_vars,
  missing_n = vapply(df[check_vars], function(x) sum(is.na(x)), numeric(1)),
  non_missing_n = vapply(df[check_vars], function(x) sum(!is.na(x)), numeric(1))) %>%
  mutate(total_n = nrow(df),
         missing_percent = if (nrow(df) > 0) 100 * missing_n / nrow(df) else NA_real_) %>%
  arrange(desc(missing_percent))
write_csv(missing_summary, file.path(output_dir, "missing_data_summary.csv"))
descriptive_stats <- map_dfr(all_vars, function(v) {
  x <- df[[v]]; z <- x[!is.na(x)]
  tibble(variable = v, n = length(z), missing_n = sum(is.na(x)),
    mean = if (length(z) > 0) mean(z) else NA_real_,
    sd = if (length(z) > 1) sd(z) else NA_real_,
    median = if (length(z) > 0) median(z) else NA_real_,
    min = if (length(z) > 0) min(z) else NA_real_,
    max = if (length(z) > 0) max(z) else NA_real_, unique_values = n_distinct(z))
})
write_csv(descriptive_stats, file.path(output_dir, "descriptive_statistics.csv"))

# 4. FREQUENCIES AND RESPONSE PERCENTAGES -----------------------
for (v in check_vars) {
  frequency_table <- df %>% transmute(response = .data[[v]]) %>%
    count(response, name = "frequency") %>%
    mutate(percentage = 100 * frequency / sum(frequency)) %>% arrange(response)
  safe_name <- gsub("[^A-Za-z0-9_]", "_", v)
  write_csv(frequency_table, file.path(output_dir, paste0("frequency_", safe_name, ".csv")))
}
distribution_data <- df %>% dplyr::select(all_of(all_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "response")
discrete_vars <- all_vars[vapply(all_vars, function(v) {
  n_distinct(df[[v]], na.rm = TRUE) <= 30
}, logical(1))]
response_percentages <- distribution_data %>%
  filter(!is.na(response), variable %in% discrete_vars) %>%
  count(variable, response, name = "frequency") %>% group_by(variable) %>%
  mutate(percentage = 100 * frequency / sum(frequency)) %>% ungroup()
write_csv(response_percentages, file.path(output_dir, "response_percentages.csv"))

# 5. SPEARMAN CORRELATIONS: ORIGINAL METHOD ---------------------
# Original small helper retained; the overall script is not a function.
safe_spearman <- function(data, x_var, y_var) {
  pair_data <- data %>% dplyr::select(all_of(c(x_var, y_var))) %>% drop_na()
  n_pair <- nrow(pair_data)
  test_result <- NULL
  if (n_pair >= 3 && n_distinct(pair_data[[x_var]]) >= 2 &&
      n_distinct(pair_data[[y_var]]) >= 2) {
    test_result <- tryCatch(cor.test(pair_data[[x_var]], pair_data[[y_var]],
      method = "spearman", exact = FALSE), error = function(e) NULL)
  }
  tibble(social_variable = x_var, outcome = y_var, n = n_pair,
    rho = if (is.null(test_result)) NA_real_ else unname(test_result$estimate),
    p_value = if (is.null(test_result)) NA_real_ else test_result$p.value)
}
cor_results <- expand_grid(social_variable = social_vars, outcome = outcome_vars) %>%
  mutate(result = map2(social_variable, outcome, ~ safe_spearman(df, .x, .y))) %>%
  dplyr::select(result) %>% unnest(result) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"),
    significant_adjusted = !is.na(p_adjusted) & p_adjusted < 0.05) %>% arrange(p_adjusted)
write_csv(cor_results, file.path(output_dir, "spearman_correlations.csv"))
cat("\nAssociations with BH-adjusted p < .05:",
    sum(cor_results$significant_adjusted, na.rm = TRUE), "\n")
all_correlations <- cor_results %>% filter(!is.na(p_adjusted), !is.na(rho)) %>%
  arrange(p_adjusted) %>% mutate(association = paste(social_variable, outcome, sep = " - "),
    association = factor(association, levels = rev(association)),
    significance_label = ifelse(p_adjusted < 0.05, "*", ""))
write_csv(all_correlations, file.path(output_dir, "all_correlations_ranked.csv"))

# 6. LINEAR REGRESSIONS: ORIGINAL MODELS AND INFERENCE -----------
# Complete cases per outcome; conventional CIs, unadjusted regression p-values.
regression_results_list <- list()
vif_results_list <- list()
diagnostics_results_list <- list()
model_summaries_list <- list()
regression_models <- list()
for (outcome_var in outcome_vars) {
  cat("\nRunning regression for:", outcome_var, "\n")
  model_vars <- c(outcome_var, model_predictors)
  model_data <- df %>% dplyr::select(all_of(model_vars)) %>% drop_na()
  n_model <- nrow(model_data)
  if (n_model < length(model_predictors) + 3 || n_distinct(model_data[[outcome_var]]) < 2) {
    warning("Skipping regression for ", outcome_var,
            ": insufficient complete cases or no outcome variation."); next
  }
  constant_predictors <- model_predictors[vapply(model_data[model_predictors],
    function(x) n_distinct(x) < 2, logical(1))]
  if (length(constant_predictors) > 0) {
    warning("Skipping regression for ", outcome_var, ": constant predictors: ",
            paste(constant_predictors, collapse = ", ")); next
  }
  regression_formula <- reformulate(model_predictors, response = outcome_var)
  model <- tryCatch(lm(regression_formula, data = model_data), error = function(e) {
    warning("Model failed for ", outcome_var, ": ", conditionMessage(e)); NULL
  })
  if (is.null(model)) next
  regression_models[[outcome_var]] <- model
  model_summary <- summary(model)
  tidy_model <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(outcome = outcome_var, n = n_model, r_squared = model_summary$r.squared,
           adjusted_r_squared = model_summary$adj.r.squared)
  regression_results_list[[outcome_var]] <- tidy_model
  safe_outcome_name <- gsub("[^A-Za-z0-9_]", "_", outcome_var)
  write_csv(tidy_model, file.path(output_dir, paste0("regression_", safe_outcome_name, ".csv")))
  model_summaries_list[[outcome_var]] <- tibble(outcome = outcome_var, n = n_model,
    r_squared = model_summary$r.squared, adjusted_r_squared = model_summary$adj.r.squared,
    residual_standard_error = model_summary$sigma,
    model_p_value = tryCatch(pf(model_summary$fstatistic[1], model_summary$fstatistic[2],
      model_summary$fstatistic[3], lower.tail = FALSE), error = function(e) NA_real_))
  model_rank <- model$rank
  number_of_coefficients <- length(coef(model))
  if (length(model_rank) == 1 && !is.na(model_rank) && model_rank == number_of_coefficients) {
    vif_output <- tryCatch(car::vif(model), error = function(e) {
      warning("VIF calculation failed for ", outcome_var, ": ", conditionMessage(e)); NULL
    })
    if (!is.null(vif_output)) {
      if (is.matrix(vif_output)) {
        vif_table <- as.data.frame(vif_output) %>% rownames_to_column("predictor")
        if ("GVIF" %in% names(vif_table)) {
          vif_table <- vif_table %>% mutate(adjusted_GVIF = GVIF^(1 / (2 * Df)))
        }
      } else vif_table <- tibble(predictor = names(vif_output), VIF = as.numeric(vif_output))
      vif_results_list[[outcome_var]] <- vif_table %>% mutate(outcome = outcome_var)
    }
  } else warning("VIF skipped for ", outcome_var, ": model rank deficient.")
  diagnostic_file <- file.path(output_dir,
    paste0("regression_diagnostics_", safe_outcome_name, ".png"))
  png(diagnostic_file, width = 1200, height = 1000, res = 130)
  par(mfrow = c(2, 2))
  plot(model, which = 1, main = "Residuals vs Fitted")
  plot(model, which = 2, main = "Normal Q-Q")
  plot(model, which = 3, main = "Scale-Location")
  plot(model, which = 5, main = "Residuals vs Leverage")
  dev.off()
  diagnostics_results_list[[outcome_var]] <- tibble(outcome = outcome_var, n = n_model,
    rank = model$rank, residual_sd = sd(residuals(model)), residual_mean = mean(residuals(model)),
    max_abs_standardized_residual = max(abs(rstandard(model)), na.rm = TRUE))
  capture.output(summary(model), file = file.path(output_dir,
    paste0("regression_summary_", safe_outcome_name, ".txt")))
}

# 7. COMBINED REGRESSION EXPORTS --------------------------------
if (length(regression_results_list) > 0) {
  regression_results <- bind_rows(regression_results_list)
  write_csv(regression_results, file.path(output_dir, "all_regression_results.csv"))
} else {
  regression_results <- tibble()
  warning("No regression models were successfully fitted.")
}
if (length(model_summaries_list) > 0) {
  model_summaries <- bind_rows(model_summaries_list)
  write_csv(model_summaries, file.path(output_dir, "regression_model_summaries.csv"))
}
if (length(vif_results_list) > 0) {
  vif_results <- bind_rows(vif_results_list)
  write_csv(vif_results, file.path(output_dir, "regression_vif_results.csv"))
}
if (length(diagnostics_results_list) > 0) {
  diagnostics_summary <- bind_rows(diagnostics_results_list)
  write_csv(diagnostics_summary, file.path(output_dir, "regression_diagnostics_summary.csv"))
}

# 8. PLATFORM ANALYSES: ORIGINAL TESTS AND CORRECTIONS -----------
if (use_platform) {
  dfp <- df %>% filter(!is.na(platform))
  platform_counts <- dfp %>% count(platform, name = "n") %>% mutate(percent = 100 * n / sum(n))
  write_csv(platform_counts, file.path(output_dir, "platform_counts.csv"))
  print(platform_counts)
  platform_descriptives <- dfp %>%
    pivot_longer(all_of(all_vars), names_to = "variable", values_to = "value") %>%
    group_by(platform, variable) %>%
    summarise(n = sum(!is.na(value)), mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE), median = median(value, na.rm = TRUE), .groups = "drop")
  write_csv(platform_descriptives, file.path(output_dir, "platform_descriptives.csv"))
  # Original effect-size formula H/(n-1); magnitude interpretation is contextual.
  platform_omnibus <- map_dfr(all_vars, function(v) {
    d <- dfp %>% dplyr::select(value = all_of(v), platform) %>% drop_na()
    kw <- kruskal.test(value ~ platform, data = d)
    tibble(variable = v, n = nrow(d), kruskal_H = unname(kw$statistic),
      df = unname(kw$parameter), p_value = kw$p.value,
      epsilon_squared = unname(kw$statistic) / (nrow(d) - 1))
  }) %>% mutate(p_adjusted = p.adjust(p_value, method = "BH"), significant_adjusted = p_adjusted < 0.05)
  write_csv(platform_omnibus, file.path(output_dir, "platform_kruskal_wallis.csv"))
  print(platform_omnibus)
  platform_pairwise <- map_dfr(all_vars, function(v) {
    d <- dfp %>% dplyr::select(value = all_of(v), platform) %>% drop_na()
    pw <- pairwise.wilcox.test(d$value, d$platform, p.adjust.method = "BH", exact = FALSE)
    broom::tidy(pw) %>% mutate(variable = v, .before = 1)
  })
  write_csv(platform_pairwise, file.path(output_dir, "platform_pairwise_wilcoxon.csv"))
  platform_interaction <- map_dfr(outcome_vars, function(y) {
    d <- dfp %>% dplyr::select(all_of(c(y, social_vars, "platform"))) %>% drop_na()
    base <- lm(reformulate(c(social_vars, "platform"), y), data = d)
    inter <- lm(reformulate(c(social_vars, "platform", paste0(social_vars, ":platform")), y), data = d)
    a <- anova(base, inter)
    tibble(outcome = y, p_interaction = a$`Pr(>F)`[2],
      r2_without_interaction = summary(base)$r.squared, r2_with_interaction = summary(inter)$r.squared)
  }) %>% mutate(p_interaction_adj = p.adjust(p_interaction, method = "BH"))
  write_csv(platform_interaction, file.path(output_dir, "platform_interaction_tests.csv"))
  print(platform_interaction)
} else message("Platform comparisons skipped: fewer than 2 usable platforms.")

# 9. FIGURE 1: THREE CLEAR OUTCOME TRENDS ------------------------
# Uses existing additive models, NOT new hypothesis tests. Predictions average
# over fixed platform proportions. Conventional pointwise mean CIs use the
# full coefficient covariance matrix. Restricted to overlapping observed
# screen-time ranges, which does not guarantee dense support throughout.
plot_labels <- c(Daily_Screen_Time_Hours = "Daily screen time (hours/day)",
  Sleep_Duration_Hours = "Sleep duration (hours/night)",
  GAD_7_Score = "Anxiety symptoms (GAD-7 points)",
  PHQ_9_Score = "Depressive symptoms (PHQ-9 points)")
plot_colors <- c(Sleep_Duration_Hours = "#0072B2", GAD_7_Score = "#D55E00", PHQ_9_Score = "#009E73")
reader_theme <- theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold", size = 15), strip.text = element_text(face = "bold", size = 10),
  plot.caption = element_text(hjust = 0, size = 9), panel.grid.minor = element_blank(), legend.position = "none")
trend_rows <- list()
for (y in names(regression_models)) {
  m <- regression_models[[y]]
  trend <- tryCatch({
    b <- coef(m)
    if (anyNA(b) || m$rank != length(b) || df.residual(m) <= 0) stop("Model not suitable for prediction bands.")
    mf <- model.frame(m)
    adjusted <- "platform" %in% names(mf)
    if (adjusted) {
      ranges <- lapply(split(mf[[social_vars]], droplevels(mf$platform)), range)
      lower <- max(vapply(ranges, function(x) x[1], numeric(1)))
      upper <- min(vapply(ranges, function(x) x[2], numeric(1)))
    } else { lower <- min(mf[[social_vars]]); upper <- max(mf[[social_vars]]) }
    if (!is.finite(lower) || !is.finite(upper) || upper <= lower) stop("No usable overlapping screen-time range.")
    grid <- seq(lower, upper, length.out = 100)
    prediction_terms <- delete.response(terms(m))
    if (adjusted) {
      counts <- table(mf$platform)
      groups <- names(counts)[counts > 0]
      weights <- as.numeric(counts[groups]) / sum(counts)
      newdata <- expand.grid(screen_value = grid, platform = groups, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      names(newdata)[1] <- social_vars
      newdata$platform <- factor(newdata$platform, levels = m$xlevels$platform)
      design <- model.matrix(prediction_terms, newdata, contrasts.arg = m$contrasts, xlev = m$xlevels)
      averaged_design <- matrix(0, nrow = length(grid), ncol = ncol(design), dimnames = list(NULL, colnames(design)))
      for (j in seq_along(groups)) {
        rows <- ((j - 1) * length(grid) + 1):(j * length(grid))
        averaged_design <- averaged_design + weights[j] * design[rows, , drop = FALSE]
      }
    } else averaged_design <- model.matrix(prediction_terms, setNames(data.frame(grid), social_vars))
    averaged_design <- averaged_design[, names(b), drop = FALSE]
    covariance <- vcov(m)[names(b), names(b), drop = FALSE]
    estimate <- as.numeric(averaged_design %*% b)
    standard_error <- sqrt(pmax(0, rowSums((averaged_design %*% covariance) * averaged_design)))
    critical <- qt(0.975, df = df.residual(m))
    interaction_flag <- use_platform && any(platform_interaction$outcome == y &
      !is.na(platform_interaction$p_interaction_adj) & platform_interaction$p_interaction_adj < 0.05)
    tibble(outcome = y, screen_hours = grid, estimated_mean = estimate,
      lower_ci = estimate - critical * standard_error, upper_ci = estimate + critical * standard_error,
      panel = paste0(plot_labels[[y]], "\nModel n = ", nrow(mf),
        if (adjusted) " | platform-adjusted" else " | unadjusted",
        if (interaction_flag) "\nDifferent slopes: review interaction results" else ""))
  }, error = function(e) { warning("Trend panel skipped for ", y, ": ", conditionMessage(e)); NULL })
  if (!is.null(trend)) trend_rows[[y]] <- trend
}
trend_data <- bind_rows(trend_rows)
if (nrow(trend_data) > 0) {
  trend_data$panel <- factor(trend_data$panel, levels = unique(trend_data$panel))
  screen_time_trend_plot <- ggplot(trend_data, aes(screen_hours, estimated_mean, colour = outcome, fill = outcome)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.18, colour = NA) + geom_line(linewidth = 1.2) +
    facet_wrap(~ panel, ncol = 3, scales = "free") +
    scale_colour_manual(values = plot_colors) + scale_fill_manual(values = plot_colors) +
    labs(title = "How are screen time, sleep and mental health related?",
      subtitle = "Estimated mean outcomes from the existing additive linear models",
      x = "Daily screen time (hours/day)", y = "Estimated mean (panel-specific units)",
      caption = paste("Shading: pointwise 95% confidence intervals for mean outcomes, not individual predictions.",
        "Platform composition is held constant. Panels use their own scales and common observed screen-time ranges.",
        "Higher anxiety/depression scores indicate more symptoms. Associations are not causal.", sep = "\n")) + reader_theme
  print(screen_time_trend_plot)
  ggsave(file.path(output_dir, "01_screen_time_outcome_trends.png"), plot = screen_time_trend_plot, width = 13, height = 6, dpi = 300)
}

# 10. FIGURE 2: COMPACT PLATFORM DISTRIBUTIONS ------------------
# Descriptive, unadjusted; outlier markers hidden, observations retained.
if (use_platform) {
  plot_counts <- sort(table(dfp$platform), decreasing = TRUE)
  plot_groups <- names(plot_counts)
  group_labels <- setNames(paste0(plot_groups, " (n = ", as.integer(plot_counts), ")"), plot_groups)
  box_data <- dfp %>% dplyr::select(platform, all_of(all_vars)) %>%
    pivot_longer(all_of(all_vars), names_to = "variable", values_to = "value") %>%
    mutate(panel = factor(unname(plot_labels[variable]), levels = unname(plot_labels[all_vars])))
  platform_boxplot <- ggplot(box_data, aes(x = value, y = platform)) +
    geom_boxplot(orientation = "y", outlier.shape = NA, na.rm = TRUE, width = 0.55, fill = "#DCEAF3", colour = "#28536B") +
    facet_wrap(~ panel, scales = "free_x", ncol = 2) + scale_y_discrete(limits = rev(plot_groups), labels = group_labels) +
    labs(title = "How do screen time and health outcomes vary by platform?",
      subtitle = "Unadjusted distributions; platforms ordered by respondent count",
      x = "Observed value (panel-specific units)", y = NULL,
      caption = paste("Centre line: median. Box: middle 50%. Whiskers: observed values within 1.5 IQR.",
        "Outlying points are hidden, not excluded. Label n is platform total; available n may differ by outcome.",
        "Only retained platforms are shown. These comparisons are descriptive, not adjusted or causal.", sep = "\n")) + reader_theme
  print(platform_boxplot)
  ggsave(file.path(output_dir, "02_platform_comparisons.png"), plot = platform_boxplot,
         width = 13, height = max(7.5, 3.5 + 0.7 * length(plot_groups)), dpi = 300)
}

# 11. NEW: OVERALL STATISTICAL FINDINGS REPORT -------------------
# Reporting only: no tests, models, p-values, or plots are changed.
# All values come from the current run. Use list lengths/use_platform to
# avoid accidentally reporting old objects left in the R environment.
# These small formatting helpers do NOT wrap the analysis in a function.
report_number <- function(x) {
  if (length(x) != 1 || is.na(x)) "NA" else sprintf("%.3f", x)
}
report_p <- function(x) {
  if (length(x) != 1 || is.na(x)) "NA" else if (x < 0.001) "<0.001" else sprintf("%.3f", x)
}
report_table <- function(x) {
  if (nrow(x) > 0) print(as.data.frame(x), row.names = FALSE) else cat("No available results.\n")
}
analysis_report <- capture.output({
  cat("SCREEN TIME, SLEEP AND MENTAL HEALTH: MAIN STATISTICAL FINDINGS\n")
  cat("=============================================================\n\n")
  cat("1. SAMPLE AND DESCRIPTIVE RESULTS\n")
  cat("Total respondents: ", nrow(df), ".\n", sep = "")
  cat("Platform column: ", platform_var, "; minimum platform total: ", min_group_n, ".\n", sep = "")
  cat("Retained platform categories: ", length(keep_levels), ".\n", sep = "")
  cat("Originally missing/blank platforms: ", sum(is.na(platform_raw)),
      "; respondents assigned NA for rare platforms: ",
      sum(!is.na(platform_raw) & !(platform_raw %in% keep_levels)), ".\n", sep = "")
  if (use_platform) cat("Regression reference platform: ", ref_platform, ".\n", sep = "")
  cat("Descriptives and correlations use available responses in the full dataset.\n")
  cat("Platform-adjusted analyses use retained groups only. Complete-case sample\n")
  cat("sizes may differ between outcomes. Platform NA includes filtered categories.\n\n")
  report_table(descriptive_stats)
  cat("\nMISSINGNESS\n"); report_table(missing_summary)
  if (use_platform) { cat("\nRETAINED PLATFORM COUNTS\n"); report_table(platform_counts) }

  cat("\n2. SCREEN-TIME CORRELATIONS\n")
  cat("Spearman measures unadjusted monotonic associations, not causal effects.\n")
  cat("BH adjustment covers the three correlations.\n")
  cat("Associations meeting BH-adjusted p < .05: ", sum(cor_results$significant_adjusted, na.rm = TRUE), " of 3.\n", sep = "")
  for (i in seq_len(nrow(cor_results))) {
    r <- cor_results[i, ]
    cat("- ", plot_labels[[r$outcome]], ": n = ", r$n, "; rho = ", report_number(r$rho),
        "; raw p = ", report_p(r$p_value), "; BH p = ", report_p(r$p_adjusted), ". ",
        if (is.na(r$p_adjusted)) "Not estimable." else if (r$p_adjusted < .05)
          "Meets the adjusted threshold." else "Does not meet the adjusted threshold.", "\n", sep = "")
  }

  cat("\n3. LINEAR REGRESSION FINDINGS\n")
  cat(if (use_platform) "Models include screen time and platform.\n" else
      "Fewer than two retained platforms: models use screen time alone.\n")
  cat("Regression coefficient and overall-model p-values are NOT corrected for\n")
  cat("multiple testing. Confidence intervals are conventional pointwise 95% CIs.\n")
  if (length(regression_results_list) == 0) cat("No regression models were successfully fitted.\n")
  for (y in names(regression_results_list)) {
    coefs <- regression_results_list[[y]]
    fit <- model_summaries_list[[y]]
    slope <- coefs %>% filter(term == social_vars)
    cat("\n", plot_labels[[y]], "\n", sep = "")
    cat("n = ", fit$n, "; R-squared = ", report_number(fit$r_squared),
        "; adjusted R-squared = ", report_number(fit$adjusted_r_squared),
        "; overall model p = ", report_p(fit$model_p_value), ".\n", sep = "")
    if (nrow(slope) == 1) {
      cat("Screen-time coefficient per additional hour/day: b = ", report_number(slope$estimate),
          "; 95% CI [", report_number(slope$conf.low), ", ", report_number(slope$conf.high),
          "]; unadjusted p = ", report_p(slope$p.value), ".\n", sep = "")
      if (is.finite(slope$estimate)) {
        cat("Estimated association: ", report_number(abs(slope$estimate)),
            if (y == "Sleep_Duration_Hours") " hours/night" else " score points",
            if (slope$estimate >= 0) " higher" else " lower",
            " per additional screen-time hour/day",
            if (use_platform) ", holding platform constant.\n" else ".\n", sep = "")
      }
      cat(if (is.na(slope$p.value)) "Coefficient inference unavailable.\n" else if (slope$p.value < .05)
        "Meets the UNADJUSTED .05 threshold; multiple-testing caution applies.\n" else
        "Does not meet the unadjusted .05 threshold; this does not establish no association.\n")
    }
  }
  skipped_outcomes <- setdiff(outcome_vars, names(regression_results_list))
  if (length(skipped_outcomes)) cat("Regressions skipped/failed: ", paste(skipped_outcomes, collapse = ", "), ".\n", sep = "")

  cat("\n4. PLATFORM COMPARISONS\n")
  if (use_platform) {
    cat("Kruskal-Wallis tests rank/distribution differences, not necessarily median\n")
    cat("differences when group distribution shapes differ. These tests are unadjusted\n")
    cat("for screen time and other covariates. BH correction covers four omnibus tests.\n")
    for (i in seq_len(nrow(platform_omnibus))) {
      r <- platform_omnibus[i, ]
      cat("- ", plot_labels[[r$variable]], ": n = ", r$n, "; H = ", report_number(r$kruskal_H),
          "; df = ", r$df, "; BH p = ", report_p(r$p_adjusted),
          "; epsilon_squared (H/(n-1)) = ", report_number(r$epsilon_squared), ".\n", sep = "")
    }
    significant_groups <- platform_omnibus %>% filter(!is.na(p_adjusted), p_adjusted < .05)
    cat("Variables meeting the adjusted threshold: ",
        if (nrow(significant_groups)) paste(unname(plot_labels[significant_groups$variable]), collapse = "; ") else "none among estimable tests",
        ".\n", sep = "")
    cat("\nPairwise Wilcoxon results: p.value is ALREADY BH-adjusted within each variable.\n")
    significant_pairs <- platform_pairwise %>% filter(!is.na(p.value), p.value < .05)
    cat("Pairs meeting the within-variable adjusted threshold: ", nrow(significant_pairs), ".\n", sep = "")
    if (nrow(significant_pairs)) report_table(significant_pairs)
    cat("These pairwise corrections do not correct across every variable combined.\n")

    cat("\n5. DOES THE SCREEN-TIME SLOPE DIFFER BY PLATFORM?\n")
    cat("Nested-model F-tests; BH adjustment across three outcomes.\n")
    report_table(platform_interaction)
    significant_interactions <- platform_interaction %>%
      filter(!is.na(p_interaction_adj), p_interaction_adj < .05)
    if (nrow(significant_interactions)) {
      cat("Evidence of differing slopes for: ",
          paste(unname(plot_labels[significant_interactions$outcome]), collapse = "; "), ".\n", sep = "")
      cat("The common-slope trend figure is incomplete for these outcomes; examine\n")
      cat("platform-specific slopes before drawing substantive conclusions.\n")
    } else cat("No estimable interaction meets the adjusted threshold; this does not prove equal slopes.\n")
  } else cat("Platform comparisons and interactions skipped: fewer than two retained platforms.\n")

  cat("\n6. INTERPRETATION AND LIMITATIONS\n")
  cat("Figure 1 displays estimated means from the existing additive models, not\n")
  cat("individual outcomes. Shading is uncertainty in means, not a prediction interval.\n")
  cat("Figure 2 displays observed unadjusted distributions; box overlap is not a test.\n")
  cat("Regression diagnostics require visual review for linearity, residual variance,\n")
  cat("normality and influential points. This report does not certify assumptions.\n")
  cat("GAD-7 and PHQ-9 are bounded symptom scores; OLS may be unsuitable in some data.\n")
  cat("Independent observations are assumed. Missingness, rare-platform filtering,\n")
  cat("and unmeasured confounding can bias results. Models do not adjust for age/gender.\n")
  cat("Statistical significance need not imply practical importance. Non-significance\n")
  cat("does not demonstrate absence of an association. No causal conclusions are justified.\n")

  cat("\n7. COMPLETE RESULTS TABLES\n")
  cat("\nCORRELATIONS\n"); report_table(cor_results)
  if (length(regression_results_list)) {
    cat("\nREGRESSION COEFFICIENTS\n"); report_table(bind_rows(regression_results_list))
    cat("\nMODEL FIT\n"); report_table(bind_rows(model_summaries_list))
  }
  if (length(vif_results_list)) {
    cat("\nVIF/GVIF\n"); report_table(bind_rows(vif_results_list))
    cat("Adjusted GVIF = GVIF^(1/(2*Df)); for one-df terms, this is sqrt(VIF).\n")
  }
  if (length(diagnostics_results_list)) {
    cat("\nRESIDUAL DIAGNOSTICS\n"); report_table(bind_rows(diagnostics_results_list))
  }
  if (use_platform) {
    cat("\nPLATFORM DESCRIPTIVES\n"); report_table(platform_descriptives)
    cat("\nKRUSKAL-WALLIS\n"); report_table(platform_omnibus)
    cat("\nALL PAIRWISE TESTS (ADJUSTED P-VALUES)\n"); report_table(platform_pairwise)
  }
})
report_file <- file.path(output_dir, "analysis_report.txt")
writeLines(analysis_report, con = report_file)

# 12. SESSION EXPORT AND COMPLETION ----------------------------
capture.output(sessionInfo(), file = file.path(output_dir, "session_information.txt"))
cat("\nANALYSIS COMPLETED\nResults folder:", normalizePath(output_dir), "\n")
cat("Main findings report:", report_file, "\n")
cat("Main figures: 01_screen_time_outcome_trends.png, 02_platform_comparisons.png\n")
cat("A figure/panel may be skipped if its data/model is unsuitable.\n")
cat("Original statistical tables, regression summaries and diagnostics retained.\n")
# Older output files are not deleted automatically. Matching filenames are
# overwritten. Report is generated only when execution reaches section 11.
