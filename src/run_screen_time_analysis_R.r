# Save as src/run_screen_time_analysis.R.
# Sourcing this file defines the function; it does not execute the analysis.
# Runs analysis on the new data that was used. 

#' Run the screen-time, sleep, platform and mental-health analysis
#'
#' @param data Original data frame, or NULL to read a CSV from data_path.
#' @param data_path CSV path or URL, ignored when data is supplied.
#' @param platform_var Platform column name. NULL detects a single column
#'   containing 'platform' in its name. Supply original, not filtered data.
#' @param min_group_n Minimum total respondents for a retained platform.
#' @param output_dir Folder for CSVs, PNGs, regression summaries and report.
#' @param print_plots Display the two main figures in main.Rmd or RStudio.
#'   Figures are saved regardless. Diagnostics are saved, not displayed.
#' @param verbose Display selected tables and completion messages.
#'
#' @return Invisibly returns prepared data, statistical tables, fitted models,
#'   plot objects, report text, analysis notes and paths of files written.
#'
#' @details
#' Retains pairwise-complete Spearman tests (exact=FALSE), BH across three
#' correlations; additive OLS models with screen time and platform when >=2
#' platforms are retained; conventional 95% CIs and UNADJUSTED regression
#' p-values; Kruskal-Wallis tests with BH across four variables; pairwise
#' Wilcoxon tests (exact=FALSE) with BH within each variable; nested-model
#' interaction F-tests with BH across three outcomes. H/(n-1) is retained
#' as the original Kruskal-Wallis effect-size calculation.
#'
#' Rare platforms become NA, not deleted rows: full-sample descriptives and
#' correlations can include them, but platform models cannot. Model samples
#' use complete cases. The most common retained platform is the reference.
#' Platform combinations remain categories, not separate app exposures.
#'
#' Figure 1 uses existing additive models, averaging over fixed platform
#' proportions in each model sample, with pointwise mean confidence bands
#' using the full coefficient covariance matrix. Display is restricted to
#' the common observed screen-time range. Figure 2 is descriptive/unadjusted.
#' Significant interactions make a single common slope incomplete.
#'
#' Extra execution guards record failed analyses as NA/skipped with notes;
#' they do not replace tests or alter successful model estimates.
#' Dependencies: ggplot2, dplyr, tidyr, purrr, stringr, tibble, readr, broom,
#' car. Install before knitting, e.g. install.packages(c('tidyverse','broom','car')).
#' Does not attach packages, install software, change directories, or call View().
#' Existing matching outputs are overwritten; old/unrelated files are not deleted.
#' Returns only files from the current run. No causal inference is justified.
#'
#' @examples
#' # main.Rmd in the project root:
#' # source('src/run_screen_time_analysis.R')
#' # results <- run_screen_time_analysis(verbose = FALSE)
#' # knitr::kable(results$correlations, digits = 3)
#' # Or use an original data frame already loaded by main.Rmd:
#' # results <- run_screen_time_analysis(data = actualdata, verbose = FALSE)
run_screen_time_analysis <- function(
    data = NULL,
    data_path = paste0('https://raw.githubusercontent.com/laura-gsm/pips-project/',
                      'c075d4ef653730e296b8c88f17539d9b260b8d35/',
                      'social_media_mental_health%202.csv'),
    platform_var = NULL,
    min_group_n = 30,
    output_dir = 'screen_time_analysis_results',
    print_plots = TRUE,
    verbose = TRUE) {

  # 1. Dependencies, arguments and data.
  packages <- c('ggplot2','dplyr','tidyr','purrr','stringr','tibble','readr','broom','car')
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) stop('Install required packages before running: ',
                                    paste(missing_packages, collapse = ', '), call. = FALSE)
  if (!is.numeric(min_group_n) || length(min_group_n) != 1L ||
      !is.finite(min_group_n) || min_group_n < 1 || min_group_n != floor(min_group_n))
    stop('min_group_n must be a positive integer.', call. = FALSE)
  for (value in list(print_plots, verbose)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop('print_plots and verbose must each be TRUE or FALSE.', call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) stop('Invalid output_dir.', call. = FALSE)
  if (is.null(data)) {
    if (!is.character(data_path) || length(data_path) != 1L || is.na(data_path) || !nzchar(data_path))
      stop('data_path must be a CSV path or URL.', call. = FALSE)
    data <- tryCatch(utils::read.csv(data_path, stringsAsFactors = FALSE),
      error = function(e) stop('Could not read dataset: ', conditionMessage(e), call. = FALSE))
  }
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)))
    stop('data must be a nonempty data frame with unique column names.', call. = FALSE)
  df <- as.data.frame(data)
  screen <- 'Daily_Screen_Time_Hours'
  outcomes <- c('Sleep_Duration_Hours','GAD_7_Score','PHQ_9_Score')
  variables <- c(screen, outcomes)
  labels <- c(Daily_Screen_Time_Hours = 'Daily screen time (hours/day)',
              Sleep_Duration_Hours = 'Sleep duration (hours/night)',
              GAD_7_Score = 'Anxiety symptoms (GAD-7 points)',
              PHQ_9_Score = 'Depressive symptoms (PHQ-9 points)')
  absent <- setdiff(variables, names(df))
  if (length(absent)) stop('Missing columns: ', paste(absent, collapse = ', '), call. = FALSE)
  for (v in variables) {
    if (!is.numeric(df[[v]]) || any(!is.na(df[[v]]) & !is.finite(df[[v]])))
      stop(v, ' must contain finite numeric values or NA. Review its coding.', call. = FALSE)
  }
  if (is.null(platform_var)) {
    candidates <- grep('platform', names(df), ignore.case = TRUE, value = TRUE)
    if (length(candidates) != 1L) stop('Specify platform_var; detected: ',
                                       paste(candidates, collapse = ', '), call. = FALSE)
    platform_var <- candidates
  }
  if (!is.character(platform_var) || length(platform_var) != 1L ||
      is.na(platform_var) || !platform_var %in% names(df))
    stop('platform_var must identify an existing column.', call. = FALSE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!dir.exists(output_dir)) stop('Could not create output_dir.', call. = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  output_files <- character(); notes <- character()
  add_note <- function(text) {
    notes <<- c(notes, text)
    if (verbose) message(text)
  }
  save_csv <- function(x, filename) {
    path <- file.path(output_dir, filename)
    readr::write_csv(tibble::as_tibble(x), path)
    output_files <<- c(output_files, path)
  }
  save_text <- function(x, filename) {
    path <- file.path(output_dir, filename)
    writeLines(x, path)
    output_files <<- c(output_files, path)
  }
  save_plot <- function(x, filename, width, height) {
    path <- file.path(output_dir, filename)
    ggplot2::ggsave(path, plot = x, width = width, height = height, dpi = 300)
    output_files <<- c(output_files, path)
    if (print_plots) print(x)
  }
  complete <- function(cols) {
    x <- df[cols]
    x[stats::complete.cases(x), , drop = FALSE]
  }
  bind <- function(rows) as.data.frame(dplyr::bind_rows(rows))

  # 2. Original platform cleaning, filtering and reference selection.
  platform_raw <- stringr::str_squish(as.character(df[[platform_var]]))
  platform_raw[!is.na(platform_raw) & platform_raw == ''] <- NA_character_
  if (any(stringr::str_detect(platform_raw, '[,;/|]'), na.rm = TRUE))
    add_note('Platform combinations are treated as categories, not separate app exposures.')
  raw_counts <- sort(table(platform_raw), decreasing = TRUE)
  retained <- names(raw_counts)[raw_counts >= min_group_n]
  df$platform <- factor(ifelse(platform_raw %in% retained, platform_raw, NA), levels = retained)
  use_platform <- nlevels(df$platform) >= 2L
  reference <- if (use_platform) levels(df$platform)[1L] else NA_character_
  predictors <- if (use_platform) c(screen, 'platform') else screen
  if (!use_platform) add_note('Fewer than two retained platforms: regressions use screen time only; platform tests skipped.')
  save_csv(data.frame(variable = c(variables, 'platform'),
    variable_group = c('Predictor (numeric)', rep('Outcome', 3), 'Predictor (categorical)')),
    'variable_list.csv')
  save_csv(data.frame(total_rows = nrow(df), total_columns = ncol(df)), 'dataset_information.csv')

  # 3. Missingness, descriptives, frequencies and response percentages.
  check <- c(variables, 'platform')
  missing_summary <- data.frame(variable = check,
    missing_n = vapply(df[check], function(x) sum(is.na(x)), numeric(1)),
    non_missing_n = vapply(df[check], function(x) sum(!is.na(x)), numeric(1)), total_n = nrow(df))
  missing_summary$missing_percent <- 100 * missing_summary$missing_n / nrow(df)
  missing_summary <- missing_summary[order(-missing_summary$missing_percent), ]
  descriptive_stats <- bind(lapply(variables, function(v) {
    x <- df[[v]]; z <- x[!is.na(x)]; n <- length(z)
    data.frame(variable = v, n = n, missing_n = sum(is.na(x)),
      mean = if (n) mean(z) else NA_real_, sd = if (n > 1) stats::sd(z) else NA_real_,
      median = if (n) stats::median(z) else NA_real_, min = if (n) min(z) else NA_real_,
      max = if (n) max(z) else NA_real_, unique_values = length(unique(z)))
  }))
  save_csv(missing_summary, 'missing_data_summary.csv')
  save_csv(descriptive_stats, 'descriptive_statistics.csv')
  frequencies <- setNames(lapply(check, function(v) {
    x <- as.data.frame(dplyr::count(data.frame(response = df[[v]]), response, name = 'frequency'))
    x$percentage <- 100 * x$frequency / sum(x$frequency)
    x <- x[order(x$response, na.last = TRUE), ]
    save_csv(x, paste0('frequency_', v, '.csv'))
    x
  }), check)
  distribution <- tidyr::pivot_longer(tibble::as_tibble(df[variables]),
    cols = tidyr::everything(), names_to = 'variable', values_to = 'response')
  discrete <- variables[vapply(df[variables], function(x) length(unique(x[!is.na(x)])) <= 30L, logical(1))]
  percentages <- dplyr::filter(distribution, !is.na(response), variable %in% discrete)
  percentages <- dplyr::count(percentages, variable, response, name = 'frequency')
  percentages <- dplyr::group_by(percentages, variable)
  percentages <- dplyr::ungroup(dplyr::mutate(percentages, percentage = 100 * frequency / sum(frequency)))
  save_csv(percentages, 'response_percentages.csv')

  # 4. Original Spearman tests and correction.
  correlations <- bind(lapply(outcomes, function(y) {
    d <- complete(c(screen, y)); test <- NULL
    if (nrow(d) >= 3L && length(unique(d[[screen]])) >= 2L && length(unique(d[[y]])) >= 2L)
      test <- tryCatch(stats::cor.test(d[[screen]], d[[y]], method = 'spearman', exact = FALSE),
                       error = function(e) NULL)
    if (is.null(test)) add_note(paste('Spearman test not estimable for', y))
    data.frame(social_variable = screen, outcome = y, n = nrow(d),
      rho = if (is.null(test)) NA_real_ else unname(test$estimate),
      p_value = if (is.null(test)) NA_real_ else test$p.value)
  }))
  correlations$p_adjusted <- stats::p.adjust(correlations$p_value, 'BH')
  correlations$significant_adjusted <- !is.na(correlations$p_adjusted) & correlations$p_adjusted < .05
  correlations <- correlations[order(correlations$p_adjusted, na.last = TRUE), ]
  save_csv(correlations, 'spearman_correlations.csv')
  ranked <- correlations[!is.na(correlations$p_adjusted) & !is.na(correlations$rho), ]
  ranked$association <- paste(ranked$social_variable, ranked$outcome, sep = ' - ')
  ranked$association <- factor(ranked$association, levels = rev(ranked$association))
  ranked$significance_label <- ifelse(ranked$p_adjusted < .05, '*', '')
  save_csv(ranked, 'all_correlations_ranked.csv')

  # 5. Original additive OLS regressions, diagnostics and exports.
  models <- coefficient_rows <- fit_rows <- vif_rows <- diagnostic_rows <- list()
  save_diagnostics <- function(m, y) {
    path <- file.path(output_dir, paste0('regression_diagnostics_', y, '.png'))
    grDevices::png(path, width = 1200, height = 1000, res = 130)
    device <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(which = device), add = TRUE)
    graphics::par(mfrow = c(2, 2))
    for (i in c(1, 2, 3, 5)) graphics::plot(m, which = i)
    output_files <<- c(output_files, path)
  }
  for (y in outcomes) {
    d <- complete(c(y, predictors))
    if (nrow(d) < length(predictors) + 3L ||
        any(vapply(d[c(y, predictors)], function(x) length(unique(x)) < 2L, logical(1)))) {
      add_note(paste('Regression skipped for', y, ': insufficient complete cases or variation.')); next
    }
    m <- tryCatch(stats::lm(stats::reformulate(predictors, response = y), data = d),
      error = function(e) { add_note(paste('Regression failed for', y, conditionMessage(e))); NULL })
    if (is.null(m)) next
    if (stats::df.residual(m) <= 0) { add_note(paste('No residual degrees of freedom for', y)); next }
    models[[y]] <- m; s <- summary(m)
    tab <- as.data.frame(broom::tidy(m, conf.int = TRUE))
    tab$outcome <- y; tab$n <- nrow(d)
    tab$r_squared <- s$r.squared; tab$adjusted_r_squared <- s$adj.r.squared
    coefficient_rows[[y]] <- tab
    save_csv(tab, paste0('regression_', y, '.csv'))
    save_text(capture.output(summary(m)), paste0('regression_summary_', y, '.txt'))
    f <- s$fstatistic
    fit_rows[[y]] <- data.frame(outcome = y, n = nrow(d), r_squared = s$r.squared,
      adjusted_r_squared = s$adj.r.squared, residual_standard_error = s$sigma,
      model_p_value = if (is.null(f)) NA_real_ else unname(stats::pf(f[1], f[2], f[3], lower.tail = FALSE)))
    if (m$rank == length(stats::coef(m))) {
      v <- tryCatch(car::vif(m), error = function(e) {
        add_note(paste('VIF unavailable for', y, ':', conditionMessage(e))); NULL })
      if (!is.null(v)) {
        if (is.matrix(v)) {
          vt <- data.frame(predictor = rownames(v), as.data.frame(v), row.names = NULL)
          vt$adjusted_GVIF <- vt$GVIF^(1 / (2 * vt$Df))
        } else vt <- data.frame(predictor = names(v), VIF = as.numeric(v))
        vt$outcome <- y; vif_rows[[y]] <- vt
      }
    } else add_note(paste('Rank-deficient model for', y, ': VIF and trend panel unavailable.'))
    tryCatch(save_diagnostics(m, y), error = function(e)
      add_note(paste('Diagnostic image failed for', y, conditionMessage(e))))
    diagnostic_rows[[y]] <- data.frame(outcome = y, n = nrow(d), rank = m$rank,
      residual_sd = stats::sd(stats::residuals(m)), residual_mean = mean(stats::residuals(m)),
      max_abs_standardized_residual = max(abs(stats::rstandard(m)), na.rm = TRUE))
  }
  regression_results <- bind(coefficient_rows); model_summaries <- bind(fit_rows)
  vif_results <- bind(vif_rows); diagnostics_summary <- bind(diagnostic_rows)
  if (length(coefficient_rows)) save_csv(regression_results, 'all_regression_results.csv')
  if (length(fit_rows)) save_csv(model_summaries, 'regression_model_summaries.csv')
  if (length(vif_rows)) save_csv(vif_results, 'regression_vif_results.csv')
  if (length(diagnostic_rows)) save_csv(diagnostics_summary, 'regression_diagnostics_summary.csv')

  # 6. Original platform comparisons and interaction tests.
  platform_counts <- platform_descriptives <- omnibus <- pairwise <- interactions <- data.frame()
  if (use_platform) {
    dfp <- df[!is.na(df$platform), ]
    counts <- table(dfp$platform)
    platform_counts <- data.frame(platform = names(counts), n = as.integer(counts),
                                  percent = 100 * as.integer(counts) / sum(counts))
    platform_descriptives <- bind(lapply(levels(dfp$platform), function(g) {
      bind(lapply(variables, function(v) {
        x <- dfp[as.character(dfp$platform) == g, v]
        data.frame(platform = g, variable = v, n = sum(!is.na(x)),
          mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
          median = stats::median(x, na.rm = TRUE))
      }))
    }))
    pair_rows <- list()
    omnibus <- bind(lapply(variables, function(v) {
      d <- complete(c(v, 'platform')); d$platform <- droplevels(d$platform)
      kw <- tryCatch(stats::kruskal.test(stats::reformulate('platform', response = v), data = d),
                     error = function(e) NULL)
      if (is.null(kw) || !is.finite(kw$p.value)) {
        kw <- NULL; add_note(paste('Kruskal-Wallis not estimable for', v))
      }
      data.frame(variable = v, n = nrow(d),
        kruskal_H = if (is.null(kw)) NA_real_ else unname(kw$statistic),
        df = if (is.null(kw)) NA_real_ else unname(kw$parameter),
        p_value = if (is.null(kw)) NA_real_ else kw$p.value,
        epsilon_squared = if (is.null(kw)) NA_real_ else unname(kw$statistic) / (nrow(d) - 1))
    }))
    omnibus$p_adjusted <- stats::p.adjust(omnibus$p_value, 'BH')
    omnibus$significant_adjusted <- omnibus$p_adjusted < .05
    for (v in variables) {
      d <- complete(c(v, 'platform')); d$platform <- droplevels(d$platform)
      pw <- tryCatch(stats::pairwise.wilcox.test(d[[v]], d$platform,
        p.adjust.method = 'BH', exact = FALSE), error = function(e) NULL)
      if (is.null(pw)) { add_note(paste('Pairwise Wilcoxon unavailable for', v)); next }
      tab <- as.data.frame(broom::tidy(pw)); tab$variable <- rep(v, nrow(tab))
      pair_rows[[v]] <- tab[c('variable','group1','group2','p.value')]
    }
    pairwise <- if (length(pair_rows)) bind(pair_rows) else
      data.frame(variable = character(), group1 = character(), group2 = character(), p.value = numeric())
    interactions <- bind(lapply(outcomes, function(y) {
      d <- complete(c(y, screen, 'platform'))
      tryCatch({
        base <- stats::lm(stats::reformulate(c(screen, 'platform'), response = y), data = d)
        inter <- stats::lm(stats::reformulate(c(screen, 'platform', paste0(screen, ':platform')),
                                            response = y), data = d)
        if (stats::df.residual(inter) <= 0 || inter$rank != length(stats::coef(inter)) ||
            base$rank != length(stats::coef(base))) stop('Insufficient rank/residual degrees of freedom.')
        a <- stats::anova(base, inter)
        data.frame(outcome = y, p_interaction = a$`Pr(>F)`[2],
          r2_without_interaction = summary(base)$r.squared, r2_with_interaction = summary(inter)$r.squared)
      }, error = function(e) {
        add_note(paste('Interaction unavailable for', y, ':', conditionMessage(e)))
        data.frame(outcome = y, p_interaction = NA_real_,
          r2_without_interaction = NA_real_, r2_with_interaction = NA_real_)
      })
    }))
    interactions$p_interaction_adj <- stats::p.adjust(interactions$p_interaction, 'BH')
    save_csv(platform_counts, 'platform_counts.csv')
    save_csv(platform_descriptives, 'platform_descriptives.csv')
    save_csv(omnibus, 'platform_kruskal_wallis.csv')
    save_csv(pairwise, 'platform_pairwise_wilcoxon.csv')
    save_csv(interactions, 'platform_interaction_tests.csv')
  }

  # 7. The same two simplified main figures.
  local_theme <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = 'bold', size = 15),
    strip.text = ggplot2::element_text(face = 'bold', size = 10),
    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
    panel.grid.minor = ggplot2::element_blank(), legend.position = 'none')
  colors <- c(Sleep_Duration_Hours = '#0072B2', GAD_7_Score = '#D55E00', PHQ_9_Score = '#009E73')
  trend_rows <- list(); plots <- list()
  for (y in names(models)) {
    m <- models[[y]]
    result <- tryCatch({
      b <- stats::coef(m)
      if (anyNA(b) || m$rank != length(b)) stop('Rank-deficient model.')
      mf <- stats::model.frame(m); adjusted <- 'platform' %in% names(mf)
      if (adjusted) {
        ranges <- lapply(split(mf[[screen]], droplevels(mf$platform)), range)
        lower <- max(vapply(ranges, function(x) x[1], numeric(1)))
        upper <- min(vapply(ranges, function(x) x[2], numeric(1)))
      } else { lower <- min(mf[[screen]]); upper <- max(mf[[screen]]) }
      if (!is.finite(lower) || !is.finite(upper) || upper <= lower) stop('No usable common screen-time range.')
      grid <- seq(lower, upper, length.out = 100)
      terms <- stats::delete.response(stats::terms(m))
      if (adjusted) {
        ct <- table(mf$platform); groups <- names(ct)[ct > 0]
        weights <- as.numeric(ct[groups]) / sum(ct)
        new <- expand.grid(screen_value = grid, platform = groups, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
        names(new)[1] <- screen
        new$platform <- factor(new$platform, levels = m$xlevels$platform)
        design <- stats::model.matrix(terms, new, contrasts.arg = m$contrasts, xlev = m$xlevels)
        X <- matrix(0, length(grid), ncol(design), dimnames = list(NULL, colnames(design)))
        for (j in seq_along(groups)) {
          rows <- ((j - 1) * length(grid) + 1):(j * length(grid))
          X <- X + weights[j] * design[rows, , drop = FALSE]
        }
      } else X <- stats::model.matrix(terms, setNames(data.frame(grid), screen))
      X <- X[, names(b), drop = FALSE]; V <- stats::vcov(m)[names(b), names(b), drop = FALSE]
      estimate <- as.numeric(X %*% b)
      se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
      critical <- stats::qt(.975, stats::df.residual(m))
      flag <- use_platform && any(interactions$outcome == y &
        !is.na(interactions$p_interaction_adj) & interactions$p_interaction_adj < .05)
      data.frame(outcome = y, screen_hours = grid, estimated_mean = estimate,
        lower_ci = estimate - critical * se, upper_ci = estimate + critical * se,
        panel = paste0(labels[[y]], '\nModel n = ', nrow(mf),
          if (adjusted) ' | platform-adjusted' else ' | unadjusted',
          if (flag) '\nDifferent slopes: review interaction results' else ''))
    }, error = function(e) { add_note(paste('Trend panel skipped for', y, ':', conditionMessage(e))); NULL })
    if (!is.null(result)) trend_rows[[y]] <- result
  }
  trend_data <- bind(trend_rows)
  if (nrow(trend_data)) {
    trend_data$panel <- factor(trend_data$panel, levels = unique(trend_data$panel))
    p <- ggplot2::ggplot(trend_data, ggplot2::aes(screen_hours, estimated_mean, colour = outcome, fill = outcome)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), alpha = .18, colour = NA) +
      ggplot2::geom_line(linewidth = 1.2) + ggplot2::facet_wrap(~ panel, ncol = 3, scales = 'free') +
      ggplot2::scale_colour_manual(values = colors) + ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(title = 'How are screen time, sleep and mental health related?',
        subtitle = 'Estimated mean outcomes from the existing additive linear models',
        x = 'Daily screen time (hours/day)', y = 'Estimated mean (panel-specific units)',
        caption = paste('Shading: pointwise 95% confidence intervals for means, not individual predictions.',
          'Platform composition is constant. Panels use their own scales and common observed screen-time ranges.',
          'Higher anxiety/depression scores indicate more symptoms. Associations are not causal.', sep = '\n')) + local_theme
    plots$screen_time_trends <- p
    save_plot(p, '01_screen_time_outcome_trends.png', 13, 6)
  }
  if (use_platform) {
    ct <- sort(table(dfp$platform), decreasing = TRUE); groups <- names(ct)
    group_labels <- setNames(paste0(groups, ' (n = ', as.integer(ct), ')'), groups)
    box_data <- tidyr::pivot_longer(tibble::as_tibble(dfp[c('platform', variables)]),
      cols = tidyr::all_of(variables), names_to = 'variable', values_to = 'value')
    box_data$panel <- factor(unname(labels[box_data$variable]), levels = unname(labels[variables]))
    p <- ggplot2::ggplot(box_data, ggplot2::aes(x = value, y = platform)) +
      ggplot2::geom_boxplot(orientation = 'y', outlier.shape = NA, na.rm = TRUE,
                            width = .55, fill = '#DCEAF3', colour = '#28536B') +
      ggplot2::facet_wrap(~ panel, scales = 'free_x', ncol = 2) +
      ggplot2::scale_y_discrete(limits = rev(groups), labels = group_labels) +
      ggplot2::labs(title = 'How do screen time and health outcomes vary by platform?',
        subtitle = 'Unadjusted distributions; platforms ordered by respondent count',
        x = 'Observed value (panel-specific units)', y = NULL,
        caption = paste('Centre line: median. Box: middle 50%. Whiskers: observed values within 1.5 IQR.',
          'Outliers are hidden, not excluded. Label n is platform total; available n may differ by outcome.',
          'Only retained platforms are shown. Comparisons are descriptive, not adjusted or causal.', sep = '\n')) + local_theme
    plots$platform_distributions <- p
    save_plot(p, '02_platform_comparisons.png', 13, max(7.5, 3.5 + .7 * length(groups)))
  }

  # 8. Overall report: calculated findings and complete statistical tables.
  number <- function(x) if (length(x) != 1L || is.na(x)) 'NA' else sprintf('%.3f', x)
  pvalue <- function(x) if (length(x) != 1L || is.na(x)) 'NA' else if (x < .001) '<0.001' else sprintf('%.3f', x)
  print_table <- function(x) {
    if (nrow(x)) print(as.data.frame(x), row.names = FALSE) else cat('No available results.\n')
  }
  report <- capture.output({
    cat('SCREEN TIME, SLEEP AND MENTAL HEALTH: MAIN STATISTICAL FINDINGS\n')
    cat('=============================================================\n\n')
    cat('1. SAMPLE AND DESCRIPTIVE RESULTS\n')
    cat('Total respondents: ', nrow(df), '; platform column: ', platform_var, '.\n', sep = '')
    cat('Minimum platform total: ', min_group_n, '; retained categories: ', length(retained), '.\n', sep = '')
    cat('Originally missing/blank platform: ', sum(is.na(platform_raw)),
      '; respondents filtered for rare categories: ', sum(!is.na(platform_raw) & !(platform_raw %in% retained)), '.\n', sep = '')
    if (use_platform) cat('Reference platform: ', reference, '.\n', sep = '')
    cat('Descriptives/correlations use available full-sample data; platform models use\n')
    cat('retained groups. Complete-case n varies. Platform NA includes filtered categories.\n\n')
    print_table(descriptive_stats)
    cat('\nMISSINGNESS\n'); print_table(missing_summary)
    if (use_platform) { cat('\nPLATFORM COUNTS\n'); print_table(platform_counts) }
    cat('\n2. SPEARMAN CORRELATIONS\nBH correction across three unadjusted monotonic associations.\n')
    cat(sum(correlations$significant_adjusted), ' of 3 meet BH-adjusted p < .05.\n', sep = '')
    for (i in seq_len(nrow(correlations))) {
      r <- correlations[i, ]
      cat('- ', labels[[r$outcome]], ': n = ', r$n, '; rho = ', number(r$rho),
        '; raw p = ', pvalue(r$p_value), '; BH p = ', pvalue(r$p_adjusted), '.\n', sep = '')
    }
    cat('\n3. LINEAR REGRESSIONS\n')
    cat(if (use_platform) 'Models include screen time and platform.\n' else 'Screen-time-only models.\n')
    cat('Coefficient and overall-model p-values are UNADJUSTED for multiple testing.\n')
    cat('Confidence intervals are conventional pointwise 95% CIs.\n')
    for (y in names(models)) {
      f <- fit_rows[[y]]; r <- coefficient_rows[[y]]
      r <- r[r$term == screen, , drop = FALSE]
      cat('\n', labels[[y]], ': n = ', f$n, '; R-squared = ', number(f$r_squared),
        '; adjusted R-squared = ', number(f$adjusted_r_squared),
        '; overall model p = ', pvalue(f$model_p_value), '.\n', sep = '')
      if (nrow(r) == 1L) {
        cat('Per additional hour/day: b = ', number(r$estimate), '; 95% CI [',
          number(r$conf.low), ', ', number(r$conf.high), ']; unadjusted p = ', pvalue(r$p.value), '.\n', sep = '')
        if (is.finite(r$estimate)) cat('Estimated association: ', number(abs(r$estimate)),
          if (y == outcomes[1]) ' hours/night' else ' score points',
          if (r$estimate >= 0) ' higher' else ' lower',
          if (use_platform) ', holding platform constant.\n' else ' (unadjusted).\n', sep = '')
        cat(if (is.na(r$p.value)) 'Inference unavailable.\n' else if (r$p.value < .05)
          'Meets unadjusted .05 threshold; multiple-testing caution applies.\n' else
          'Does not meet unadjusted .05 threshold; no-association is not established.\n')
      }
    }
    skipped <- setdiff(outcomes, names(models))
    if (length(skipped)) cat('Models skipped: ', paste(skipped, collapse = ', '), '.\n', sep = '')
    cat('\n4. PLATFORM COMPARISONS\n')
    if (use_platform) {
      cat('Kruskal-Wallis rank/distribution tests; BH across four variables.\n')
      cat('Not necessarily median differences when shapes differ; no covariate adjustment.\n')
      cat('Original effect-size formula epsilon_squared = H/(n-1) retained.\n')
      print_table(omnibus)
      sig <- omnibus$variable[!is.na(omnibus$p_adjusted) & omnibus$p_adjusted < .05]
      cat('Variables meeting adjusted .05 threshold: ', if (length(sig)) paste(unname(labels[sig]), collapse = '; ') else 'none among estimable tests', '.\n', sep = '')
      cat('\nPairwise Wilcoxon: p.value is ALREADY BH-adjusted within each variable.\n')
      sig_pairs <- pairwise[!is.na(pairwise$p.value) & pairwise$p.value < .05, ]
      cat('Pairs meeting adjusted threshold: ', nrow(sig_pairs), '.\n', sep = '')
      if (nrow(sig_pairs)) print_table(sig_pairs)
      cat('Pairwise correction is not across all variables combined.\n')
      cat('\n5. SCREEN-TIME-BY-PLATFORM INTERACTIONS\nNested-model F-tests; BH across three outcomes.\n')
      print_table(interactions)
      sig <- interactions$outcome[!is.na(interactions$p_interaction_adj) & interactions$p_interaction_adj < .05]
      if (length(sig)) {
        cat('Evidence of differing slopes for: ', paste(unname(labels[sig]), collapse = '; '), '.\n', sep = '')
        cat('The common-slope figure is incomplete for these outcomes: examine platform-specific slopes.\n')
      } else cat('No estimable interaction meets adjusted .05; this does not prove equal slopes.\n')
    } else cat('Platform comparisons/interactions skipped.\n')
    cat('\n6. INTERPRETATION AND LIMITATIONS\n')
    cat('Figure 1 shows modelled means; bands are not individual prediction intervals.\n')
    cat('Figure 2 is descriptive/unadjusted; box overlap is not a significance test.\n')
    cat('Review diagnostics for linearity, variance, normality and influential points.\n')
    cat('This report does not certify assumptions. GAD-7/PHQ-9 are bounded symptom\n')
    cat('scores, not diagnoses established by this analysis. Independent observations\n')
    cat('are assumed. Missingness, filtering and unmeasured confounding can bias results.\n')
    cat('No age/gender adjustment is included. Significance need not imply practical\n')
    cat('importance; non-significance is not proof of no association. No causal claims.\n')
    cat('\nEXECUTION NOTES\n')
    if (length(notes)) cat(paste0('- ', notes), sep = '\n') else cat('No recorded execution issues.\n')
    cat('\n\n7. COMPLETE RESULTS TABLES\n\nCORRELATIONS\n'); print_table(correlations)
    cat('\nREGRESSION COEFFICIENTS\n'); print_table(regression_results)
    cat('\nMODEL FIT\n'); print_table(model_summaries)
    cat('\nVIF/GVIF\n'); print_table(vif_results)
    cat('Adjusted GVIF = GVIF^(1/(2*Df)), equal to sqrt(VIF) for one-df terms.\n')
    cat('\nRESIDUAL DIAGNOSTICS\n'); print_table(diagnostics_summary)
    if (use_platform) {
      cat('\nPLATFORM DESCRIPTIVES\n'); print_table(platform_descriptives)
      cat('\nALL PAIRWISE TESTS (ADJUSTED P-VALUES)\n'); print_table(pairwise)
    }
  })
  save_text(report, 'analysis_report.txt')
  save_text(capture.output(utils::sessionInfo()), 'session_information.txt')
  if (verbose) {
    cat('\nDESCRIPTIVE STATISTICS\n'); print_table(descriptive_stats)
    cat('\nCORRELATIONS\n'); print_table(correlations)
    cat('\nMODEL FIT\n'); print_table(model_summaries)
    cat('\nANALYSIS COMPLETE\nOutput folder: ', output_dir, '\nReport: analysis_report.txt\n', sep = '')
  }
  invisible(list(data = df, platform_variable = platform_var, reference_platform = reference,
    descriptive_statistics = descriptive_stats, missing_summary = missing_summary,
    frequency_tables = frequencies, response_percentages = percentages,
    correlations = correlations, ranked_correlations = ranked,
    regression_models = models, regression_results = regression_results,
    model_summaries = model_summaries, vif_results = vif_results,
    diagnostics_summary = diagnostics_summary, platform_counts = platform_counts,
    platform_descriptives = platform_descriptives, platform_omnibus = omnibus,
    platform_pairwise = pairwise, platform_interaction = interactions,
    plots = plots, trend_data = trend_data, report_text = report,
    notes = notes, output_dir = output_dir, output_files = output_files))
}
