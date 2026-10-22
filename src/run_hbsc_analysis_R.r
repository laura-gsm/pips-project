# Save this file as src/run_hbsc_analysis.R.
# Sourcing this file defines the function; it does not run the analysis.

#' Run the HBSC social media and children's outcomes analysis
#'
#' @description
#' Recodes emcsocmed2 through emcsocmed9 from 1/2 to 0/1 and sums
#' the eight items into a score ranging from 0 to 8. The score is NA
#' unless all eight items are answered. Calculates descriptive statistics,
#' Spearman correlations, unadjusted linear regressions, and missingness.
#' Saves CSV tables, PNG plots, and a text report.
#'
#' @param data Optional data frame containing the original, unre coded data.
#'   If supplied, data_path is ignored. Items must contain only 1, 2, or NA;
#'   outcomes must be numeric, with finite values or NA.
#' @param data_path CSV path or URL, used when data is NULL. The default
#'   assumes main.Rmd is in the project root and the CSV is in data/.
#' @param output_folder Directory for saved outputs; created if necessary.
#'   Existing files with the same names are overwritten.
#' @param print_plots Logical. Print plots to the active graphics device
#'   (including an R Markdown document). Plots are always saved to disk.
#' @param verbose Logical. Print tables, score checks, and model summaries.
#'
#' @return Invisibly returns a named list containing analysis_data,
#'   descriptive_statistics, correlation_results, regression_results,
#'   regression_models, missing_data_summary, plots, output_folder,
#'   and output_files. Skipped regression models are NULL.
#'
#' @details
#' Each correlation and regression uses complete cases for its two variables.
#' Tests require at least three complete cases and variation in both variables.
#' Benjamini-Hochberg adjustments are applied separately to the four correlation
#' tests and the four regression tests. Outcome coding is left unchanged;
#' interpret coefficient directions using the dataset's codebook.
#' These analyses are unadjusted associations, not causal effects or clinical
#' diagnoses. Survey weights and clustering are not incorporated.
#' Requires ggplot2 and broom; install these before rendering main.Rmd.
#' The function does not change the working directory or attach packages.
#'
#' @examples
#' # From main.Rmd in the project root:
#' # source("src/run_hbsc_analysis.R")
#' # results <- run_hbsc_analysis(verbose = FALSE)
#' # results$correlation_results
#' # Or use a data frame already loaded by main.Rmd:
#' # results <- run_hbsc_analysis(data = actualdata, print_plots = FALSE)
run_hbsc_analysis <- function(
    data = NULL,
    data_path = "data/hbsc_data.csv",
    output_folder = "children_social_media+_folder",
    print_plots = TRUE,
    verbose = TRUE) {

  # 1. Check dependencies and arguments without attaching packages.
  required_packages <- c("ggplot2", "broom")
  missing_packages <- required_packages[!vapply(
    required_packages, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(missing_packages) > 0L) {
    stop("Install required packages before running: ",
         paste(missing_packages, collapse = ", "), call. = FALSE)
  }
  for (argument in c("print_plots", "verbose")) {
    value <- if (argument == "print_plots") print_plots else verbose
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(argument, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.character(output_folder) || length(output_folder) != 1L ||
      is.na(output_folder) || !nzchar(output_folder)) {
    stop("output_folder must be a non-empty directory path.", call. = FALSE)
  }

  # 2. Load and validate the original data.
  if (is.null(data)) {
    if (!is.character(data_path) || length(data_path) != 1L ||
        is.na(data_path) || !nzchar(data_path)) {
      stop("data_path must be a non-empty CSV path or URL.", call. = FALSE)
    }
    data <- tryCatch(
      utils::read.csv(data_path, row.names = NULL, stringsAsFactors = FALSE),
      error = function(e) {
        stop("Could not read HBSC data from ", data_path, ": ",
             conditionMessage(e), call. = FALSE)
      }
    )
  }
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("data must be a data frame with at least one row.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("data must have unique column names.", call. = FALSE)
  }

  social_media_items <- paste0("emcsocmed", 2:9)
  n_items <- length(social_media_items)
  outcomes <- c("feellow", "nervous", "sleepdificulty", "physact60")
  required_variables <- c(social_media_items, outcomes)
  missing_variables <- setdiff(required_variables, names(data))
  if (length(missing_variables) > 0L) {
    stop("Required columns not found: ",
         paste(missing_variables, collapse = ", "), call. = FALSE)
  }
  for (variable in social_media_items) {
    values <- data[[variable]]
    invalid <- !is.na(values) & !(as.character(values) %in% c("1", "2"))
    if (any(invalid)) {
      stop("Unexpected values in ", variable,
           ". Expected only 1, 2, or NA.", call. = FALSE)
    }
  }
  for (variable in outcomes) {
    values <- data[[variable]]
    if (!is.numeric(values) || any(!is.na(values) & !is.finite(values))) {
      stop(variable, " must be numeric with finite values or NA.",
           call. = FALSE)
    }
  }

  # 3. Create the output directory.
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_folder)) {
    stop("Could not create output directory: ", output_folder, call. = FALSE)
  }
  output_folder <- normalizePath(output_folder, mustWork = TRUE)
  output_files <- character()

  # Local output helpers keep the main analysis readable.
  save_table <- function(table, filename) {
    path <- file.path(output_folder, filename)
    utils::write.csv(table, path, row.names = FALSE, na = "")
    output_files <<- c(output_files, path)
  }
  save_plot <- function(plot, filename, width = 8, height = 5.5) {
    path <- file.path(output_folder, filename)
    ggplot2::ggsave(filename = path, plot = plot,
                   width = width, height = height, dpi = 300)
    output_files <<- c(output_files, path)
    if (print_plots) print(plot)
  }
  show_table <- function(title, table) {
    if (verbose) {
      cat("\n--- ", title, " ---\n", sep = "")
      print(table)
    }
  }

  # 4. Recode the eight items and calculate the complete-item score.
  analysis_data <- as.data.frame(data)
  for (variable in social_media_items) {
    analysis_data[[variable]] <- as.numeric(as.character(data[[variable]])) - 1
  }
  item_matrix <- as.matrix(analysis_data[social_media_items])
  analysis_data$n_items_answered <- rowSums(!is.na(item_matrix))
  analysis_data$social_media_score <- ifelse(
    analysis_data$n_items_answered == n_items,
    rowSums(item_matrix, na.rm = TRUE),
    NA_real_
  )
  if (verbose) {
    cat("\n--- CHECK OF RECODED SOCIAL MEDIA ITEMS ---\n")
    for (variable in social_media_items) {
      cat("\n", variable, "\n", sep = "")
      print(table(analysis_data[[variable]], useNA = "ifany"))
    }
    cat("\n--- SOCIAL MEDIA SCORE SUMMARY ---\n")
    print(summary(analysis_data$social_media_score))
    cat("\nParticipants with a complete score: ",
        sum(!is.na(analysis_data$social_media_score)),
        "\nParticipants without a complete score: ",
        sum(is.na(analysis_data$social_media_score)), "\n", sep = "")
  }
  save_table(analysis_data, "data_with_social_media_score.csv")

  # 5. Descriptive statistics.
  descriptive_statistics <- do.call(rbind, lapply(
    c("social_media_score", outcomes), function(variable) {
      values <- analysis_data[[variable]]
      values <- values[!is.na(values)]
      n <- length(values)
      data.frame(
        variable = variable, n = n,
        mean = if (n > 0L) mean(values) else NA_real_,
        sd = if (n > 1L) stats::sd(values) else NA_real_,
        median = if (n > 0L) stats::median(values) else NA_real_,
        min = if (n > 0L) min(values) else NA_real_,
        max = if (n > 0L) max(values) else NA_real_
      )
    }
  ))
  show_table("DESCRIPTIVE STATISTICS", descriptive_statistics)
  save_table(descriptive_statistics, "descriptive_statistics.csv")

  # 6. Score distribution.
  score_axis_label <- paste0("Social media score (0-", n_items, ")")
  score_plot <- ggplot2::ggplot(
    analysis_data, ggplot2::aes(x = social_media_score)
  ) +
    ggplot2::geom_bar(na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = 0:n_items) +
    ggplot2::labs(title = "Distribution of the Social Media Score",
                  x = score_axis_label, y = "Number of participants") +
    ggplot2::theme_minimal()
  save_plot(score_plot, "social_media_score_distribution.png", height = 5)

  # 7. Spearman correlations and unadjusted linear regressions.
  complete_data <- setNames(lapply(outcomes, function(outcome) {
    pairs <- analysis_data[c("social_media_score", outcome)]
    pairs[stats::complete.cases(pairs), , drop = FALSE]
  }), outcomes)
  can_estimate <- function(pairs) {
    nrow(pairs) >= 3L &&
      length(unique(pairs[[1L]])) >= 2L &&
      length(unique(pairs[[2L]])) >= 2L
  }
  regression_models <- setNames(vector("list", length(outcomes)), outcomes)
  correlation_rows <- list()
  regression_rows <- list()

  for (outcome in outcomes) {
    pairs <- complete_data[[outcome]]
    n <- nrow(pairs)
    correlation_row <- data.frame(
      outcome = outcome, n = n, spearman_rho = NA_real_, p_value = NA_real_
    )
    regression_row <- data.frame(
      outcome = outcome, n = n, estimate = NA_real_, std_error = NA_real_,
      statistic = NA_real_, p_value = NA_real_, r_squared = NA_real_
    )
    if (can_estimate(pairs)) {
      test <- stats::cor.test(
        pairs$social_media_score, pairs[[outcome]],
        method = "spearman", exact = FALSE
      )
      correlation_row$spearman_rho <- unname(test$estimate)
      correlation_row$p_value <- test$p.value

      model <- stats::lm(
        stats::reformulate("social_media_score", response = outcome),
        data = pairs
      )
      regression_models[[outcome]] <- model
      coefficients <- broom::tidy(model)
      coefficient <- coefficients[
        coefficients$term == "social_media_score", , drop = FALSE
      ]
      regression_row$estimate <- coefficient$estimate
      regression_row$std_error <- coefficient$std.error
      regression_row$statistic <- coefficient$statistic
      regression_row$p_value <- coefficient$p.value
      regression_row$r_squared <- summary(model)$r.squared
    } else {
      warning("Skipping correlation and regression for ", outcome,
              ": insufficient complete data or variation.", call. = FALSE)
    }
    correlation_rows[[outcome]] <- correlation_row
    regression_rows[[outcome]] <- regression_row
  }
  correlation_results <- do.call(rbind, correlation_rows)
  regression_results <- do.call(rbind, regression_rows)
  rownames(correlation_results) <- NULL
  rownames(regression_results) <- NULL
  correlation_results$adjusted_p_value <- stats::p.adjust(
    correlation_results$p_value, method = "BH"
  )
  regression_results$adjusted_p_value <- stats::p.adjust(
    regression_results$p_value, method = "BH"
  )
  show_table("SPEARMAN CORRELATIONS", correlation_results)
  show_table("LINEAR REGRESSION RESULTS", regression_results)
  save_table(correlation_results, "spearman_correlations.csv")
  save_table(regression_results, "linear_regression_results.csv")
  if (verbose) {
    cat("\n--- FULL REGRESSION MODEL SUMMARIES ---\n")
    for (outcome in outcomes) {
      if (!is.null(regression_models[[outcome]])) {
        cat("\nOutcome: ", outcome, "\n", sep = "")
        print(summary(regression_models[[outcome]]))
      }
    }
  }

  # 8. Create AND save each association graph inside the loop.
  outcome_labels <- c(
    feellow = "Low Feelings (Depression-Related Indicator)",
    nervous = "Nervousness (Anxiety-Related Indicator)",
    sleepdificulty = "Sleep Difficulties",
    physact60 = "Physical Activity"
  )
  association_plots <- setNames(vector("list", length(outcomes)), outcomes)
  for (outcome in outcomes) {
    pairs <- complete_data[[outcome]]
    # Standardize the y-column locally to avoid dynamic aes evaluation.
    plot_data <- data.frame(
      social_media_score = pairs$social_media_score,
      outcome_value = pairs[[outcome]]
    )
    estimable <- can_estimate(pairs)
    graph <- ggplot2::ggplot(
      plot_data, ggplot2::aes(x = social_media_score, y = outcome_value)
    ) +
      ggplot2::scale_x_continuous(
        breaks = 0:n_items, limits = c(-0.25, n_items + 0.25)
      ) +
      ggplot2::labs(
        title = outcome_labels[[outcome]],
        subtitle = paste0(
          if (estimable) "Overall linear trend" else "Trend not estimable",
          " | n = ", nrow(pairs)
        ),
        x = score_axis_label, y = outcome_labels[[outcome]],
        caption = if (estimable) {
          "Shaded area shows the 95% confidence interval."
        } else {
          "Insufficient complete data or variation to estimate a trend."
        }
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
    if (estimable) {
      graph <- graph + ggplot2::geom_smooth(
        method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
        linewidth = 1.2, color = "steelblue", fill = "lightblue", alpha = 0.4
      )
    } else {
      graph <- graph + ggplot2::geom_blank()
    }
    association_plots[[outcome]] <- graph
    save_plot(graph, paste0("association_social_media_score_", outcome, ".png"))
  }

  # 9. Missing data summary and plot.
  missing_n <- vapply(required_variables, function(variable) {
    sum(is.na(analysis_data[[variable]]))
  }, numeric(1))
  missing_data_summary <- data.frame(
    variable = required_variables,
    missing_n = unname(missing_n),
    total_n = nrow(analysis_data),
    missing_percent = unname(100 * missing_n / nrow(analysis_data))
  )
  show_table("MISSING DATA SUMMARY", missing_data_summary)
  save_table(missing_data_summary, "missing_data_summary.csv")
  missing_plot <- ggplot2::ggplot(
    missing_data_summary,
    ggplot2::aes(x = reorder(variable, missing_percent), y = missing_percent)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Missing Values by Variable",
                  x = "Variable", y = "Missing values (%)") +
    ggplot2::theme_minimal()
  save_plot(missing_plot, "missing_data_plot.png", height = 6)

  # 10. Write a report without redirecting the session's output with sink().
  report_file <- file.path(output_folder, "analysis_report.txt")
  report_lines <- capture.output({
    cat("SOCIAL MEDIA USE AND CHILDREN'S OUTCOMES\n")
    cat("========================================\n\n")
    cat("Social media score: sum of ", n_items,
        " recoded items (0-", n_items, ").\n", sep = "")
    cat("Items: ", paste(social_media_items, collapse = ", "), "\n", sep = "")
    cat("Original coding: 1 = No; 2 = Yes; NA = missing.\n")
    cat("A total score requires all ", n_items, " answers.\n", sep = "")
    cat("Participants with a complete score: ",
        sum(!is.na(analysis_data$social_media_score)), "\n", sep = "")
    cat("Participants without a complete score: ",
        sum(is.na(analysis_data$social_media_score)), "\n\n", sep = "")
    cat("Outcome coding is unchanged; consult the codebook for direction.\n")
    cat("Unadjusted associations do not establish causation.\n")
    cat("Survey weights and clustering are not incorporated.\n")
    cat("BH adjustments are applied separately to each family of four tests.\n")
    cat("\nDESCRIPTIVE STATISTICS\n")
    print(descriptive_statistics, row.names = FALSE)
    cat("\nSPEARMAN CORRELATIONS\n")
    print(correlation_results, row.names = FALSE)
    cat("\nLINEAR REGRESSION RESULTS\n")
    print(regression_results, row.names = FALSE)
    cat("\nMISSING DATA SUMMARY\n")
    print(missing_data_summary, row.names = FALSE)
    cat("\nFULL REGRESSION MODEL SUMMARIES\n")
    for (outcome in outcomes) {
      cat("\nOutcome: ", outcome, "\n", sep = "")
      model <- regression_models[[outcome]]
      if (is.null(model)) {
        cat("Skipped: insufficient complete data or variation.\n")
      } else {
        print(summary(model))
      }
    }
  })
  writeLines(report_lines, con = report_file)
  output_files <- c(output_files, report_file)
  if (verbose) {
    cat("\nANALYSIS COMPLETE\nOutput folder: ", output_folder, "\n", sep = "")
  }

  # Return results for reuse in main.Rmd without creating global objects.
  invisible(list(
    analysis_data = analysis_data,
    descriptive_statistics = descriptive_statistics,
    correlation_results = correlation_results,
    regression_results = regression_results,
    regression_models = regression_models,
    missing_data_summary = missing_data_summary,
    plots = list(
      score_distribution = score_plot,
      associations = association_plots,
      missing_data = missing_plot
    ),
    output_folder = output_folder,
    output_files = output_files
  ))
}
