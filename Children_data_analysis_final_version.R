
# ============================================================
# SOCIAL MEDIA USE AND CHILDREN'S OUTCOMES
# ============================================================
#
# BACKGROUND AND PURPOSE
#
# The total social media score is based on eight questionnaire
# items: emcsocmed2 through emcsocmed9.
#
# Each item was coded as:
#   1 = No
#   2 = Yes
#   NA = Missing
#
# For the total score, responses are recoded as:
#   No  (1) = 0
#   Yes (2) = 1
#
# The eight recoded items are added together to create a score
# ranging from 0 to 8. A higher score indicates that more of
# the eight items were answered "Yes".
#
# The total score is only calculated when all eight items have
# been answered. Missing responses are not counted as "No".
#
# Four numerical dependent variables are investigated:
#
# 1. feellow:
#    Used as an indicator of how low a person feels, with the
#    aim of estimating likelihood of depression-related
#    feelings. This is not, by itself, a clinical diagnosis.
#
# 2. nervous:
#    Used as an indicator of nervousness, with the aim of
#    estimating likelihood of anxiety-related feelings.
#    This is not, by itself, a clinical diagnosis.
#
# 3. sleepdificulty:
#    Used to investigate sleep difficulties.
#
# 4. physact60:
#    Used to investigate physical activity.
#
# The analysis examines associations between the social media
# score and each dependent variable. Associations do not
# establish that social media use causes an outcome.
#
# ============================================================


# ------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES AND DATASET
# ------------------------------------------------------------

# Loading the dataset
actualdata=read.csv("https://raw.githubusercontent.com/laura-gsm/pips-project/refs/heads/main/data/hbsc_data.csv", row.names = NULL)
View(actualdata)


# Run this installation command once if necessary:
# install.packages(c("tidyverse", "broom"))

library(tidyverse)
library(broom)


# ------------------------------------------------------------
# 2. CHECK THAT THE DATASET IS LOADED
# ------------------------------------------------------------

# The dataset will be called actualdata

if (!exists("actualdata")) {
  stop("Dataset 'actualdata' was not found. Please load it first.")
}


# ------------------------------------------------------------
# 3. CREATE THE OUTPUT FOLDER
# ------------------------------------------------------------

output_folder <- "children_social_media+_folder"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}


# ------------------------------------------------------------
# 4. IDENTIFY THE VARIABLES
# ------------------------------------------------------------

# Nine social media questionnaire items.
social_media_items <- paste0("emcsocmed", 2:9)

# Four dependent variables.
outcomes <- c(
  "feellow",
  "nervous",
  "sleepdificulty",
  "physact60"
)

# Check whether all required variables exist.
required_variables <- c(social_media_items, outcomes)

missing_variables <- setdiff(
  required_variables,
  names(actualdata)
)

if (length(missing_variables) > 0) {
  stop(
    "These variables were not found in actualdata: ",
    paste(missing_variables, collapse = ", "),
    ". Please check the column names."
  )
}


# ------------------------------------------------------------
# 5. CHECK AND RECODE THE NINE SOCIAL MEDIA ITEMS
# ------------------------------------------------------------

# First, check that each item contains only 1, 2, or NA.
# This prevents unexpected values from being silently recoded.

for (variable in social_media_items) {
  
  original_values <- actualdata[[variable]]
  
  invalid_values <- !is.na(original_values) &
    !(as.character(original_values) %in% c("1", "2"))
  
  if (any(invalid_values)) {
    stop(
      "Unexpected values found in ", variable,
      ". Expected only 1, 2, or NA."
    )
  }
}

# Recode the responses:
# 1 (No)  -> 0
# 2 (Yes) -> 1
# NA      -> NA

analysis_data <- actualdata %>%
  mutate(
    across(
      all_of(social_media_items),
      ~ case_when(
        is.na(.) ~ NA_real_,
        as.character(.) == "1" ~ 0,
        as.character(.) == "2" ~ 1
      )
    )
  )

cat("\n--- CHECK OF RECODED SOCIAL MEDIA ITEMS ---\n")

for (variable in social_media_items) {
  cat("\n", variable, "\n")
  print(table(analysis_data[[variable]], useNA = "ifany"))
}


# ------------------------------------------------------------
# 6. CALCULATE THE TOTAL SOCIAL MEDIA SCORE
# ------------------------------------------------------------

# Add the eight recoded items together.
#
# The total score is only calculated if all eight answers
# are present.
#
# Score interpretation:
# 0 = No to all eight items
# 8 = Yes to all eight items

analysis_data <- analysis_data %>%
  mutate(
    n_items_answered = rowSums(
      !is.na(pick(all_of(social_media_items)))
    ),
    
    social_media_score = if_else(
      n_items_answered == length(social_media_items),
      
      rowSums(
        pick(all_of(social_media_items)),
        na.rm = TRUE
      ),
      
      NA_real_
    )
  )

cat("\n--- SOCIAL MEDIA SCORE SUMMARY ---\n")
print(summary(analysis_data$social_media_score))

cat(
  "\nParticipants with a complete score:",
  sum(!is.na(analysis_data$social_media_score)),
  "\n"
)

cat(
  "Participants without a complete score:",
  sum(is.na(analysis_data$social_media_score)),
  "\n"
)


# Save the data with recoded items and the new score.
write_csv(
  analysis_data,
  file.path(output_folder, "data_with_social_media_score.csv")
)


# ------------------------------------------------------------
# 7. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

# Calculate the number of observations, mean, standard
# deviation, median, minimum, and maximum for each variable.

descriptive_statistics <- analysis_data %>%
  select(social_media_score, all_of(outcomes)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = if (n > 0) mean(value, na.rm = TRUE) else NA_real_,
    sd = if (n > 1) sd(value, na.rm = TRUE) else NA_real_,
    median = if (n > 0) median(value, na.rm = TRUE) else NA_real_,
    min = if (n > 0) min(value, na.rm = TRUE) else NA_real_,
    max = if (n > 0) max(value, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

cat("\n--- DESCRIPTIVE STATISTICS ---\n")
print(descriptive_statistics)

write_csv(
  descriptive_statistics,
  file.path(output_folder, "descriptive_statistics.csv")
)


# ------------------------------------------------------------
# 8. PLOT THE DISTRIBUTION OF THE SOCIAL MEDIA SCORE
# ------------------------------------------------------------

score_plot <- ggplot(
  analysis_data,
  aes(x = social_media_score)
) +
  geom_bar(na.rm = TRUE) +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "Distribution of the Social Media Score",
    x = "Social media score (0-9)",
    y = "Number of participants"
  ) +
  theme_minimal()

# Display the plot in RStudio.
print(score_plot)

# Save the plot.
ggsave(
  file.path(output_folder, "social_media_score_distribution.png"),
  plot = score_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# 9. SPEARMAN CORRELATION ANALYSIS
# ------------------------------------------------------------

# Spearman correlation measures the direction and strength
# of a monotonic association between two variables.
#
# Each test uses participants with non-missing values for
# the social media score and the outcome being examined.

correlation_results <- map_dfr(outcomes, function(outcome_name) {
  
  complete_pairs <- analysis_data %>%
    select(social_media_score, all_of(outcome_name)) %>%
    drop_na()
  
  if (
    nrow(complete_pairs) < 3 ||
    n_distinct(complete_pairs$social_media_score) < 2 ||
    n_distinct(complete_pairs[[outcome_name]]) < 2
  ) {
    return(tibble(
      outcome = outcome_name,
      n = nrow(complete_pairs),
      spearman_rho = NA_real_,
      p_value = NA_real_
    ))
  }
  
  test <- cor.test(
    complete_pairs$social_media_score,
    complete_pairs[[outcome_name]],
    method = "spearman",
    exact = FALSE
  )
  
  tibble(
    outcome = outcome_name,
    n = nrow(complete_pairs),
    spearman_rho = unname(test$estimate),
    p_value = test$p.value
  )
})

# Adjust for testing four outcomes using the Benjamini-Hochberg
# procedure to control the false discovery rate.

correlation_results <- correlation_results %>%
  mutate(
    adjusted_p_value = p.adjust(p_value, method = "BH")
  )

cat("\n--- SPEARMAN CORRELATIONS ---\n")
print(correlation_results)

write_csv(
  correlation_results,
  file.path(output_folder, "spearman_correlations.csv")
)


# ------------------------------------------------------------
# 10. LINEAR REGRESSION MODELS
# ------------------------------------------------------------

# Fit one model for each outcome.
#
# Model:
# Dependent variable ~ social_media_score
#
# The coefficient for social_media_score estimates the average
# change in the outcome associated with a one-point increase
# in the score, under the linear model assumptions.
#
# This is an unadjusted association: no other variables are
# included as predictors.

regression_models <- list()
regression_results_list <- list()

for (outcome_name in outcomes) {
  
  # Keep participants with complete data for this model.
  model_data <- analysis_data %>%
    select(all_of(outcome_name), social_media_score) %>%
    drop_na()
  
  cat(
    "\nFitting model for:", outcome_name,
    "| Number of participants:", nrow(model_data), "\n"
  )
  
  # Check that a model can be estimated.
  if (
    nrow(model_data) < 3 ||
    n_distinct(model_data$social_media_score) < 2 ||
    n_distinct(model_data[[outcome_name]]) < 2
  ) {
    warning(
      "Skipping model for ", outcome_name,
      ": insufficient data or variation."
    )
    
    regression_results_list[[outcome_name]] <- tibble(
      outcome = outcome_name,
      n = nrow(model_data),
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_
    )
    
    next
  }
  
  # Build and fit the model.
  model_formula <- reformulate(
    "social_media_score",
    response = outcome_name
  )
  
  model <- lm(model_formula, data = model_data)
  
  # Store the model for later inspection.
  regression_models[[outcome_name]] <- model
  
  # Extract the social media score coefficient.
  coefficient <- tidy(model) %>%
    filter(term == "social_media_score")
  
  regression_results_list[[outcome_name]] <- tibble(
    outcome = outcome_name,
    n = nrow(model_data),
    estimate = coefficient$estimate,
    std_error = coefficient$std.error,
    statistic = coefficient$statistic,
    p_value = coefficient$p.value,
    r_squared = summary(model)$r.squared
  )
}

# Combine the results into one table.
regression_results <- bind_rows(regression_results_list) %>%
  mutate(
    adjusted_p_value = p.adjust(p_value, method = "BH")
  )

cat("\n--- LINEAR REGRESSION RESULTS ---\n")
print(regression_results)

write_csv(
  regression_results,
  file.path(output_folder, "linear_regression_results.csv")
)

# Optional: print the full model summaries in the Console.
cat("\n--- FULL REGRESSION MODEL SUMMARIES ---\n")

for (outcome_name in names(regression_models)) {
  cat("\n\nOutcome:", outcome_name, "\n")
  print(summary(regression_models[[outcome_name]]))
}


# ------------------------------------------------------------
# 11. CREATE FOUR SEPARATE ASSOCIATION GRAPHS
# ------------------------------------------------------------

# Instead of putting all outcomes into one figure, create
# one graph for each overall association.
#
# Each graph includes:
# - Overall association lines
# - Shaded areas are the confidence intervals

#
# The social media score is always on the x-axis.
# The dependent variable is on the y-axis.

outcome_labels <- c(
  feellow = "Low Feelings (Depression-Related Indicator)",
  nervous = "Nervousness (Anxiety-Related Indicator)",
  sleepdificulty = "Sleep Difficulties",
  physact60 = "Physical Activity"
)

for (outcome_name in outcomes) {
  
  plot_data <- analysis_data %>%
    select(social_media_score, all_of(outcome_name)) %>%
    drop_na()
  
  graph_title <- outcome_labels[[outcome_name]]
  
  association_plot <- ggplot(
    plot_data,
    aes(
      x = social_media_score,
      y = .data[[outcome_name]]
    )
  ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1.2,
      color = "steelblue",
      fill = "lightblue",
      alpha = 0.4
    ) +
    scale_x_continuous(
      breaks = 0:9,
      limits = c(-0.25, 9.25)
    ) +
    labs(
      title = graph_title,
      subtitle = paste("Overall linear trend | n =", nrow(plot_data)),
      x = "Social media score (0–9)",
      y = graph_title,
      caption = "Shaded area shows the 95% confidence interval."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  print(association_plot)
}

  
  # Display each graph in the RStudio Plots window.
  print(association_plot)
  
  # Save each graph as a separate PNG file.
  file_name <- paste0(
    "association_social_media_score_",
    outcome_name,
    ".png"
  )
  
  ggsave(
    filename = file.path(output_folder, file_name),
    plot = association_plot,
    width = 8,
    height = 5.5,
    dpi = 300
  )



# ------------------------------------------------------------
# 12. SUMMARISE MISSING DATA
# ------------------------------------------------------------

# Calculate how many values are missing in each item and outcome.

variables_to_check <- c(social_media_items, outcomes)

missing_data_summary <- tibble(
  variable = variables_to_check,
  missing_n = map_int(
    variables_to_check,
    ~ sum(is.na(analysis_data[[.x]]))
  ),
  total_n = nrow(analysis_data),
  missing_percent = if (nrow(analysis_data) > 0) {
    100 * missing_n / total_n
  } else {
    NA_real_
  }
)

cat("\n--- MISSING DATA SUMMARY ---\n")
print(missing_data_summary)

write_csv(
  missing_data_summary,
  file.path(output_folder, "missing_data_summary.csv")
)


# Plot the percentage of missing values.
missing_plot <- ggplot(
  missing_data_summary,
  aes(
    x = reorder(variable, missing_percent),
    y = missing_percent
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Missing Values by Variable",
    x = "Variable",
    y = "Missing values (%)"
  ) +
  theme_minimal()

# Display and save the plot.
print(missing_plot)

ggsave(
  file.path(output_folder, "missing_data_plot.png"),
  plot = missing_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# 13. SAVE A TEXT REPORT
# ------------------------------------------------------------

report_file <- file.path(
  output_folder,
  "analysis_report.txt"
)

sink(report_file)

cat("SOCIAL MEDIA USE AND CHILDREN'S OUTCOMES\n")
cat("========================================\n\n")

cat("Social media score: sum of nine recoded items (0-9).\n")
cat("Original coding: 1 = No; 2 = Yes; NA = missing.\n")
cat("A total score requires all nine answers.\n\n")

cat("DESCRIPTIVE STATISTICS\n")
print(descriptive_statistics)

cat("\nSPEARMAN CORRELATIONS\n")
print(correlation_results)

cat("\nLINEAR REGRESSION RESULTS\n")
print(regression_results)

cat("\nMISSING DATA SUMMARY\n")
print(missing_data_summary)

sink()


# ------------------------------------------------------------
# 14. FINISH
# ------------------------------------------------------------

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat(
  "Output folder:",
  normalizePath(output_folder),
  "\n\n"
)

cat("Saved outputs include:\n")
cat("- Dataset with recoded items and social media score\n")
cat("- Descriptive statistics\n")
cat("- Spearman correlation results\n")
cat("- Linear regression results\n")
cat("- Full regression summaries in the Console\n")
cat("- One separate association graph per outcome\n")
cat("- Missing data summary and plot\n")
cat("- Text report\n\n")

cat("All graphs were printed to the RStudio Plots window.\n")
