# =============================================================================
# SIMULATED SOCIAL MEDIA SCREEN TIME AND HEALTH OUTCOMES
# Reproducible teaching example — all data/effects are fictional.
#
# Research question:
# How are time spent on Snapchat, Instagram, TikTok, Facebook and Pinterest
# associated with physical activity, sleep problems, depression and anxiety?
#
# Important:
# - This script simulates data; it does not estimate real-world effects.
# - Observational regression estimates associations, not causal "influences".
# - The outcome scores below are illustrative, not validated clinical scales.
# - Screen time is modelled in HOURS so coefficients are interpretable per
#   additional hour/day on an app (rather than per one minute).
# =============================================================================

# 0. Packages -----------------------------------------------------------------
required_packages <- c("MASS", "tidyverse", "broom", "car", "sandwich", "lmtest")
to_install <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(to_install) > 0) install.packages(to_install)

library(MASS)
library(tidyverse)
library(broom)
library(car)
library(sandwich)
library(lmtest)

# 1. Simulate data -------------------------------------------------------------
set.seed(2026)
n <- 500

app_names <- c(
  "snapchat_min", "instagram_min", "tiktok_min",
  "facebook_min", "pinterest_min"
)
medians <- c(35, 55, 45, 20, 15)
sigmas <- c(0.75, 0.70, 0.80, 0.85, 0.90)

corr <- matrix(c(
  1.00, 0.48, 0.42, 0.25, 0.22,
  0.48, 1.00, 0.50, 0.30, 0.32,
  0.42, 0.50, 1.00, 0.22, 0.25,
  0.25, 0.30, 0.22, 1.00, 0.28,
  0.22, 0.32, 0.25, 0.28, 1.00
), nrow = 5, byrow = TRUE)

cov_matrix <- diag(sigmas) %*% corr %*% diag(sigmas)
z <- MASS::mvrnorm(n, mu = rep(0, 5), Sigma = cov_matrix)
app_time <- exp(sweep(z, 2, log(medians), FUN = "+"))
app_time <- round(pmin(app_time, 300))
colnames(app_time) <- app_names

age <- sample(18:60, n, replace = TRUE)
gender <- sample(
  c("Woman", "Man", "Non-binary / another identity"),
  n, replace = TRUE, prob = c(0.50, 0.47, 0.03)
)

dat <- data.frame(
  participant_id = seq_len(n),
  age = age,
  gender = factor(gender),
  app_time,
  check.names = FALSE
)

# The data-generating mechanism uses hours/day.
app_hours <- as.matrix(dat[, app_names]) / 60
age_c <- (age - 30) / 10
gender_effect <- ifelse(gender == "Woman", 0.25,
                        ifelse(gender == "Man", 0, 0.15))

dat$depression_score <- round(pmin(27, pmax(0,
  5 + app_hours %*% c(0.35, 0.45, 0.60, 0.15, 0.05) +
    0.15 * age_c + gender_effect + rnorm(n, 0, 3.2)
)))

dat$anxiety_score <- round(pmin(21, pmax(0,
  4.5 + app_hours %*% c(0.40, 0.45, 0.55, 0.10, 0.05) +
    0.10 * age_c + 0.7 * gender_effect + rnorm(n, 0, 3.0)
)))

dat$sleep_problems_score <- round(pmin(21, pmax(0,
  5 + app_hours %*% c(0.25, 0.40, 0.55, 0.10, 0.05) -
    0.10 * age_c + rnorm(n, 0, 2.8)
)))

dat$physical_activity_min_day <- round(pmin(180, pmax(0,
  42 - app_hours %*% c(1.2, 1.5, 2.0, 0.5, 0.3) +
    0.25 * age_c + rnorm(n, 0, 13)
)))

# Make a separate modelling dataset with app exposure in hours/day.
app_hour_names <- paste0(sub("_min$", "", app_names), "_hours")
names(dat)[match(app_names, names(dat))] <- app_hour_names

# Save simulated dataset in the working directory.
write.csv(dat, "simulated_social_media_health_data.csv", row.names = FALSE)

# 2. Variable labels and quick checks ------------------------------------------
app_hours_names <- c(
  "snapchat_hours", "instagram_hours", "tiktok_hours",
  "facebook_hours", "pinterest_hours"
)
outcome_names <- c(
  "physical_activity_min_day", "sleep_problems_score",
  "depression_score", "anxiety_score"
)

outcome_labels <- c(
  physical_activity_min_day = "Physical activity (min/day)",
  sleep_problems_score = "Sleep problems score",
  depression_score = "Depression score",
  anxiety_score = "Anxiety score"
)

app_labels <- c(
  snapchat_hours = "Snapchat",
  instagram_hours = "Instagram",
  tiktok_hours = "TikTok",
  facebook_hours = "Facebook",
  pinterest_hours = "Pinterest"
)

cat("\n--- Dataset dimensions ---\n")
print(dim(dat))
cat("\n--- First rows ---\n")
print(head(dat))
cat("\n--- Missing values by variable ---\n")
print(colSums(is.na(dat)))
cat("\n--- Variable summaries ---\n")
print(summary(dat))

# 3. Descriptive statistics ----------------------------------------------------
# Continuous variables: n, mean, SD, median, IQR, minimum and maximum.
continuous_vars <- c(
  app_hours_names, "age", outcome_names
)
descriptives <- dat %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    variable = dplyr::recode(
      variable,
      !!!outcome_labels,
      .default = variable
    )
  )

cat("\n--- Descriptive statistics ---\n")
print(descriptives, n = Inf)
write.csv(descriptives, "descriptive_statistics.csv", row.names = FALSE)

cat("\n--- Gender counts and percentages ---\n")
gender_summary <- dat %>%
  count(gender) %>%
  mutate(percent = 100 * n / sum(n))
print(gender_summary)
write.csv(gender_summary, "gender_counts.csv", row.names = FALSE)

# 4. Exploratory plots ---------------------------------------------------------
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  strip.text = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))

# 4a. Distributions of outcomes
outcome_long <- dat %>%
  select(all_of(outcome_names)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "score") %>%
  mutate(outcome = dplyr::recode(outcome, !!!outcome_labels))

p_outcome_dist <- ggplot(outcome_long, aes(x = score)) +
  geom_histogram(bins = 25, color = "white", fill = "#3977A8") +
  facet_wrap(~ outcome, scales = "free", ncol = 2) +
  labs(title = "Distributions of simulated health outcomes",
       x = NULL, y = "Number of participants")
print(p_outcome_dist)
ggsave("01_outcome_distributions.png", p_outcome_dist,
       width = 10, height = 7, dpi = 300)

# 4b. Screen-time distributions, with each app on its own panel
screen_long <- dat %>%
  select(all_of(app_hours_names)) %>%
  pivot_longer(everything(), names_to = "app", values_to = "hours_day") %>%
  mutate(app = dplyr::recode(app, !!!app_labels))

p_screen_dist <- ggplot(screen_long, aes(x = hours_day)) +
  geom_histogram(bins = 25, color = "white", fill = "#2A9D8F") +
  facet_wrap(~ app, scales = "free", ncol = 3) +
  labs(title = "Daily screen time by social media app",
       x = "Screen time (hours/day)", y = "Number of participants")
print(p_screen_dist)
ggsave("02_screen_time_distributions.png", p_screen_dist,
       width = 10, height = 6, dpi = 300)

# 4c. Unadjusted scatterplots: descriptive only, not adjusted for other apps.
scatter_data <- dat %>%
  select(all_of(app_hours_names), all_of(outcome_names)) %>%
  pivot_longer(all_of(app_hours_names),
               names_to = "app", values_to = "hours_day") %>%
  pivot_longer(all_of(outcome_names),
               names_to = "outcome", values_to = "score") %>%
  mutate(
    app = dplyr::recode(app, !!!app_labels),
    outcome = dplyr::recode(outcome, !!!outcome_labels)
  )

p_scatter <- ggplot(scatter_data, aes(x = hours_day, y = score)) +
  geom_point(alpha = 0.28, size = 1.2, color = "#264653") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#E76F51", linewidth = 0.8) +
  facet_grid(outcome ~ app, scales = "free") +
  labs(title = "Unadjusted associations between app time and outcomes",
       subtitle = "Each line is a separate simple linear fit; no covariate adjustment",
       x = "Screen time (hours/day)", y = NULL)
print(p_scatter)
ggsave("03_unadjusted_scatterplots.png", p_scatter,
       width = 15, height = 10, dpi = 300)

# 4d. Correlations among app-use variables
cor_mat <- cor(dat[, app_hours_names], use = "pairwise.complete.obs")
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(app_1 = Var1, app_2 = Var2, correlation = Freq) %>%
  mutate(
    app_1 = dplyr::recode(app_1, !!!app_labels),
    app_2 = dplyr::recode(app_2, !!!app_labels)
  )

p_cor <- ggplot(cor_long, aes(x = app_1, y = app_2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 3.5) +
  scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027",
                       midpoint = 0, limits = c(-1, 1)) +
  coord_equal() +
  labs(title = "Correlations between social media screen-time variables",
       x = NULL, y = NULL, fill = "Pearson r") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
print(p_cor)
ggsave("04_app_time_correlations.png", p_cor,
       width = 8, height = 7, dpi = 300)

# 5. Multiple linear regression models ----------------------------------------
# Each model includes all five app exposures simultaneously and adjusts for
# age and gender. Coefficients for app exposure are per additional hour/day.
model_depression <- lm(
  depression_score ~ snapchat_hours + instagram_hours + tiktok_hours +
    facebook_hours + pinterest_hours + age + gender, data = dat
)
model_anxiety <- lm(
  anxiety_score ~ snapchat_hours + instagram_hours + tiktok_hours +
    facebook_hours + pinterest_hours + age + gender, data = dat
)
model_sleep <- lm(
  sleep_problems_score ~ snapchat_hours + instagram_hours + tiktok_hours +
    facebook_hours + pinterest_hours + age + gender, data = dat
)
model_activity <- lm(
  physical_activity_min_day ~ snapchat_hours + instagram_hours + tiktok_hours +
    facebook_hours + pinterest_hours + age + gender, data = dat
)

models <- list(
  "Depression" = model_depression,
  "Anxiety" = model_anxiety,
  "Sleep problems" = model_sleep,
  "Physical activity" = model_activity
)

cat("\n--- Model summaries ---\n")
invisible(lapply(models, print))

# 6. Regression tables and inference ------------------------------------------
# Report estimates, 95% CIs, p-values and model fit. Use Holm correction across
# the 20 app-specific tests (5 apps x 4 outcomes) to reduce multiplicity.
tidy_results <- bind_rows(lapply(names(models), function(outcome) {
  broom::tidy(models[[outcome]], conf.int = TRUE) %>%
    mutate(outcome = outcome)
}))

app_terms <- app_hours_names
app_results <- tidy_results %>%
  filter(term %in% app_terms) %>%
  group_by(outcome) %>%
  mutate(p_holm_within_outcome = p.adjust(p.value, method = "holm")) %>%
  ungroup() %>%
  mutate(
    p_holm_all_app_tests = p.adjust(p.value, method = "holm"),
    app = dplyr::recode(term, !!!app_labels)
  ) %>%
  select(outcome, app, term, estimate, std.error, statistic,
         p.value, p_holm_within_outcome, p_holm_all_app_tests,
         conf.low, conf.high)

cat("\n--- App exposure coefficients (per additional hour/day) ---\n")
print(app_results, n = Inf)
write.csv(app_results, "regression_app_coefficients.csv", row.names = FALSE)

model_fit <- bind_rows(lapply(names(models), function(outcome) {
  glance(models[[outcome]]) %>% mutate(outcome = outcome)
})) %>%
  select(outcome, nobs, r.squared, adj.r.squared, sigma, statistic,
         p.value, AIC, BIC)

cat("\n--- Model fit statistics ---\n")
print(model_fit)
write.csv(model_fit, "regression_model_fit.csv", row.names = FALSE)

# Optional robust standard errors (HC3) for heteroskedasticity-robust inference.
# These are a sensitivity analysis; coefficient estimates do not change.
robust_results <- bind_rows(lapply(names(models), function(outcome) {
  m <- models[[outcome]]
  ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC3"))
  tibble(
    outcome = outcome,
    term = rownames(ct),
    estimate = ct[, 1],
    robust_se_HC3 = ct[, 2],
    t_value = ct[, 3],
    p_value_HC3 = ct[, 4]
  )
})) %>%
  filter(term %in% app_terms) %>%
  mutate(app = dplyr::recode(term, !!!app_labels))

cat("\n--- HC3 robust standard-error sensitivity analysis ---\n")
print(robust_results, n = Inf)
write.csv(robust_results, "regression_HC3_robust_results.csv", row.names = FALSE)

# 7. Coefficient plot ----------------------------------------------------------
# Coefficients are adjusted for the other four apps, age and gender.
# The vertical zero line represents no estimated association.
p_coef <- ggplot(app_results,
                 aes(x = estimate, y = factor(app, levels = rev(unname(app_labels))))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y", width = 0.18, color = "#3977A8") +
  geom_point(size = 2.6, color = "#E76F51") +
  facet_wrap(~ outcome, scales = "free_x", ncol = 2) +
  labs(
    title = "Adjusted associations between app screen time and outcomes",
    subtitle = "Points are coefficients; bars are 95% confidence intervals",
    x = "Change in outcome score/minutes per additional hour/day",
    y = "Social media app"
  ) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
print(p_coef)
ggsave("05_adjusted_coefficient_plot.png", p_coef,
       width = 12, height = 8, dpi = 300)

# 8. Regression diagnostics ----------------------------------------------------
# Inspect all four diagnostic panels for each model.
for (model_name in names(models)) {
  m <- models[[model_name]]
  safe_name <- gsub("[^A-Za-z0-9]+", "_", tolower(model_name))
  png(paste0("diagnostics_", safe_name, ".png"),
      width = 1400, height = 1200, res = 160)
  par(mfrow = c(2, 2))
  plot(m, main = model_name)
  dev.off()
}
par(mfrow = c(1, 1))

# VIF can flag possible multicollinearity among predictors.
vif_results <- bind_rows(lapply(names(models), function(model_name) {
  v <- car::vif(models[[model_name]])
  if (is.matrix(v)) {
    tibble(model = model_name, term = rownames(v),
           GVIF = v[, "GVIF"], Df = v[, "Df"],
           adjusted_GVIF = v[, "GVIF"]^(1 / (2 * v[, "Df"])))
  } else {
    tibble(model = model_name, term = names(v),
           GVIF = as.numeric(v), Df = 1,
           adjusted_GVIF = as.numeric(v)^(1 / 2))
  }
}))
cat("\n--- Variance inflation factors (review, do not use a single cutoff blindly) ---\n")
print(vif_results, n = Inf)
write.csv(vif_results, "vif_results.csv", row.names = FALSE)

# 9. Save a concise session report ---------------------------------------------
capture.output(sessionInfo(), file = "R_session_info.txt")

cat("\nFinished. Files saved to your working directory:\n",
    "- simulated_social_media_health_data.csv\n",
    "- descriptive_statistics.csv\n",
    "- gender_counts.csv\n",
    "- regression_app_coefficients.csv\n",
    "- regression_model_fit.csv\n",
    "- regression_HC3_robust_results.csv\n",
    "- vif_results.csv\n",
    "- 01_outcome_distributions.png through 05_adjusted_coefficient_plot.png\n",
    "- diagnostics_*.png\n",
    "- R_session_info.txt\n")
