# ============================================================
# SHINY APP: SCREEN TIME, PLATFORM, SLEEP AND MENTAL HEALTH
# ============================================================
# Flow:
#   1. A centred screen where the user ticks their platform(s)
#      and sets their daily screen time.
#   2. A short animated "analysing" transition.
#   3. An animated results screen (rings, counters, curves).
#
# Predictions come from the same models as the analysis script:
#   outcome ~ Daily_Screen_Time_Hours + platform   (lm)
# with the most common platform as the reference group, and
# platforms with fewer than 30 respondents removed.
#
# Needs R >= 4.0 (raw strings). Run with:
#   shiny::runApp("path/to/folder/containing/app.R")
#
# IMPORTANT: these are statistical associations, not causal
# effects, and not a diagnosis. Individual scores vary a lot
# around the predictions.
# ============================================================

required_packages <- c("shiny", "ggplot2", "dplyr", "stringr", "tidyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)


# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
# Tries the GitHub URL first. If you are offline, put a copy of
# the CSV next to app.R and name it as below.

DATA_URL <- paste0(
  "https://raw.githubusercontent.com/laura-gsm/pips-project/",
  "c075d4ef653730e296b8c88f17539d9b260b8d35/",
  "social_media_mental_health%202.csv"
)
LOCAL_FILE <- "social_media_mental_health 2.csv"

actualdata <- tryCatch(
  read.csv(DATA_URL, stringsAsFactors = FALSE),
  error = function(e) NULL
)
if (is.null(actualdata) && file.exists(LOCAL_FILE)) {
  actualdata <- read.csv(LOCAL_FILE, stringsAsFactors = FALSE)
}
if (is.null(actualdata)) {
  stop("Could not load the dataset from the URL or from '", LOCAL_FILE, "'.")
}

df <- actualdata


# ------------------------------------------------------------
# 2. VARIABLES (same as the analysis script)
# ------------------------------------------------------------

screen_var <- "Daily_Screen_Time_Hours"
outcome_vars <- c("Sleep_Duration_Hours", "GAD_7_Score", "PHQ_9_Score")
min_group_n <- 30
platform_var <- NULL   # NULL = detect a column containing "platform"

missing_vars <- setdiff(c(screen_var, outcome_vars), names(df))
if (length(missing_vars) > 0) {
  stop("Variables not found: ", paste(missing_vars, collapse = ", "))
}

if (is.null(platform_var)) {
  candidates <- grep("platform", names(df), ignore.case = TRUE, value = TRUE)
  if (length(candidates) != 1) {
    stop("Could not identify the platform column. Set platform_var manually. ",
         "Columns: ", paste(names(df), collapse = ", "))
  }
  platform_var <- candidates
}

platform_raw <- str_squish(as.character(df[[platform_var]]))
platform_raw[platform_raw == ""] <- NA

platform_table_raw <- sort(table(platform_raw), decreasing = TRUE)
keep_levels <- names(platform_table_raw)[platform_table_raw >= min_group_n]

df$platform <- factor(
  ifelse(platform_raw %in% keep_levels, platform_raw, NA),
  levels = keep_levels
)

if (nlevels(df$platform) < 2) {
  stop("Fewer than 2 platforms with at least ", min_group_n, " respondents.")
}

ref_platform <- levels(df$platform)[1]
platform_n <- table(df$platform)
n_total <- sum(!is.na(df$platform))

screen_min <- floor(min(df[[screen_var]], na.rm = TRUE))
screen_max <- ceiling(max(df[[screen_var]], na.rm = TRUE))
screen_mean <- round(mean(df[[screen_var]], na.rm = TRUE), 1)
screen_mean <- min(max(screen_mean, screen_min), screen_max)


# ------------------------------------------------------------
# 3. PALETTE
# ------------------------------------------------------------

palette_platforms <- c("#22d3ee", "#a78bfa", "#f472b6", "#fbbf24",
                       "#5eead4", "#fb923c", "#60a5fa", "#fb7185")
platform_colours <- setNames(
  rep_len(palette_platforms, nlevels(df$platform)),
  levels(df$platform)
)

outcome_info <- list(
  Sleep_Duration_Hours = list(
    label = "Sleep", unit = "hours / night", lower = 0, upper = 24,
    gauge_max = 12, accent = c("#22d3ee", "#60a5fa")
  ),
  GAD_7_Score = list(
    label = "Anxiety (GAD-7)", unit = "out of 21", lower = 0, upper = 21,
    gauge_max = 21, accent = c("#a78bfa", "#818cf8")
  ),
  PHQ_9_Score = list(
    label = "Depression (PHQ-9)", unit = "out of 27", lower = 0, upper = 27,
    gauge_max = 27, accent = c("#f472b6", "#fb7185")
  )
)

col_good <- "#5eead4"; col_mild <- "#fbbf24"
col_mod <- "#fb923c";  col_severe <- "#fb7185"


# ------------------------------------------------------------
# 4. FIT THE MODELS
# ------------------------------------------------------------

models <- lapply(outcome_vars, function(y) {
  d <- df %>%
    select(all_of(c(y, screen_var, "platform"))) %>%
    tidyr::drop_na()
  lm(reformulate(c(screen_var, "platform"), response = y), data = d)
})
names(models) <- outcome_vars

model_summary_table <- do.call(rbind, lapply(outcome_vars, function(y) {
  s <- summary(models[[y]])
  data.frame(
    Outcome = outcome_info[[y]]$label,
    N = nobs(models[[y]]),
    `R-squared` = round(s$r.squared, 3),
    `Typical error (residual SD)` = round(s$sigma, 2),
    check.names = FALSE
  )
}))

coef_table <- do.call(rbind, lapply(outcome_vars, function(y) {
  ct <- as.data.frame(coef(summary(models[[y]])))
  ci <- confint(models[[y]])
  data.frame(
    Outcome = outcome_info[[y]]$label,
    Term = rownames(ct),
    Estimate = round(ct$Estimate, 3),
    `CI low` = round(ci[, 1], 3),
    `CI high` = round(ci[, 2], 3),
    `p-value` = signif(ct$`Pr(>|t|)`, 3),
    check.names = FALSE,
    row.names = NULL
  )
}))
coef_table$Term <- sub("^platform", "Platform: ", coef_table$Term)
coef_table$Term <- sub(screen_var, "Screen time (per hour)", coef_table$Term)

# Lookups used to write the plain-language interpretation
outcome_stats <- lapply(outcome_vars, function(y) {
  v <- model.frame(models[[y]])[[1]]
  list(mean = mean(v), sd = sd(v), values = v)
})
names(outcome_stats) <- outcome_vars
model_coefs <- lapply(models, function(m) coef(summary(m)))
model_cis <- lapply(models, confint)
model_r2 <- vapply(models, function(m) summary(m)$r.squared, numeric(1))
screen_mean_exact <- mean(df[[screen_var]], na.rm = TRUE)


# ------------------------------------------------------------
# 5. HELPERS
# ------------------------------------------------------------

#' Restrict values to a range.
#'
#' Purpose: keep predictions inside the possible score range of an outcome
#' (e.g. GAD-7 must stay between 0 and 21).
#' @param x Numeric vector of values.
#' @param lo,hi Numeric scalars: lower and upper bound.
#' @return Numeric vector, same length as `x`, with values limited to [lo, hi].
clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

#' Predict all three outcomes for a given platform selection and screen time.
#'
#' Purpose: apply the fitted regression models (outcome ~ screen time +
#' platform) to the user's inputs, with confidence and prediction intervals.
#' @param platforms Character vector of platform names; each must be one of
#'   levels(df$platform).
#' @param hours Single numeric value: daily screen time in hours.
#' @return A data.frame with one row per platform x outcome and the columns
#'   `outcome`, `platform`, `fit` (predicted value), `ci_low`/`ci_high` (95%
#'   confidence interval for the average person) and `pred_low`/`pred_high`
#'   (95% prediction interval for an individual). All values are clipped to
#'   the possible range of the outcome.
predict_all <- function(platforms, hours) {
  newdata <- data.frame(
    Daily_Screen_Time_Hours = hours,
    platform = factor(platforms, levels = levels(df$platform))
  )
  names(newdata)[1] <- screen_var

  bind_rows(lapply(outcome_vars, function(y) {
    info <- outcome_info[[y]]
    ci <- predict(models[[y]], newdata, interval = "confidence")
    pi <- predict(models[[y]], newdata, interval = "prediction")
    data.frame(
      outcome = y,
      platform = platforms,
      fit = clamp(ci[, "fit"], info$lower, info$upper),
      ci_low = clamp(ci[, "lwr"], info$lower, info$upper),
      ci_high = clamp(ci[, "upr"], info$lower, info$upper),
      pred_low = clamp(pi[, "lwr"], info$lower, info$upper),
      pred_high = clamp(pi[, "upr"], info$lower, info$upper),
      stringsAsFactors = FALSE
    )
  }))
}

#' Classify a predicted value into a descriptive band and a colour.
#'
#' Purpose: translate a numeric prediction into the label and colour shown on
#' the result cards (sleep: recommended 7-9 h; GAD-7 and PHQ-9: minimal to
#' severe ranges, which are screening bands and not diagnoses).
#' @param y Character: outcome variable name, one of `outcome_vars`.
#' @param x Single numeric value: the predicted outcome.
#' @return A list of length 2: a label (character) and a hex colour (character).
category_and_colour <- function(y, x) {
  if (y == "Sleep_Duration_Hours") {
    if (x >= 7 && x <= 9) return(list("Within the recommended 7\u20139 h", col_good))
    if (x > 9) return(list("Above the recommended 7\u20139 h", col_mild))
    if (x >= 6) return(list("Slightly below the recommended 7\u20139 h", col_mod))
    return(list("Well below the recommended 7\u20139 h", col_severe))
  }
  if (y == "GAD_7_Score") {
    if (x < 4.5) return(list("Minimal range", col_good))
    if (x < 9.5) return(list("Mild range", col_mild))
    if (x < 14.5) return(list("Moderate range", col_mod))
    return(list("Severe range", col_severe))
  }
  if (x < 4.5) return(list("Minimal range", col_good))
  if (x < 9.5) return(list("Mild range", col_mild))
  if (x < 14.5) return(list("Moderate range", col_mod))
  if (x < 19.5) return(list("Moderately severe range", col_mod))
  list("Severe range", col_severe)
}

#' Build the result card for one outcome.
#'
#' Purpose: render an animated ring gauge, the predicted value, its band,
#' the comparison with the average respondent and the 95% individual range.
#' @param y Character: outcome variable name, one of `outcome_vars`.
#' @param rows A data.frame: the rows of predict_all() output belonging to
#'   outcome `y` (one row per selected platform).
#' @param idx Integer: position of the card, used to stagger its entrance
#'   animation.
#' @return A shiny tag (div) with the card HTML.
outcome_card <- function(y, rows, idx) {
  info <- outcome_info[[y]]
  est <- mean(rows$fit)
  p_lo <- mean(rows$pred_low)
  p_hi <- mean(rows$pred_high)
  cc <- category_and_colour(y, est)
  frac <- min(max(est / info$gauge_max, 0.02), 1)
  circ <- 2 * pi * 54
  grad_id <- paste0("grad-", y)

  ring_svg <- HTML(paste0(
    '<svg class="ring" viewBox="0 0 140 140">',
    '<defs><linearGradient id="', grad_id, '" x1="0" y1="0" x2="1" y2="1">',
    '<stop offset="0%" stop-color="', info$accent[1], '"/>',
    '<stop offset="100%" stop-color="', info$accent[2], '"/>',
    '</linearGradient></defs>',
    '<circle class="ring-bg" cx="70" cy="70" r="54"/>',
    '<circle class="ring-fg" cx="70" cy="70" r="54" transform="rotate(-90 70 70)" ',
    'stroke="url(#', grad_id, ')" ',
    'stroke-dasharray="', sprintf("%.2f", circ), '" ',
    'stroke-dashoffset="', sprintf("%.2f", circ), '" ',
    'data-frac="', sprintf("%.3f", frac), '"/>',
    '</svg>'
  ))

  across <- if (nrow(rows) > 1) {
    div(class = "card-note",
        sprintf("Across your platforms: %.1f to %.1f",
                min(rows$fit), max(rows$fit)))
  }

  div(
    class = "result-card reveal",
    style = paste0("--i:", idx, "; --a1:", info$accent[1], "; --a2:", info$accent[2], ";"),
    div(class = "card-label", info$label),
    div(
      class = "ring-wrap",
      ring_svg,
      div(
        class = "ring-center",
        span(class = "count-up", `data-value` = sprintf("%.1f", est), "0.0"),
        span(class = "ring-unit", info$unit)
      )
    ),
    div(class = "badge",
        span(class = "badge-dot", style = paste0("background:", cc[[2]], ";")),
        cc[[1]]),
    div(class = "card-note card-avg", vs_average_text(y, est)),
    div(class = "card-note",
        sprintf("Where an individual might fall (95%%): %.1f to %.1f", p_lo, p_hi)),
    across
  )
}

# ---- plain-language interpretation of the model results ----

#' Format a number with a fixed number of decimals and a true minus sign.
#' @param x Numeric vector.
#' @param digits Integer: number of decimals (default 2).
#' @return Character vector.
fmt_num <- function(x, digits = 2) {
  sub("-", "\u2212", formatC(x, format = "f", digits = digits), fixed = TRUE)
}
#' Format a p-value for display (e.g. "p = .034" or "p < .001").
#' @param p Single numeric p-value (may be NA).
#' @return Character string.
fmt_p <- function(p) {
  if (is.na(p)) return("p = n/a")
  if (p < 0.001) return("p < .001")
  paste0("p = ", sub("^0", "", sprintf("%.3f", p)))
}
#' Format a proportion as a whole-number percentage.
#' @param x Numeric vector of proportions between 0 and 1.
#' @return Character vector such as "42%".
fmt_pct <- function(x) sprintf("%.0f%%", 100 * x)
#' Join words into a readable list ("a, b and c").
#' @param x Character vector.
#' @return Character string (or `x` unchanged if it has length 0 or 1).
join_and <- function(x) {
  if (length(x) <= 1) return(x)
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

#' Describe the size of an effect in words.
#'
#' Purpose: express a regression slope relative to the spread of the outcome.
#' @param ratio Numeric: slope divided by the outcome's standard deviation.
#' @return One of "tiny", "small", "modest" or "sizeable".
magnitude_word <- function(ratio) {
  ratio <- abs(ratio)
  if (ratio < 0.05) "tiny"
  else if (ratio < 0.15) "small"
  else if (ratio < 0.30) "modest"
  else "sizeable"
}

#' Describe how much variance a model explains in words.
#' @param r2 Numeric: R-squared of the model (0 to 1).
#' @return A phrase such as "very little" or "a modest share".
variance_word <- function(r2) {
  if (r2 < 0.02) "very little"
  else if (r2 < 0.10) "only a small share"
  else if (r2 < 0.25) "a modest share"
  else "a substantial share"
}

#' Describe a change in an outcome in plain language.
#'
#' Purpose: turn a regression coefficient into text such as
#' "0.12 hours less sleep" or "a 0.20-point higher GAD-7 score".
#' @param y Character: outcome variable name, one of `outcome_vars`.
#' @param b Single numeric value: the change in the outcome (a coefficient or
#'   a coefficient times a difference in hours).
#' @return Character string.
change_phrase <- function(y, b) {
  if (y == "Sleep_Duration_Hours") {
    sprintf("%s hours %s sleep", fmt_num(abs(b)), if (b >= 0) "more" else "less")
  } else {
    label <- if (y == "GAD_7_Score") "GAD-7" else "PHQ-9"
    sprintf("a %s-point %s %s score", fmt_num(abs(b)), if (b >= 0) "higher" else "lower", label)
  }
}

#' Compare a predicted value with the average respondent.
#' @param y Character: outcome variable name, one of `outcome_vars`.
#' @param est Single numeric value: the predicted outcome.
#' @return Character string, e.g. "0.8 h less than the average respondent (6.9 h)".
vs_average_text <- function(y, est) {
  st <- outcome_stats[[y]]
  d <- est - st$mean
  if (y == "Sleep_Duration_Hours") {
    if (abs(d) < 0.05) return(sprintf("About the same as the average respondent (%.1f h)", st$mean))
    return(sprintf("%.1f h %s than the average respondent (%.1f h)",
                   abs(d), if (d > 0) "more" else "less", st$mean))
  }
  if (abs(d) < 0.05) return(sprintf("About the same as the average respondent (%.1f)", st$mean))
  sprintf("%.1f points %s than the average respondent (%.1f)",
          abs(d), if (d > 0) "higher" else "lower", st$mean)
}

#' Build the explanation block for one outcome.
#'
#' Purpose: write plain-language, data-based text on the prediction, the
#' screen-time association, platform differences, how common the level is
#' and how much the model explains.
#' @param y Character: outcome variable name, one of `outcome_vars`.
#' @param r A list with `platforms` (character), `hours` (numeric) and
#'   `preds` (output of predict_all()).
#' @param idx Integer: position of the block, used to stagger its animation.
#' @return A shiny tag (div) with the explanation.
build_insight <- function(y, r, idx) {
  info <- outcome_info[[y]]
  st <- outcome_stats[[y]]
  cf <- model_coefs[[y]]
  ci <- model_cis[[y]]
  is_sleep <- y == "Sleep_Duration_Hours"
  tag <- if (y == "GAD_7_Score") "GAD-7" else "PHQ-9"

  rows <- r$preds[r$preds$outcome == y, ]
  est <- mean(rows$fit)
  cc <- category_and_colour(y, est)

  # --- headline ---
  headline <- if (is_sleep) {
    sprintf("With %.1f hours of screen time a day, the model predicts about %.1f hours of sleep per night (%s).",
            r$hours, est, tolower(cc[[1]]))
  } else {
    sprintf("With %.1f hours of screen time a day, the model predicts a %s score of about %.1f, in the %s.",
            r$hours, tag, est, tolower(cc[[1]]))
  }
  if (length(r$platforms) > 1) {
    headline <- paste(headline, "This is the average of the predictions for your platforms.")
  }

  # --- comparison with the average respondent ---
  d <- est - st$mean
  vs_avg <- if (abs(d) < 0.05) {
    sprintf("That is about the same as the average respondent (%.1f).", st$mean)
  } else if (is_sleep) {
    sprintf("That is %.1f hours %s than the average respondent (%.1f hours).",
            abs(d), if (d > 0) "more" else "less", st$mean)
  } else {
    sprintf("That is %.1f points %s than the average respondent (%.1f).",
            abs(d), if (d > 0) "higher" else "lower", st$mean)
  }

  # --- screen-time association ---
  b <- cf[screen_var, "Estimate"]
  p <- cf[screen_var, "Pr(>|t|)"]
  lo <- ci[screen_var, 1]
  hi <- ci[screen_var, 2]
  verdict <- if (p < 0.05) "a statistically clear association" else "not statistically distinguishable from zero"
  slope_item <- tags$li(
    tags$strong("Screen time: "),
    sprintf("in this dataset, each additional hour per day is associated with %s (95%% CI %s to %s; %s), %s. ",
            change_phrase(y, b), fmt_num(lo), fmt_num(hi), fmt_p(p), verdict),
    sprintf("Relative to how much people differ from one another (SD = %.1f), this is a %s effect.",
            st$sd, magnitude_word(b / st$sd))
  )

  # --- what your hours mean relative to the average ---
  gap <- r$hours - screen_mean_exact
  shift_item <- tags$li(
    tags$strong("Your hours vs. the average: "),
    if (abs(gap) < 0.25) {
      sprintf("your screen time is close to the average in the data (%.1f h/day), so it barely changes how you compare with the average respondent.",
              screen_mean_exact)
    } else {
      sprintf("the average respondent reports %.1f h/day. Your %.1f h/day (%s h %s) corresponds to %s in the model.",
              screen_mean_exact, r$hours, fmt_num(abs(gap), 1),
              if (gap > 0) "more" else "less", change_phrase(y, b * gap))
    }
  )

  # --- platform differences ---
  plat_items <- lapply(r$platforms, function(pl) {
    if (pl == ref_platform) {
      return(tags$li(
        tags$strong(pl, ": "),
        "this is the reference platform (the most common one in the data); other platforms are compared with it."
      ))
    }
    term <- paste0("platform", pl)
    if (!term %in% rownames(cf)) return(NULL)
    pb <- cf[term, "Estimate"]
    pp <- cf[term, "Pr(>|t|)"]
    tags$li(
      tags$strong(pl, ": "),
      sprintf("compared with %s users at the same screen time, %s users show %s (95%% CI %s to %s; %s, %s).",
              ref_platform, pl, change_phrase(y, pb),
              fmt_num(ci[term, 1]), fmt_num(ci[term, 2]), fmt_p(pp),
              if (pp < 0.05) "statistically clear" else "not statistically clear")
    )
  })
  plat_items <- Filter(Negate(is.null), plat_items)

  # --- how common is this level? ---
  common_item <- if (is_sleep) {
    tags$li(
      tags$strong("How common: "),
      sprintf("about %s of respondents slept less than your predicted amount, and %s slept under 7 hours, the minimum that sleep experts recommend for adults.",
              fmt_pct(mean(st$values < est)), fmt_pct(mean(st$values < 7)))
    )
  } else {
    tags$li(
      tags$strong("How common: "),
      sprintf("about %s of respondents scored at or above your predicted level, and %s scored 10 or higher, a cut-off often used to flag people for further assessment (a screening threshold, not a diagnosis).",
              fmt_pct(mean(st$values >= est)), fmt_pct(mean(st$values >= 10)))
    )
  }

  # --- how much does the model explain? ---
  r2 <- model_r2[[y]]
  fit_item <- tags$li(
    tags$strong("How much this explains: "),
    sprintf("screen time and platform together account for %s of the differences in %s between people (R\u00b2 = %.2f). Most of the variation comes from factors this model does not include, so treat the prediction as a rough guide, not a personal forecast.",
            variance_word(r2), tolower(info$label), r2)
  )

  div(
    class = "insight reveal",
    style = paste0("--i:", idx, "; --a1:", info$accent[1], "; --a2:", info$accent[2], ";"),
    h5(span(class = "insight-dot"), info$label),
    p(class = "insight-lead", headline, " ", vs_avg),
    tags$ul(slope_item, shift_item, plat_items, common_item, fit_item)
  )
}

#' Build the short overview shown above the per-outcome explanations.
#'
#' Purpose: summarise the predictions, state which outcomes show a
#' statistically clear association with screen time, and add the caveat
#' that these are associations, not causes.
#' @param r A list with `platforms` (character), `hours` (numeric) and
#'   `preds` (output of predict_all()).
#' @return A shiny tag (div) with the summary paragraphs.
build_takeaways <- function(r) {
  p_all <- r$preds
  est <- vapply(outcome_vars, function(y) mean(p_all$fit[p_all$outcome == y]), numeric(1))
  pv <- vapply(outcome_vars, function(y) model_coefs[[y]][screen_var, "Pr(>|t|)"], numeric(1))
  sl <- vapply(outcome_vars, function(y) model_coefs[[y]][screen_var, "Estimate"], numeric(1))

  describe <- c(
    Sleep_Duration_Hours = if (sl[1] < 0) "shorter sleep" else "longer sleep",
    GAD_7_Score = if (sl[2] < 0) "lower anxiety (GAD-7) scores" else "higher anxiety (GAD-7) scores",
    PHQ_9_Score = if (sl[3] < 0) "lower depression (PHQ-9) scores" else "higher depression (PHQ-9) scores"
  )
  clear <- outcome_vars[pv < 0.05]

  plat_txt <- if (length(r$platforms) == 1) {
    paste0("on ", r$platforms)
  } else {
    paste0("across ", join_and(r$platforms))
  }

  div(
    class = "takeaways",
    p(sprintf("Based on survey data from %s respondents, spending %.1f hours a day on screens %s goes with a predicted %.1f hours of sleep, a GAD-7 anxiety score of %.1f and a PHQ-9 depression score of %.1f.",
              format(n_total, big.mark = ","), r$hours, plat_txt, est[1], est[2], est[3])),
    p(if (length(clear) > 0) {
      paste0("In this dataset, more daily screen time is statistically associated with ",
             join_and(unname(describe[clear])), ".",
             if (length(clear) < 3) " The other outcomes show no statistically clear link with screen time." else "")
    } else {
      "In this dataset, none of the three outcomes shows a statistically clear linear link with daily screen time."
    }),
    p(class = "takeaway-caveat",
      "These are associations in observational survey data. They do not show that screen time or a platform causes these outcomes: the link may run the other way (poor sleep or low mood can lead to more screen use) or reflect other factors.")
  )
}

dark_theme <- theme_minimal(base_size = 13) +
  theme(
    text = element_text(colour = "#cfcbe8"),
    axis.text = element_text(colour = "#a29fc6"),
    axis.title = element_text(colour = "#a29fc6"),
    panel.grid.major = element_line(colour = "#ffffff14"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#f3f1ff", face = "bold", size = 13),
    plot.subtitle = element_text(colour = "#a29fc6"),
    legend.position = "bottom",
    legend.text = element_text(colour = "#cfcbe8"),
    legend.title = element_text(colour = "#a29fc6"),
    plot.background = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA)
  )


# ------------------------------------------------------------
# 6. STYLE
# ------------------------------------------------------------

app_css <- r"---(
:root {
  --bg: #0b0a1a; --bg2: #15122e;
  --glass: rgba(255,255,255,.045); --border: rgba(255,255,255,.10);
  --text: #ece9ff; --muted: #a29fc6;
  --violet: #8b5cf6; --pink: #f472b6; --cyan: #22d3ee;
}
html, body { height: 100%; }
body {
  margin: 0; overflow: hidden; color: var(--text);
  font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif;
  background: radial-gradient(1200px 800px at 15% 10%, #1c1646 0%, transparent 60%),
              radial-gradient(1000px 700px at 90% 90%, #2a1240 0%, transparent 55%),
              var(--bg);
}
.container-fluid { padding: 0; }

/* animated background orbs */
.bg-orbs { position: fixed; inset: 0; z-index: 0; overflow: hidden; pointer-events: none; }
.orb { position: absolute; border-radius: 50%; filter: blur(90px); opacity: .45; }
.orb.o1 { width: 440px; height: 440px; background: #6d4aff; top: -120px; left: -100px;
          animation: drift1 22s ease-in-out infinite alternate; }
.orb.o2 { width: 380px; height: 380px; background: #f472b6; bottom: -140px; right: -80px;
          opacity: .30; animation: drift2 26s ease-in-out infinite alternate; }
.orb.o3 { width: 320px; height: 320px; background: #22d3ee; top: 40%; left: 55%;
          opacity: .20; animation: drift3 30s ease-in-out infinite alternate; }
@keyframes drift1 { to { transform: translate(180px, 140px) scale(1.2); } }
@keyframes drift2 { to { transform: translate(-200px, -120px) scale(1.15); } }
@keyframes drift3 { to { transform: translate(-260px, 120px) scale(.85); } }

/* stages */
.stage {
  position: fixed; inset: 0; z-index: 1; overflow-y: auto; display: flex;
  padding: 40px 20px;
  opacity: 0; visibility: hidden; pointer-events: none;
  transform: translateY(24px) scale(.985);
  transition: opacity .7s ease, transform .7s ease, visibility 0s linear .7s;
}
.stage.active {
  opacity: 1; visibility: visible; pointer-events: auto; transform: none;
  transition: opacity .7s ease .15s, transform .7s ease .15s, visibility 0s;
}
.stage-inner { margin: auto; width: 100%; }

@keyframes fadeUp { from { opacity: 0; transform: translateY(26px); } to { opacity: 1; transform: none; } }
.stage.active .reveal { animation: fadeUp .8s cubic-bezier(.2,.7,.2,1) both; animation-delay: calc(var(--i, 0) * .14s + .2s); }

.glass {
  background: var(--glass); border: 1px solid var(--border); border-radius: 24px;
  backdrop-filter: blur(18px); -webkit-backdrop-filter: blur(18px);
  box-shadow: 0 20px 60px rgba(0,0,0,.35);
}
.grad-text {
  background: linear-gradient(90deg, #a78bfa, #f472b6 55%, #fbbf24);
  -webkit-background-clip: text; background-clip: text; color: transparent;
}

/* ---------- input stage ---------- */
.intro { max-width: 660px; margin: 0 auto; padding: 44px 40px; text-align: center; }
.intro h1 { font-size: 40px; font-weight: 800; margin: 0 0 10px; letter-spacing: -1px; }
.intro .lead { color: var(--muted); margin: 0 auto 34px; max-width: 480px; line-height: 1.55; }
.step-label { font-size: 12px; letter-spacing: .14em; text-transform: uppercase;
              color: var(--muted); margin: 0 0 14px; font-weight: 600; }
.step { margin-bottom: 34px; }

.chips-wrap .form-group { margin: 0; }
.chips-wrap .control-label { display: none; }
.chips .shiny-options-group { display: flex; flex-wrap: wrap; gap: 10px; justify-content: center; }
.chips .checkbox { margin: 0; padding: 0; }
.chips .checkbox label { padding: 0; margin: 0; min-height: 0; cursor: pointer; }
.chips .checkbox input { position: absolute; opacity: 0; pointer-events: none; margin: 0; }
.chips .checkbox label > span {
  display: flex; align-items: center; gap: 9px; padding: 11px 18px;
  border-radius: 999px; border: 1px solid var(--border);
  background: rgba(255,255,255,.04); color: var(--text); font-weight: 500;
  transition: all .25s ease;
}
.chips .checkbox label:hover > span { transform: translateY(-2px); border-color: rgba(255,255,255,.3); }
.chips .checkbox input:checked + span {
  background: linear-gradient(135deg, rgba(139,92,246,.35), rgba(244,114,182,.30));
  border-color: rgba(244,114,182,.7);
  box-shadow: 0 0 0 4px rgba(244,114,182,.12), 0 8px 24px rgba(139,92,246,.25);
  transform: translateY(-2px);
}
.chips .checkbox input:focus-visible + span { outline: 2px solid var(--cyan); outline-offset: 3px; }
.chip-dot { width: 9px; height: 9px; border-radius: 50%; display: inline-block; }
.chip-n { font-size: 11px; color: var(--muted); }
.chips-hint { color: #fb7185; font-size: 13px; height: 0; overflow: hidden; opacity: 0;
              transition: all .3s ease; margin-top: 0; }
.chips-hint.show { height: 20px; opacity: 1; margin-top: 12px; }
@keyframes shake { 0%,100% { transform: translateX(0); } 20% { transform: translateX(-9px); }
                   40% { transform: translateX(8px); } 60% { transform: translateX(-6px); }
                   80% { transform: translateX(4px); } }
.chips-wrap.shake { animation: shake .5s ease; }

.hours-readout { display: flex; align-items: baseline; justify-content: center; gap: 10px; margin-bottom: 18px; }
#hours-val { font-size: 64px; font-weight: 800; line-height: 1; letter-spacing: -2px; }
.hours-unit { color: var(--muted); font-size: 16px; }
#hours-range {
  -webkit-appearance: none; appearance: none; width: 100%; height: 10px; border-radius: 99px;
  outline: none; --pct: 50%; cursor: pointer;
  background: linear-gradient(90deg, var(--violet) 0%, var(--pink) var(--pct), rgba(255,255,255,.12) var(--pct));
}
#hours-range::-webkit-slider-thumb {
  -webkit-appearance: none; width: 30px; height: 30px; border-radius: 50%; background: #fff;
  border: 7px solid var(--pink); box-shadow: 0 0 0 7px rgba(244,114,182,.18), 0 4px 16px rgba(0,0,0,.5);
  transition: box-shadow .2s ease, transform .2s ease;
}
#hours-range::-webkit-slider-thumb:hover { transform: scale(1.08); box-shadow: 0 0 0 11px rgba(244,114,182,.2), 0 4px 16px rgba(0,0,0,.5); }
#hours-range::-moz-range-thumb {
  width: 16px; height: 16px; border-radius: 50%; background: #fff; border: 7px solid var(--pink);
  box-shadow: 0 0 0 7px rgba(244,114,182,.18), 0 4px 16px rgba(0,0,0,.5);
}
.slider-scale { display: flex; justify-content: space-between; color: var(--muted); font-size: 12px; margin-top: 10px; }

.btn-go {
  border: 0; border-radius: 999px; padding: 16px 40px; font-size: 17px; font-weight: 700;
  color: #fff; cursor: pointer; letter-spacing: .01em;
  background: linear-gradient(90deg, #8b5cf6, #f472b6);
  box-shadow: 0 10px 34px rgba(139,92,246,.45);
  transition: transform .25s ease, box-shadow .25s ease;
  animation: glow 2.8s ease-in-out infinite;
}
.btn-go:hover { transform: translateY(-3px) scale(1.03); box-shadow: 0 16px 44px rgba(244,114,182,.5); }
.btn-go:active { transform: translateY(0) scale(.99); }
@keyframes glow { 0%,100% { box-shadow: 0 10px 34px rgba(139,92,246,.45); }
                  50% { box-shadow: 0 10px 52px rgba(244,114,182,.6); } }
.fineprint { color: var(--muted); font-size: 12px; margin-top: 22px; }

/* ---------- loading stage ---------- */
.loader { text-align: center; }
.orbit { position: relative; width: 170px; height: 170px; margin: 0 auto 38px; }
.orbit .r { position: absolute; inset: 0; border-radius: 50%; border: 3px solid transparent; }
.orbit .r1 { border-top-color: #a78bfa; border-right-color: #a78bfa; animation: spin 1.6s linear infinite; }
.orbit .r2 { inset: 18px; border-bottom-color: #f472b6; border-left-color: #f472b6; animation: spin 2.1s linear infinite reverse; }
.orbit .r3 { inset: 36px; border-top-color: #22d3ee; animation: spin 1.2s linear infinite; }
.orbit .core { position: absolute; inset: 62px; border-radius: 50%;
  background: radial-gradient(circle, #f472b6, #8b5cf6); animation: pulse 1.4s ease-in-out infinite;
  box-shadow: 0 0 40px rgba(244,114,182,.6); }
@keyframes spin { to { transform: rotate(360deg); } }
@keyframes pulse { 0%,100% { transform: scale(.8); opacity: .8; } 50% { transform: scale(1.15); opacity: 1; } }
.load-msg { font-size: 20px; font-weight: 600; min-height: 30px; }
.load-msg.swap { animation: msgIn .5s ease both; }
@keyframes msgIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: none; } }
.load-bar { width: 260px; height: 5px; border-radius: 99px; background: rgba(255,255,255,.1); margin: 24px auto 0; overflow: hidden; }
.load-bar-fill { height: 100%; width: 0; border-radius: 99px; background: linear-gradient(90deg, #8b5cf6, #f472b6, #fbbf24); }
.stage.active .load-bar-fill { animation: fillbar 3.4s cubic-bezier(.4,.1,.3,1) forwards; }
@keyframes fillbar { to { width: 100%; } }

/* ---------- results stage ---------- */
.results { max-width: 1080px; margin: 0 auto; }
.results-head { text-align: center; margin-bottom: 30px; }
.results-head h2 { font-size: 34px; font-weight: 800; letter-spacing: -.8px; margin: 0 0 12px; }
.summary-pills { display: flex; flex-wrap: wrap; gap: 8px; justify-content: center; }
.pill { display: inline-flex; align-items: center; gap: 8px; padding: 7px 14px; border-radius: 999px;
        border: 1px solid var(--border); background: rgba(255,255,255,.05); font-size: 14px; }
.cards-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-bottom: 22px; }
.result-card {
  position: relative; padding: 24px 22px 22px; text-align: center; border-radius: 24px;
  background: linear-gradient(160deg, rgba(255,255,255,.07), rgba(255,255,255,.025));
  border: 1px solid var(--border); box-shadow: 0 20px 50px rgba(0,0,0,.35); overflow: hidden;
  transition: transform .3s ease, box-shadow .3s ease;
}
.result-card::before {
  content: ''; position: absolute; top: -60px; left: 50%; width: 220px; height: 120px; transform: translateX(-50%);
  background: radial-gradient(closest-side, var(--a1), transparent); opacity: .30; filter: blur(14px);
}
.result-card:hover { transform: translateY(-6px); box-shadow: 0 26px 60px rgba(0,0,0,.45); }
.card-label { position: relative; font-weight: 700; font-size: 15px; letter-spacing: .02em; color: var(--text); margin-bottom: 14px; }
.ring-wrap { position: relative; width: 170px; height: 170px; margin: 0 auto 16px; }
.ring { width: 100%; height: 100%; overflow: visible; }
.ring-bg { fill: none; stroke: rgba(255,255,255,.09); stroke-width: 11; }
.ring-fg { fill: none; stroke-width: 11; stroke-linecap: round;
           transition: stroke-dashoffset 1.8s cubic-bezier(.22,.8,.22,1) .3s;
           filter: drop-shadow(0 0 8px var(--a1)); }
.ring-center { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; }
.count-up { font-size: 42px; font-weight: 800; line-height: 1; letter-spacing: -1px; }
.ring-unit { font-size: 11px; color: var(--muted); margin-top: 4px; }
.badge { position: relative; display: inline-flex; align-items: center; gap: 8px; padding: 6px 14px; border-radius: 999px;
         background: rgba(255,255,255,.07); font-size: 13px; font-weight: 600; margin-bottom: 12px; }
.badge-dot { width: 9px; height: 9px; border-radius: 50%; box-shadow: 0 0 10px currentColor; }
.card-note { position: relative; font-size: 12px; color: var(--muted); line-height: 1.5; margin-top: 3px; }

.section-card { padding: 26px 28px; margin-bottom: 22px; }
.section-card h4 { margin: 0 0 16px; font-size: 18px; font-weight: 700; }
.glass table, .glass th, .glass td { background: transparent !important; color: var(--text); border-color: rgba(255,255,255,.08) !important; }
.glass table { width: 100%; font-size: 14px; border-collapse: collapse; }
.glass th { color: var(--muted); font-weight: 600; font-size: 12px; text-transform: uppercase; letter-spacing: .08em; padding: 8px 10px; text-align: left; }
.glass td { padding: 9px 10px; border-top: 1px solid rgba(255,255,255,.07); }
.table-scroll { overflow-x: auto; }

details.glass { padding: 20px 28px; margin-bottom: 22px; }
details.glass summary { cursor: pointer; font-weight: 700; font-size: 16px; list-style: none; }
details.glass summary::-webkit-details-marker { display: none; }
details.glass summary::after { content: '+'; float: right; color: var(--pink); font-size: 22px; line-height: 1; transition: transform .3s ease; }
details.glass[open] summary::after { transform: rotate(45deg); }
details.glass h5 { margin: 22px 0 10px; font-weight: 700; }
details.glass p { color: var(--muted); font-size: 14px; line-height: 1.6; }
details.glass code { background: rgba(255,255,255,.08); color: #f9a8d4; }

.disclaimer { border-left: 3px solid #fbbf24; background: rgba(251,191,36,.07); padding: 12px 16px;
              border-radius: 0 12px 12px 0; font-size: 13px; color: #e9e4ff; line-height: 1.55; margin-bottom: 26px; }
.btn-back {
  border: 1px solid var(--border); background: rgba(255,255,255,.06); color: var(--text);
  border-radius: 999px; padding: 12px 28px; font-weight: 600; cursor: pointer; transition: all .25s ease;
}
.btn-back:hover { background: rgba(255,255,255,.12); transform: translateY(-2px); }
.center { text-align: center; padding-bottom: 20px; }

.card-avg { color: var(--text); font-weight: 500; margin-bottom: 4px; }

/* interpretation section */
.takeaways { margin-bottom: 22px; padding: 18px 20px; border-radius: 16px;
             background: linear-gradient(135deg, rgba(139,92,246,.16), rgba(244,114,182,.10));
             border: 1px solid rgba(167,139,250,.25); }
.takeaways p { margin: 0 0 10px; line-height: 1.65; font-size: 15px; }
.takeaways p:last-child { margin-bottom: 0; }
.takeaways .takeaway-caveat { color: var(--muted); font-size: 13px; }
.insights-list { display: grid; gap: 16px; }
.insight { position: relative; padding: 20px 22px 14px 24px; border-radius: 16px;
           background: rgba(255,255,255,.035); border: 1px solid var(--border); overflow: hidden; }
.insight::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 4px;
                   background: linear-gradient(180deg, var(--a1), var(--a2)); }
.insight h5 { display: flex; align-items: center; gap: 10px; margin: 0 0 8px; font-size: 16px; font-weight: 700; color: var(--text); }
.insight-dot { width: 10px; height: 10px; border-radius: 50%; background: var(--a1); box-shadow: 0 0 12px var(--a1); }
.insight-lead { font-size: 15px; line-height: 1.6; margin: 0 0 10px; }
.insight ul { margin: 0; padding-left: 18px; }
.insight li { color: #cfcbe8; font-size: 13.5px; line-height: 1.6; margin-bottom: 8px; }
.insight li strong { color: var(--text); }

.shiny-bound-output.recalculating { opacity: 1 !important; }
@media (max-width: 560px) {
  .intro { padding: 32px 22px; } .intro h1 { font-size: 31px; } #hours-val { font-size: 52px; }
}
)---"


# ------------------------------------------------------------
# 7. CLIENT-SIDE LOGIC (stage switching and animations)
# ------------------------------------------------------------

app_js <- r"---(
(function () {
  var APP = { nTotal: __NTOTAL__ };

  function setStage(name) {
    document.querySelectorAll('.stage').forEach(function (s) {
      s.classList.toggle('active', s.id === 'stage-' + name);
    });
    var active = document.getElementById('stage-' + name);
    if (active) active.scrollTop = 0;
  }

  function updateHours() {
    var r = document.getElementById('hours-range');
    var v = parseFloat(r.value);
    document.getElementById('hours-val').textContent = v.toFixed(1);
    var pct = (v - r.min) / (r.max - r.min) * 100;
    r.style.setProperty('--pct', pct + '%');
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('hours', v);
  }

  function animateResults() {
    document.querySelectorAll('.count-up').forEach(function (el) {
      var target = parseFloat(el.dataset.value), dur = 1800, t0 = performance.now();
      (function tick(t) {
        var p = Math.min(1, (t - t0) / dur), e = 1 - Math.pow(1 - p, 3);
        el.textContent = (target * e).toFixed(1);
        if (p < 1) requestAnimationFrame(tick);
      })(t0);
    });
    document.querySelectorAll('.ring-fg').forEach(function (c) {
      var len = parseFloat(c.getAttribute('stroke-dasharray'));
      var frac = parseFloat(c.dataset.frac);
      void c.getBoundingClientRect();
      c.style.strokeDashoffset = len * (1 - frac);
    });
  }

  var messages = [
    'Reading your screen time\u2026',
    'Matching your platforms\u2026',
    'Comparing with ' + APP.nTotal.toLocaleString() + ' respondents\u2026',
    'Running the models\u2026',
    'Preparing your results\u2026'
  ];

  function startRun() {
    ['cards', 'takeaways', 'insights'].forEach(function (id) {
      var el = document.getElementById(id);
      if (el) el.innerHTML = '';
    });
    setStage('loading');
    Shiny.setInputValue('go', Date.now(), { priority: 'event' });

    var msg = document.getElementById('load-msg');
    var i = 0;
    function show() {
      msg.textContent = messages[Math.min(i, messages.length - 1)];
      msg.classList.remove('swap'); void msg.offsetWidth; msg.classList.add('swap');
      i++;
    }
    show();
    var timer = setInterval(show, 700);

    var started = Date.now();
    setTimeout(function wait() {
      var ready = document.querySelector('#cards .result-card') &&
                  document.querySelector('#insights .insight');
      if (!ready && Date.now() - started < 9000) { setTimeout(wait, 150); return; }
      clearInterval(timer);
      setStage('results');
      setTimeout(animateResults, 450);
    }, 3400);
  }

  document.addEventListener('DOMContentLoaded', function () {
    var r = document.getElementById('hours-range');
    r.addEventListener('input', updateHours);
    updateHours();

    document.getElementById('go-btn').addEventListener('click', function () {
      var n = document.querySelectorAll('.chips input:checked').length;
      if (n === 0) {
        var wrap = document.querySelector('.chips-wrap');
        wrap.classList.remove('shake'); void wrap.offsetWidth; wrap.classList.add('shake');
        document.getElementById('chips-hint').classList.add('show');
        return;
      }
      startRun();
    });

    document.querySelector('.chips').addEventListener('change', function () {
      document.getElementById('chips-hint').classList.remove('show');
    });

    document.getElementById('back-btn').addEventListener('click', function () { setStage('input'); });
  });

  if (window.jQuery) {
    jQuery(document).on('shiny:connected', updateHours);
  }
})();
)---"
app_js <- gsub("__NTOTAL__", as.character(n_total), app_js, fixed = TRUE)


# ------------------------------------------------------------
# 8. USER INTERFACE
# ------------------------------------------------------------

platform_names <- lapply(names(platform_n), function(p) {
  tagList(
    span(class = "chip-dot", style = paste0("background:", platform_colours[[p]], ";")),
    span(p),
    span(class = "chip-n", format(as.integer(platform_n[[p]]), big.mark = ","))
  )
})

slider_html <- HTML(paste0(
  '<div class="hours-readout"><span id="hours-val" class="grad-text">',
  sprintf("%.1f", screen_mean), '</span><span class="hours-unit">hours / day</span></div>',
  '<input type="range" id="hours-range" min="', screen_min, '" max="', screen_max,
  '" step="0.1" value="', screen_mean, '" aria-label="Daily screen time in hours">',
  '<div class="slider-scale"><span>', screen_min, ' h</span><span>', screen_max, ' h</span></div>'
))

ui <- bootstrapPage(
  title = "Screen time & wellbeing",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
    ),
    tags$style(HTML(app_css))
  ),

  div(class = "bg-orbs", div(class = "orb o1"), div(class = "orb o2"), div(class = "orb o3")),

  # ---------------- STAGE 1: INPUT ----------------
  div(
    id = "stage-input", class = "stage active",
    div(
      class = "stage-inner",
      div(
        class = "intro glass",
        h1(class = "grad-text reveal", style = "--i:0", "Screen time & wellbeing"),
        p(class = "lead reveal", style = "--i:1",
          "Tell us how you use your screen. We'll show what survey data from ",
          format(n_total, big.mark = ","),
          " people suggests about sleep, anxiety and mood."),
        div(
          class = "step reveal", style = "--i:2",
          p(class = "step-label", "1 \u00b7 Which platform(s) do you use?"),
          div(
            class = "chips-wrap",
            div(
              class = "chips",
              checkboxGroupInput(
                "platforms", label = NULL,
                choiceNames = platform_names,
                choiceValues = names(platform_n),
                selected = character(0)
              )
            )
          ),
          div(id = "chips-hint", class = "chips-hint", "Please pick at least one platform.")
        ),
        div(
          class = "step reveal", style = "--i:3",
          p(class = "step-label", "2 \u00b7 How many hours a day?"),
          slider_html
        ),
        div(class = "reveal", style = "--i:4",
            tags$button(id = "go-btn", class = "btn-go", type = "button", "Show my results \u2192")),
        p(class = "fineprint reveal", style = "--i:5",
          "Nothing you enter is stored. Results show statistical associations, not a diagnosis.")
      )
    )
  ),

  # ---------------- STAGE 2: LOADING ----------------
  div(
    id = "stage-loading", class = "stage",
    div(
      class = "stage-inner loader",
      div(class = "orbit",
          div(class = "r r1"), div(class = "r r2"), div(class = "r r3"), div(class = "core")),
      div(id = "load-msg", class = "load-msg", "Reading your screen time\u2026"),
      div(class = "load-bar", div(class = "load-bar-fill"))
    )
  ),

  # ---------------- STAGE 3: RESULTS ----------------
  div(
    id = "stage-results", class = "stage",
    div(
      class = "stage-inner",
      div(
        class = "results",
        div(class = "results-head reveal", style = "--i:0",
            h2(class = "grad-text", "Your predicted profile"),
            uiOutput("summary_pills", class = "summary-pills")),
        uiOutput("cards", class = "cards-grid"),
        div(class = "glass section-card reveal", style = "--i:4",
            h4("What your results mean"),
            uiOutput("takeaways"),
            uiOutput("insights", class = "insights-list")),
        div(class = "glass section-card reveal", style = "--i:5",
            h4("How it changes with screen time"),
            plotOutput("curve_plot", height = "360px")),
        div(class = "glass section-card reveal", style = "--i:6",
            h4("Predictions by platform"),
            div(class = "table-scroll", tableOutput("platform_table"))),
        tags$details(
          class = "glass reveal", style = "--i:7",
          tags$summary("About the model and caveats"),
          p("Each outcome is predicted with a multiple linear regression: ",
            code("outcome ~ Daily_Screen_Time_Hours + platform"), ". The reference platform is ",
            strong(ref_platform), " (the most common one); platform coefficients are differences from it. ",
            "Platforms with fewer than ", min_group_n, " respondents were excluded."),
          p("The data holds one platform per respondent, so if you tick several platforms the app ",
            "shows the average of the per-platform predictions, not a combined effect."),
          h5("Model fit"),
          div(class = "table-scroll", tableOutput("fit_table")),
          p("R-squared is the share of variation the model explains. Low values mean individuals ",
            "differ a lot from the prediction, which is why wide ranges are shown."),
          h5("Coefficients (95% confidence intervals)"),
          div(class = "table-scroll", tableOutput("coef_table")),
          p("GAD-7 (0\u201321) and PHQ-9 (0\u201327) are bounded, often skewed scores, so a linear model ",
            "is only an approximation. Predictions are clipped to the possible score range.")
        ),
        div(class = "disclaimer reveal", style = "--i:8",
            strong("Please note: "),
            "this is a statistical model of associations in survey data. It does not show that screen ",
            "time or a platform causes these outcomes, and it is not a diagnosis or medical advice. ",
            "If you are worried about your mental health, please talk to a healthcare professional."),
        div(class = "center reveal", style = "--i:9",
            tags$button(id = "back-btn", class = "btn-back", type = "button", "\u2190 Try different inputs"))
      )
    )
  ),

  tags$script(HTML(app_js))
)


# ------------------------------------------------------------
# 9. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {

  # Results are computed when the user presses the button.
  run <- eventReactive(input$go, {
    req(input$platforms, input$hours)
    list(
      platforms = input$platforms,
      hours = input$hours,
      preds = predict_all(input$platforms, input$hours)
    )
  })

  output$summary_pills <- renderUI({
    r <- run()
    tagList(
      lapply(r$platforms, function(p) {
        span(class = "pill",
             span(class = "chip-dot", style = paste0("background:", platform_colours[[p]], ";")),
             p)
      }),
      span(class = "pill", sprintf("%.1f h / day", r$hours))
    )
  })

  output$cards <- renderUI({
    r <- run()
    p <- r$preds
    tagList(
      outcome_card("Sleep_Duration_Hours", p[p$outcome == "Sleep_Duration_Hours", ], 1),
      outcome_card("GAD_7_Score", p[p$outcome == "GAD_7_Score", ], 2),
      outcome_card("PHQ_9_Score", p[p$outcome == "PHQ_9_Score", ], 3)
    )
  })

  output$takeaways <- renderUI({
    build_takeaways(run())
  })

  output$insights <- renderUI({
    r <- run()
    tagList(
      build_insight("Sleep_Duration_Hours", r, 1),
      build_insight("GAD_7_Score", r, 2),
      build_insight("PHQ_9_Score", r, 3)
    )
  })

  output$platform_table <- renderTable({
    r <- run()
    labels <- vapply(outcome_vars, function(o) outcome_info[[o]]$label, character(1))
    r$preds %>%
      mutate(
        Outcome = unname(labels[outcome]),
        Prediction = sprintf("%.1f", fit),
        `95% CI (average person)` = sprintf("%.1f \u2013 %.1f", ci_low, ci_high),
        `95% range (individual)` = sprintf("%.1f \u2013 %.1f", pred_low, pred_high)
      ) %>%
      arrange(platform, match(Outcome, labels)) %>%
      select(Platform = platform, Outcome, Prediction,
             `95% CI (average person)`, `95% range (individual)`)
  }, spacing = "s", width = "100%")

  output$curve_plot <- renderPlot({
    r <- run()
    grid <- seq(screen_min, screen_max, length.out = 60)
    labels <- vapply(outcome_vars, function(o) outcome_info[[o]]$label, character(1))

    curve_data <- bind_rows(lapply(r$platforms, function(pl) {
      newdata <- data.frame(Daily_Screen_Time_Hours = grid,
                            platform = factor(pl, levels = levels(df$platform)))
      names(newdata)[1] <- screen_var
      bind_rows(lapply(outcome_vars, function(y) {
        info <- outcome_info[[y]]
        data.frame(hours = grid, platform = pl, outcome = info$label,
                   value = clamp(predict(models[[y]], newdata), info$lower, info$upper))
      }))
    }))
    curve_data$outcome <- factor(curve_data$outcome, levels = unname(labels))

    current <- r$preds %>%
      mutate(outcome = factor(unname(labels[outcome]), levels = unname(labels)),
             hours = r$hours)

    ggplot(curve_data, aes(x = hours, y = value, colour = platform)) +
      geom_vline(xintercept = r$hours, linetype = "dashed", colour = "#ffffff55") +
      geom_line(linewidth = 1.3, lineend = "round") +
      geom_point(data = current, aes(y = fit), size = 4.5, shape = 21,
                 fill = "white", stroke = 1.6) +
      facet_wrap(~ outcome, scales = "free_y", ncol = 3) +
      scale_colour_manual(values = platform_colours) +
      labs(x = "Daily screen time (hours)", y = "Predicted value", colour = NULL,
           subtitle = "Dots and dashed line mark your inputs") +
      dark_theme
  }, bg = "transparent", res = 110)

  output$fit_table <- renderTable(model_summary_table, spacing = "s", width = "100%")
  output$coef_table <- renderTable(coef_table, spacing = "s", width = "100%")

  # Keep outputs live even while their stage is hidden.
  for (o in c("summary_pills", "cards", "takeaways", "insights",
              "platform_table", "curve_plot",
              "fit_table", "coef_table")) {
    outputOptions(output, o, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
