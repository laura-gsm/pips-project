# pips-project

This repository contains our group project for the Programming in Psychological Science (PIPS) course of the Research Master Psychology at the University of Amsterdam.

We built a Shiny app that lets users enter the social media platform(s) they use and their daily screen time, and shows what survey data from adults suggests about sleep, anxiety (GAD-7) and depression (PHQ-9). The repository also contains a regression analysis of the same data (the main analysis). Two extras are included: an analysis of adolescent data from the HBSC study, and a simulation check of the analysis method.

**Note: results describe statistical associations, not causal effects, and are not a diagnosis.**

Group members: Laura, Julia, Helle

## Citation

```text
[TODO: add citation for this repository]
```

Main data source: Social Media and Mental Health dataset on Kaggle, https://www.kaggle.com/datasets/bertnardomariouskono/social-media-and-mental-health

## Getting started

Clone the repository:

```bash
git clone https://github.com/laura-gsm/pips-project.git
cd pips-project
```

This project uses R (version 4.0 or newer, because the app uses raw strings) and RStudio is recommended.

1. Open `pips-project.Rproj` in RStudio. This sets the working directory to the project folder so the data files are found.
2. Install the required packages (see Dependencies below).
3. Open `main.Rmd` and run it from top to bottom (or click Knit).
4. The app chunk in `main.Rmd` is not run when knitting. To start the app, run `shiny::runApp("src")` (or run that chunk manually). It opens in a browser window or the RStudio viewer. Stop the app with the stop button in the console before running further chunks.

## Repository structure

```text
pips-project/
├── data/
│   ├── alternative_dataset.csv           # Main dataset (adults, Kaggle)
│   └── hbsc_data.csv                     # Extra: cleaned HBSC data
│   └── hbsc_data_descriptions.pdf        # Extra: HBSC data description document
├── src/
│   ├── analysis.R                        # Main analysis: regressions, plots, validation
│   ├── app.R                             # Shiny app
│   ├── hbsc_analysis.R                   # Extra: analysis of HBSC data [TODO: confirm name]
│   └── simulate_data.R                   # Extra: simulation check [TODO: confirm name]
├── outputs/                              # Tables and figures saved by analysis.R
├── main.Rmd                              # Main deliverable (explanations + calls to src/)
├── pips-project.Rproj                    # RStudio project file
├── .gitignore
└── README.md
```

## Using the project

`main.Rmd` guides the user through:

1. **Main analysis (adult data):** `run_analysis()` in `src/analysis.R` fits linear regressions for sleep duration, GAD-7 and PHQ-9 on daily screen time, adjusted for age, gender, late-night use, social comparison trigger and activity type. It reports heteroskedasticity-robust (HC3) 95% confidence intervals, Holm-adjusted p-values for the three screen-time tests, an 80/20 held-out validation, residual diagnostics, and two figures (adjusted effects, and a binned plot of sleep across screen-time levels). Tables and figures are saved to `outputs/`.
2. **App:** an interactive model that is simpler than the main analysis (no covariates, so its numbers differ from the adjusted estimates above). `src/app.R` fits `outcome ~ daily screen time + platform` for each outcome, with the most common platform as the reference group and platforms with fewer than 30 respondents removed. Users tick their platform(s) and set their daily screen time, and see predictions with 95% confidence and prediction intervals, curves across screen time, and model tables. If several platforms are ticked, the app shows the average of the per-platform predictions, because each respondent in the data has one platform.
3. **Extra: adolescents (HBSC):** the same kind of question in adolescent data, which also contains physical health variables.
4. **Extra: simulation check:** simulated data with known effects, used to check that the regression recovers them.

## Data

**Main dataset (adults):** `data/alternative_dataset.csv`, from the Kaggle dataset "Social Media and Mental Health" (link above). It contains, per respondent: daily screen time (hours), social media platform, sleep duration (hours), GAD-7 anxiety score (0-21), PHQ-9 depression score (0-27), and the covariates age, gender, activity type, late-night usage (0/1) and social comparison trigger (0/1). `analysis.R` checks that required columns exist, that values are in range, and that there are no missing values or duplicate IDs, and stops with an error otherwise. It does not delete or impute data. [TODO: state the number of respondents, and whether the Kaggle data are real survey data or synthetic, as described on the Kaggle page.]

**Extra: HBSC data (adolescents):** the Health Behaviour in School-aged Children study. The raw file (122 variables, 420,881 participants) was too large for GitHub, so we reduced it. `data/[TODO: file name]` is the cleaned version, with 33 variables and 180,397 participants. The reduction involved:

- Selecting variables: we kept only the 33 variables relevant to our question (mental health, physical health and social media use) and dropped the rest.
- Recoding missing values: the original data codes missing answers as 99, which we set to NA.
- Removing participants with many missing answers: we excluded participants with more than 2 missing answers

[TODO: describe the key HBSC variables (social media items `emcsocmed`, and the outcomes) and how they are measured. Also state whether the file may be shared publicly; if not, remove it from `data/` and explain here how to obtain it.]

**Extra: simulated data:** created by `src/[TODO: script name]` with a fixed random seed (2026) for reproducibility. The effects are fictional and chosen by us, so the simulation checks the method and does not provide evidence about real effects.

**Why two real datasets:** we first worked with the adolescent HBSC data, then built the app on an adult dataset because the app targets adults. The HBSC data is kept as an extra to look at the same question in adolescents, including physical health variables that the adult dataset does not have. The two datasets measure different things in different populations, so they are analysed separately and not pooled.

## Dependencies

- Main analysis (`src/analysis.R`): `ggplot2`, `sandwich`. Install them first with `install.packages(c("ggplot2", "sandwich"))`; the script stops with a message if they are missing.
- App (`src/app.R`): `shiny`, `ggplot2`, `dplyr`, `stringr`, `tidyr`. The app installs missing packages automatically.
- Extras (simulation and HBSC scripts): `MASS`, `tidyverse`, `broom`, `car`, `lmtest`, `psych` [TODO: confirm final list].
- Main document: `knitr`, `rmarkdown`.

## Team responsibilities

| Member | Primary responsibility |
|--------|------------------------|
| Laura  | Project structure, main file, README |
| Julia  | Data simulation / app | Also contributed to data analyses
| Helle  | Statistical analyses and visualisations | Also contributed to building app

All members contributed to other responsibilities and roles. 

All members contributed through pull requests and reviewed each other's work on GitHub.

## References

- Social Media and Mental Health [Data set]. Kaggle. https://www.kaggle.com/datasets/bertnardomariouskono/social-media-and-mental-health
- Health Behavior in School Aged Children Study. https://www.hbsc.org/data
