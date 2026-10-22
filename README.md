# pips-project

This repository contains our group project for the Programming in Psychological Science (PIPS) course of the Research Master Psychology at the University of Amsterdam.

We study how social media use relates to sleep, anxiety (GAD-7) and depression (PHQ-9). The project contains:

- **Main analysis (adults):** correlations, regressions and platform comparisons of daily screen time and primary platform with sleep, GAD-7 and PHQ-9, using the Kaggle *Social Media and Mental Health* dataset.
- **Shiny app:** an interactive tool where users select their platform(s) and daily screen time and see what the adult data suggest for sleep, anxiety and depression.
- **Extra: adolescents (HBSC):** the same kind of question in the Health Behaviour in School-aged Children data.
- **Extra: simulation check:** simulated data with known effects, used to check that our regression approach recovers them.

> **Note:** results describe statistical associations, not causal effects, and are not a diagnosis.

**Group members:** Laura, Julia, Helle

## Citation

```text
Garcia Sanchez de Mora, L., Van Gaalen, J., & Tveit, H. (2026). pips-project: Social media use, sleep, anxiety and depression [Computer software]. GitHub. https://github.com/laura-gsm/pips-project
```

**Data sources:**

- Social Media and Mental Health [Data set]. Kaggle. https://www.kaggle.com/datasets/bertnardomariouskono/social-media-and-mental-health
- Health Behaviour in School-aged Children (HBSC) study. https://www.hbsc.org/data

## Getting started

Clone the repository:

```bash
git clone https://github.com/laura-gsm/pips-project.git
cd pips-project
```

This project uses R 4.0 or newer, and RStudio is recommended.

1. Open `pips-project.Rproj` in RStudio. This sets the working directory to the project folder so the data files are found.
2. Open `main.Rmd`. The the #setup chunk of the main.Rmd installs missing packages (see Dependencies below).
3. Click "Knit" at the top. This runs the main.Rmd file from top to bottom. The analyses take a short while, because the HBSC file is large.
4. To start the app, run the chunk in the "The Shiny app" section manually, or run `shiny::runApp("src")` in the console. It opens in a browser window or the RStudio viewer. This chunk is not evaluated when knitting. Press the stop button in the console before running further chunks.

## Repository structure

```text
## Repository structure

```text
pips-project/
├── data/
│   ├── alternative_dataset.csv           # Main dataset (adults, Kaggle)
│   ├── hbsc_data.csv                     # Extra: cleaned HBSC data (adolescents)
│   ├── hbsc_data_descriptions.pdf        # Extra: HBSC variable descriptions
│   └── gitkeep
├── src/
│   ├── run_screen_time_analysis.R        # Main analysis function
│   ├── app.R                             # Shiny app
│   ├── run_hbsc_analysis.R               # Extra: HBSC analysis function
│   ├── run_social_media_analysis.R       # Extra: simulation check function
│   ├── Children_data_analysis_final_version.R    # Earlier HBSC script
│   ├── improved_social_media_analysis_Debugged.R # Earlier simulation script
│   ├── NEWDATA_ANALYSIS_FINAL.r          # Earlier adult-data script
│   ├── actualdata_R_CODE_REVISED.R       # Earlier adult-data script
│   └── .gitkeep
├── outputs/                              # Tables, figures and reports saved by the functions (created when run)
├── main.Rmd                              # Main deliverable (explanations + calls to src/)
├── pips-project.Rproj                    # RStudio project file
├── .gitignore
└── README.md
```

Only `main.Rmd`, the three `run_*` functions and `app.R` are needed to run the project. The other scripts in `src/` are earlier versions that the `run_*` functions were built from, and are kept for reference.
```

Only `main.Rmd`, the three `run_*` functions and `app.R` are needed to run the project. The other scripts in `src/` are earlier versions that the `run_*` functions were built from, and are kept for reference.
```

## Using the project

`main.Rmd` guides the user through the following steps. Each analysis is one function in `src/`. The function also saves its tables, figures and a text report to a subfolder of `outputs/`.

1. **Main analysis (adults):** `run_screen_time_analysis()` runs
   - Spearman correlations between daily screen time and sleep, GAD-7 and PHQ-9, with Benjamini-Hochberg (BH) correction;
   - linear regressions of each outcome on daily screen time and platform (reference group is the most common platform; platforms with fewer than 30 respondents are removed);
   - Kruskal-Wallis and pairwise Wilcoxon tests of platform differences (BH-corrected);
   - tests of whether the screen-time slope differs by platform.

   No covariates such as age or gender are included. Regression p-values and confidence intervals are not corrected for multiple testing. Two figures are shown: modelled trends across screen time, and distributions by platform.
2. **App:** `src/app.R` uses the same models as the main analysis. Users tick their platform(s) and set their daily screen time, and see predictions with 95% confidence and prediction intervals, curves across screen time, and model tables. If several platforms are ticked, the app shows the average of the per-platform predictions, because each respondent in the data has one platform.
3. **Extra: adolescents (HBSC):** `run_hbsc_analysis()` sums eight social media items into a score from 0 to 8 and relates it to four outcomes with Spearman correlations and unadjusted linear regressions (BH-corrected across four tests).
4. **Extra: simulation check:** `run_social_media_analysis()` simulates fictional screen time on five apps (Snapchat, Instagram, TikTok, Facebook, Pinterest), age, gender and four health outcomes with effects that we impose. It fits regressions with Holm correction and an HC3 robust-standard-error sensitivity analysis. The main document compares the estimates with the true effects.

## Data

**Main dataset (adults):** `data/alternative_dataset.csv`, a local copy of the Kaggle dataset "Social Media and Mental Health" (https://www.kaggle.com/datasets/bertnardomariouskono/social-media-and-mental-health). It contains 8,000 records collected via online surveys. Per respondent it contains age, gender, primary platform, daily screen time (hours), sleep duration (hours), GAD-7 (0-21), PHQ-9 (0-27), and further variables that the analysis does not use (activity type, late-night usage, social comparison trigger, among others). The reported analyses examine daily screen time, social media platform, sleep duration, GAD-7 scores and PHQ-9 scores. `run_screen_time_analysis()` checks that the required columns exist and are numeric where expected. It does not delete or impute data. Results describe statistical associations and should not be interpreted as causal effects or clinical diagnoses.

** Extra: adolescent data: ** 
1. **Selecting variables:** we kept only the 33 variables relevant to our question (mental health, physical health and social media use) and dropped the rest.
2. **Recoding missing values:** the original data codes missing answers as 99, which we set to NA.
3. **Removing participants with many missing answers:** we excluded participants with more than 2 missing answers. 

The analysis uses the eight social media items `emcsocmed2` to `emcsocmed9` (coded 1 = no, 2 = yes), which are recoded to 0/1 and summed into a score from 0 to 8 for participants who answered all eight. The four outcomes are `feellow`, `nervous`, `sleepdificulty` and `physact60`. Outcome coding is unchanged, so see `data/hbsc_data_descriptions.pdf` for the direction of each variable. Survey weights and clustering are not used. 

**Extra: simulated data:** created inside `run_social_media_analysis()` with a fixed random seed (2026) for reproducibility. The effects are fictional and chosen by us, so the simulation checks the method and gives no evidence about real effects.

**Why two real datasets:** we first worked with the adolescent HBSC data, then built the app on an adult dataset because the app targets adults. The two datasets measure different things in different populations, so they are analysed separately and not pooled.

## Dependencies

The #setup chunk of the main.Rmd installs missing packages. The app also installs missing packages when it starts.

- Packages: ggplot2, dplyr, tidyr, purrr, stringr, tibble, readr, broom, car
- Simulation: MASS, sandwich, lmtest
- App: shiny (plus ggplot2, dplyr, stringr, tidyr)
- Main document: knitr, rmarkdown

## Team responsibilities

| Member | Primary responsibility
|---|---|
| Laura | Project structure, `main.Rmd`, README 
| Julia | Statistical analyses, visualisations, and simulation
| Helle | Building the app 

All members participated in all roles and tasks
All members contributed through pull requests and reviewed each other's work on GitHub.

## References

- Social Media and Mental Health [Data set]. Kaggle. https://www.kaggle.com/datasets/bertnardomariouskono/social-media-and-mental-health
- Health Behaviour in School-aged Children (HBSC) study. https://www.hbsc.org/data
