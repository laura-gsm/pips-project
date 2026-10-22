# pips-project

This repository contains our group project for the Programming in Psychological Science (PIPS) course of the Research Master Psychology at the University of Amsterdam.

We build an app that lets users explore how time spent on social media apps (Snapchat, Instagram, TikTok, Facebook, Pinterest) relates to physical and mental health outcomes (e.g. depression, anxiety, sleep problems, physical activity). The project combines two data sources:

- **Simulated data**, generated with a script so that users can see how the analysis behaves when the true effects are known.
- **Real survey data** from the HBSC study (Health Behaviour in School-aged Children), cleaned and reduced to the variables relevant to our question.

> Note: results describe associations, not causal effects.

**Group members:** Laura, Julia, Helle

## Citation

[TODO: add citation for this repository]

**Data source:** [TODO: full HBSC citation and link to where the raw data comes from]

## Getting started

Clone the repository:

```
git clone https://github.com/laura-gsm/pips-project.git
cd pips-project
```

This project uses **R** (version [TODO] or newer) and RStudio is recommended.

1. Open `pips-project.Rproj` in RStudio. This sets the working directory to the project folder so the data files are found.
2. Install the required packages (see Dependencies below). The scripts install missing packages automatically.
3. Open `main.Rmd` [TODO: confirm file name] and run it from top to bottom.

[TODO: add one line on how to start the app, and whether it opens in a browser window]

## Repository structure

```
pips-project/
├── data/                 # Cleaned data used by the project [TODO: confirm folder]
├── src/                  # Supporting R scripts (simulation, analysis, plots, app)
├── output/               # Saved tables and figures [TODO: if we add this folder]
├── main.Rmd              # Main deliverable (explanations + calls to src/)
├── pips-project.Rproj    # RStudio project file
├── .gitignore
└── README.md
```

[TODO: update this tree once the final structure is decided]

## Using the project

`main.Rmd` guides the user through:

1. Loading and describing the data (simulated and HBSC).
2. Running the regression analyses (app use predicting each health outcome, adjusted for age and gender) and viewing the plots.
3. Launching the app to explore the relationships interactively.

[TODO: update this list as the main file takes shape]

## Data

- **Simulated data:** created by `src/[TODO: script name]`, with a fixed random seed for reproducibility.
- **HBSC data:** the raw file (122 variables, 420,881 participants) was too large for GitHub, so we reduced it. `data/[TODO: file name]` is the cleaned version, with 33 variables and 180,397 participants. The reduction involved:
  1. **Selecting variables:** we kept only the 33 variables relevant to our question (mental health, physical health, and social media use) and dropped the rest.
  2. **Recoding missing values:** the original data codes missing answers as 99, which we set to NA.
  3. **Removing participants with many missing answers:** some participants had missing answers on many of the 33 variables, so we excluded participants with more than 2 missing answers [TODO: check this matches the cleaning code; the draft script keeps rows with at most 2 NAs].

[TODO: describe the key variables and how app use / health outcomes are measured]

## Dependencies

[TODO: confirm final list]

- R packages for simulation and analysis: MASS, tidyverse, broom, car, sandwich, lmtest
- App: [TODO: e.g. shiny]
- Main document: knitr, rmarkdown

## Team responsibilities

| Member | Primary responsibility |
|---|---|
| Laura | Project structure, `main` file, README |
| Julia | Data simulation / app |
| Helle | Statistical analyses and visualisations |

All members contributed through pull requests and reviewed each other's work on GitHub.

## References

[TODO: add references for the HBSC study and any papers cited in main]
