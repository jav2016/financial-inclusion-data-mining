# Financial Inclusion - Data Mining Project

This is my MSc Data Mining project about financial inclusion in high-income economies.

The aim was to use R to explore which factors are linked with financial account ownership.

## Data

The script builds the dataset from the World Bank Global Findex / Data360 files.

The raw dataset is not uploaded here because it is better to download it from the official source when running the script.

## What I used

- R / RStudio
- World Bank Global Findex indicators
- KNN
- Decision Tree
- Random Forest
- Confusion matrices and model comparison

## Main Files

- `src/financial_inclusion_rstudio.R` - main R script
- `outputs/` - selected figures from the analysis
- `docs/executive_summary.pdf` - short project summary

## How to Run

Open `src/financial_inclusion_rstudio.R` in RStudio and run the script section by section.

The script installs missing R packages, downloads the data, prepares the modelling dataset, and then runs the models.

## Notes

The model results should be treated as coursework analysis, not as a real financial prediction system.

One older diabetes lab notebook is still in this repo at the moment. It is not part of this financial inclusion project and should be removed or moved later.
