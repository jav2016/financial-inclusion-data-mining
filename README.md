Financial Inclusion – Data Mining Project

This repository presents a data mining project that analyses the determinants of financial inclusion using supervised classification techniques in R. The focus is on understanding which factors are most strongly associated with financial account ownership in high-income economies.

Project Overview
Financial inclusion plays a key role in economic participation and access to formal financial services. This project applies data mining methods to identify patterns and predictors of account ownership using survey-based indicators related to demographics, digital access, and financial behaviour.

The analysis was conducted as part of an MSc Data Mining module and is presented here as a professional portfolio representation.

Dataset (not included)
- Source: World Bank – Global Findex Database (2025 edition)
- Survey year: 2024
- Scope used in analysis:
  - 2,630 observations
  - 51 high-income countries
  - United Kingdom subset included for comparison

> The raw dataset is not uploaded to this repository due to licensing, size, and academic integrity considerations.

Methods
The following supervised classification models were implemented and evaluated:

- K-Nearest Neighbours (KNN)
- Decision Tree
- Random Forest

Model performance was assessed using train/test split, cross-validation, accuracy, and Cohen’s Kappa.

Key Results (high-level)
- Decision Tree and Random Forest achieved very high predictive accuracy (≈99.8%)
- KNN also performed strongly with accuracy above 99%
- The UK showed a higher rate of account ownership compared to the average of other high-income economies
- Digital and behavioural indicators were among the most influential predictors in tree-based models

Detailed visual outputs are provided in the `outputs/` folder.

Repository Structure
