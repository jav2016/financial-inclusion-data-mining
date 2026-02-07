# =============================================================================
# Financial Inclusion Analysis - Data Mining Project (CMP7206)
# RStudio Interactive Version - Plots display in Plots pane
# Authors: Javeria Butt
# University: Birmingham City University
# Date: December 2025
# =============================================================================

# INSTRUCTIONS: Run this script section by section in RStudio (Ctrl+Enter)
# Or run entire script with Ctrl+Shift+Enter

# -----------------------------------------------------------------------------
# 1. LOAD REQUIRED LIBRARIES
# -----------------------------------------------------------------------------

# Load required packages (install if needed)
required_packages <- c("tidyverse", "caret", "class", "rpart", "rpart.plot",
                       "randomForest", "e1071", "corrplot", "ggplot2",
                       "gridExtra", "scales")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Set seed for reproducibility
set.seed(42)

cat("\n*** Libraries loaded successfully ***\n")

# =============================================================================
# 1.5  AUTO-DATA: Build DATASET.csv from the World Bank Data360 (runs anywhere)
#      Place this block AFTER your libraries (Section 1) and BEFORE Section 2.
# =============================================================================

# --- Config ---
wb_base <- "https://data360files.worldbank.org/data360-data/data/WB_FINDEX/"
force_refresh <- FALSE  # set TRUE to re-download even if DATASET.csv exists

# Map: Data360 indicator code  -> your desired column name in DATASET.csv
ind_map <- c(
  "ACCOUNT_T_D"        = "account_t_d",          # Account ownership
  "FIACCOUNT_T_D"      = "fiaccount_t_d",        # Bank or similar financial institution account
  "MOBILEACCOUNT_T_D"  = "mobileaccount_t_d",    # Mobile money account
  "DIG_ACC"            = "dig_acc",              # Digitally enabled account (2024 wave)
  "FIN27A"             = "fin27a",               # Paid online for online purchase
  "FIN27B"             = "fin27b",               # Cash on delivery for online purchase
  "FIN3"               = "merchant_pay",         # Used a card or phone to make payments
  "BORROW_ANY_T_D"     = "borrow_any_t_d",       # Borrowed any money (any source)
  "CON26M"             = "internet"              # Internet use (monthly); proxy for 'internet'
)
# (Optional extras: you can add FIN26A, etc., later; the script already uses intersect() so it's robust.)

# Income classification (FY26; published July 2025) for 'incomegroupwb24'
wb_class_xlsx <- "https://datacatalogfiles.worldbank.org/ddh-published/0037712/DR0095333/CLASS_2025_07_02.xlsx"

# --- Helper to read indicator (long CSV first, then WIDEF) and tidy it ---
read_indicator <- function(code){
  # Try long CSV first
  urls <- c(
    paste0(wb_base, "WB_FINDEX_", code, ".csv"),
    paste0(wb_base, "WB_FINDEX_", code, "_WIDEF.csv")
  )
  df <- NULL; used_wide <- FALSE
  for (u in urls) {
    try({
      tmp <- suppressWarnings(readr::read_csv(u, show_col_types = FALSE, guess_max = 100000))
      df <- tmp
      used_wide <- grepl("_WIDEF\\.csv$", u, ignore.case = TRUE)
      attr(df, "source_url") <- u
      break
    }, silent = TRUE)
  }
  if (is.null(df)) return(NULL)
  
  # harmonize to long {REF_AREA, REF_AREA_LABEL, TIME_PERIOD, value}
  if (used_wide) {
    # WIDEF: year columns like 2011,2014,2017,2021,2022,2024...
    year_cols <- names(df)[grepl("^\\d{4}$", names(df))]
    if (length(year_cols) == 0) return(NULL)
    df <- tidyr::pivot_longer(df, dplyr::all_of(year_cols),
                              names_to = "TIME_PERIOD",
                              values_to = "OBS_VALUE")
  } else {
    # long: should already have TIME_PERIOD and OBS_VALUE (or last numeric col)
    if (!"OBS_VALUE" %in% names(df)) {
      # Try last column as value if OBS_VALUE absent
      valcol <- tail(names(df), 1)
      df$OBS_VALUE <- df[[valcol]]
    }
  }
  
  # Keep totals (country total, 15+)
  keep_if <- function(col) {
    if (!col %in% names(df)) return(rep(TRUE, nrow(df)))
    df[[col]] %in% c("_T", "Total", NA)
  }
  df <- df %>%
    dplyr::filter(
      keep_if("SEX"),
      keep_if("URBANISATION"),
      keep_if("COMP_BREAKDOWN_1"),
      keep_if("COMP_BREAKDOWN_2"),
      keep_if("COMP_BREAKDOWN_3"),
      # AGE: prefer 15+ where available
      if ("AGE" %in% names(.)) AGE %in% c("Y_GE15", "_T", "Total") else TRUE
    ) %>%
    dplyr::select(
      REF_AREA_LABEL = dplyr::any_of(c("REF_AREA_LABEL","COUNTRY","Country","Ref_Area_Label")),
      REF_AREA       = dplyr::any_of(c("REF_AREA","Code","ISO3")),
      TIME_PERIOD    = dplyr::any_of(c("TIME_PERIOD","Year")),
      OBS_VALUE
    ) %>%
    dplyr::mutate(
      TIME_PERIOD = suppressWarnings(as.integer(TIME_PERIOD)),
      OBS_VALUE   = suppressWarnings(as.numeric(OBS_VALUE))/100  # convert % --> proportion
    ) %>%
    dplyr::filter(!is.na(REF_AREA), !is.na(TIME_PERIOD))
  
  if (nrow(df) == 0) return(NULL)
  df
}

# --- Build a single panel with all indicators ---
build_dataset <- function(ind_map) {
  lst <- list()
  for (code in names(ind_map)) {
    cat(sprintf("Downloading %-22s -> %-20s ... ",
                paste0("WB_FINDEX_", code), ind_map[[code]]))
    di <- read_indicator(code)
    if (is.null(di)) {
      cat("FAILED\n"); next
    }
    # rename OBS_VALUE to the target column
    di <- di %>% dplyr::rename(!!ind_map[[code]] := OBS_VALUE)
    cat("OK\n")
    lst[[ind_map[[code]]]] <- di
  }
  
  if (length(lst) == 0) stop("No indicators could be downloaded.")
  
  # Full outer join on REF_AREA + TIME_PERIOD to keep maximal coverage
  keys <- Reduce(function(x, y) dplyr::full_join(x, y,
                                                 by = c("REF_AREA","REF_AREA_LABEL","TIME_PERIOD")),
                 lst)
  
  # Attach income groups (FY26) -> incomegroupwb24
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl", repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(readxl))
  cl_tmp <- tempfile(fileext = ".xlsx")
  utils::download.file(wb_class_xlsx, cl_tmp, mode = "wb", quiet = TRUE)
  wb_cls <- readxl::read_xlsx(cl_tmp, sheet = "List of economies")
  wb_cls <- wb_cls %>%
    dplyr::select(codewb = `Code`, incomegroupwb24 = `Income group`) %>%
    dplyr::mutate(codewb = as.character(codewb))
  
  out <- keys %>%
    dplyr::rename(countrynewwb = REF_AREA_LABEL,
                  codewb       = REF_AREA,
                  year         = TIME_PERIOD) %>%
    dplyr::left_join(wb_cls, by = "codewb") %>%
    dplyr::arrange(codewb, year)
  
  out
}

# --- Create DATASET.csv if needed ---
if (!file.exists("DATASET.csv") || isTRUE(force_refresh)) {
  cat("\n=== AUTO-DATA: Building DATASET.csv from World Bank Data360 ===\n")
  dataset <- build_dataset(ind_map)
  
  # Keep only rows with at least the target variable
  if ("account_t_d" %in% names(dataset)) {
    dataset <- dataset %>% dplyr::filter(!is.na(account_t_d))
  }
  
  readr::write_csv(dataset, "DATASET.csv", na = "")
  cat(sprintf("Saved DATASET.csv  (%d rows x %d cols)\n",
              nrow(dataset), ncol(dataset)))
  cat("==============================================================\n\n")
} else {
  cat("DATASET.csv found; skipping auto-download (set force_refresh=TRUE to rebuild).\n")
}

# -----------------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL EXPLORATION
# -----------------------------------------------------------------------------

cat("\n=== LOADING DATASET ===\n")

# Load the Global Findex 2025 dataset
data <- read.csv("DATASET.csv", stringsAsFactors = FALSE)

# Initial dataset dimensions
cat("Original dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")

# View structure of key columns
cat("\nKey columns in dataset:\n")
cat("- Country:", length(unique(data$countrynewwb)), "unique countries\n")
cat("- Years available:", unique(data$year), "\n")
cat("- Income groups:", unique(data$incomegroupwb24)[!is.na(unique(data$incomegroupwb24))], "\n")

# -----------------------------------------------------------------------------
# 3. DATA FILTERING - HIGH-INCOME ECONOMIES
# -----------------------------------------------------------------------------

cat("\n=== FILTERING HIGH-INCOME ECONOMIES ===\n")

# Filter for high-income economies only
high_income_data <- data %>%
  filter(incomegroupwb24 == "High income")

cat("High-income observations:", nrow(high_income_data), "\n")
cat("Countries in high-income subset:", length(unique(high_income_data$countrynewwb)), "\n")

# Check UK presence
uk_count <- sum(high_income_data$countrynewwb == "United Kingdom", na.rm = TRUE)
cat("UK observations:", uk_count, "\n")

# -----------------------------------------------------------------------------
# 4. VARIABLE SELECTION
# -----------------------------------------------------------------------------

cat("\n=== VARIABLE SELECTION ===\n")

# Select relevant variables for analysis
selected_vars <- c(
  "countrynewwb", "codewb", "year",
  "account_t_d",
  "fiaccount_t_d", "mobileaccount_t_d",
  "dig_acc", "fin26a", "fin26b", "fin27a", "fin27b",
  "fin17a_17a1_d", "fin17a", "fin17b", "fin17f",
  "borrow_any_t_d", "fin22a_22a1_22g_d", "fin22a", "fin22b",
  "inactive_t_d", "fin24sav", "fin24fam",
  "merchant_pay", "g20_any", "internet"
)

# Select available variables
available_vars <- intersect(selected_vars, colnames(high_income_data))
cat("Selected variables available:", length(available_vars), "\n")

# Create analysis dataset
analysis_data <- high_income_data %>%
  select(all_of(available_vars))

# -----------------------------------------------------------------------------
# 5. DATA PREPROCESSING
# -----------------------------------------------------------------------------

cat("\n=== DATA PREPROCESSING ===\n")

# Handle missing values
cat("\nMissing values before cleaning:\n")
missing_summary <- colSums(is.na(analysis_data))
print(missing_summary[missing_summary > 0])

# Remove rows with missing target variable
analysis_data <- analysis_data %>%
  filter(!is.na(account_t_d))

cat("\nObservations after removing missing target:", nrow(analysis_data), "\n")

# Create UK indicator variable
analysis_data <- analysis_data %>%
  mutate(is_uk = ifelse(countrynewwb == "United Kingdom", 1, 0))

# Select final feature set for modeling
feature_vars <- c("fiaccount_t_d", "mobileaccount_t_d", "dig_acc",
                  "fin26a", "fin26b", "fin27a", "borrow_any_t_d",
                  "fin17a_17a1_d", "fin17a", "inactive_t_d",
                  "merchant_pay", "internet")

available_features <- intersect(feature_vars, colnames(analysis_data))

# Create modeling dataset
model_data <- analysis_data %>%
  select(countrynewwb, account_t_d, is_uk, all_of(available_features))

cat("\nFeatures available for modeling:", length(available_features), "\n")
cat("Features:", paste(available_features, collapse = ", "), "\n")

# Handle missing values - median imputation
for (col in available_features) {
  if (col %in% colnames(model_data)) {
    if (sum(is.na(model_data[[col]])) > 0) {
      median_val <- median(model_data[[col]], na.rm = TRUE)
      model_data[[col]][is.na(model_data[[col]])] <- median_val
    }
  }
}

cat("\nFinal modeling dataset:", nrow(model_data), "observations\n")

# Convert target to factor for classification
model_data$account_class <- factor(
  ifelse(model_data$account_t_d >= 0.5, "Has_Account", "No_Account"),
  levels = c("No_Account", "Has_Account")
)

# -----------------------------------------------------------------------------
# 6. EXPLORATORY DATA ANALYSIS (EDA)
# -----------------------------------------------------------------------------

cat("\n=== EXPLORATORY DATA ANALYSIS ===\n")

# Descriptive Statistics
cat("\n--- Descriptive Statistics ---\n")
cat("\nAccount Ownership Distribution:\n")
print(table(model_data$account_class))
print(prop.table(table(model_data$account_class)) * 100)

# Summary statistics
cat("\nSummary Statistics for Key Variables:\n")
numeric_features <- model_data %>%
  select(account_t_d, all_of(available_features[available_features %in% colnames(model_data)])) %>%
  select_if(is.numeric)

print(summary(numeric_features))

# UK vs Other High-Income Comparison
cat("\n--- UK vs Other High-Income Economies ---\n")
uk_summary <- model_data %>%
  group_by(is_uk) %>%
  summarise(
    n = n(),
    mean_account_ownership = mean(account_t_d, na.rm = TRUE),
    .groups = 'drop'
  )
uk_summary$region <- c("Other High-Income", "United Kingdom")
print(uk_summary)

# -----------------------------------------------------------------------------
# 7. VISUALIZATION - EDA PLOTS (DISPLAYED IN RSTUDIO)
# -----------------------------------------------------------------------------

cat("\n=== GENERATING VISUALIZATIONS ===\n")
cat("*** Plots will appear in RStudio Plots pane ***\n")

# FIGURE 1: Distribution of Financial Account Ownership
fig1 <- ggplot(model_data, aes(x = account_t_d)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(title = "Figure 1: Distribution of Financial Account Ownership",
       subtitle = "High-Income Economies - Global Findex 2025",
       x = "Account Ownership Rate",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(fig1)  # Display in RStudio
ggsave("Figure1_Account_Ownership_Distribution.png", fig1, width = 10, height = 6, dpi = 300)
cat("Saved: Figure1_Account_Ownership_Distribution.png\n")

# FIGURE 2: UK vs Other High-Income Economies
comparison_data <- model_data %>%
  mutate(region = ifelse(is_uk == 1, "United Kingdom", "Other High-Income")) %>%
  group_by(region) %>%
  summarise(
    mean_ownership = mean(account_t_d, na.rm = TRUE),
    se = sd(account_t_d, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

fig2 <- ggplot(comparison_data, aes(x = region, y = mean_ownership, fill = region)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = mean_ownership - se, ymax = mean_ownership + se),
                width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", mean_ownership * 100)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  labs(title = "Figure 2: UK vs Other High-Income Economies",
       subtitle = "Financial Account Ownership Comparison",
       x = "",
       y = "Account Ownership Rate") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

print(fig2)  # Display in RStudio
ggsave("Figure2_UK_vs_HighIncome_Comparison.png", fig2, width = 8, height = 6, dpi = 300)
cat("Saved: Figure2_UK_vs_HighIncome_Comparison.png\n")

# CORRELATION HEATMAP
if (ncol(numeric_features) > 1) {
  cor_matrix <- cor(numeric_features, use = "pairwise.complete.obs")

  # Display in RStudio
  corrplot(cor_matrix, method = "color", type = "upper",
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           addCoef.col = "black", number.cex = 0.6,
           title = "Correlation Matrix - Financial Inclusion Variables",
           mar = c(0, 0, 2, 0))

  # Save to file
  png("Figure_Correlation_Heatmap.png", width = 10, height = 8, units = "in", res = 300)
  corrplot(cor_matrix, method = "color", type = "upper",
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           addCoef.col = "black", number.cex = 0.6,
           title = "Correlation Matrix - Financial Inclusion Variables",
           mar = c(0, 0, 2, 0))
  dev.off()
  cat("Saved: Figure_Correlation_Heatmap.png\n")
}

# -----------------------------------------------------------------------------
# 8. PREPARE DATA FOR MACHINE LEARNING
# -----------------------------------------------------------------------------

cat("\n=== PREPARING DATA FOR MACHINE LEARNING ===\n")

# Select features for modeling
ml_features <- available_features[available_features %in% colnames(model_data)]
ml_features <- ml_features[ml_features != "account_t_d"]

# Remove features with zero variance
feature_data <- model_data[, ml_features, drop = FALSE]
non_zero_var <- sapply(feature_data, function(x) var(x, na.rm = TRUE) > 0)
ml_features <- ml_features[non_zero_var]

cat("Features used for modeling:", length(ml_features), "\n")
cat("Features:", paste(ml_features, collapse = ", "), "\n")

# Create feature matrix
X <- model_data[, ml_features, drop = FALSE]
y <- model_data$account_class

# Remove any remaining NA
complete_cases <- complete.cases(X)
X <- X[complete_cases, ]
y <- y[complete_cases]

cat("Final sample size for modeling:", nrow(X), "\n")
cat("Class distribution:\n")
print(table(y))

# Split data into training and testing sets (70-30 split)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

cat("\nTraining set size:", nrow(X_train), "\n")
cat("Testing set size:", nrow(X_test), "\n")

# Standardize features for KNN
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)

# -----------------------------------------------------------------------------
# 9. MODEL 1: K-NEAREST NEIGHBORS (KNN)
# -----------------------------------------------------------------------------

cat("\n=== MODEL 1: K-NEAREST NEIGHBORS ===\n")

# Find optimal K using cross-validation
set.seed(42)
knn_tune <- train(
  x = X_train_scaled,
  y = y_train,
  method = "knn",
  tuneGrid = data.frame(k = seq(3, 21, by = 2)),
  trControl = trainControl(method = "cv", number = 5)
)

cat("KNN Tuning Results:\n")
print(knn_tune$results)
cat("\nOptimal K:", knn_tune$bestTune$k, "\n")

# Plot KNN tuning results
plot(knn_tune, main = "KNN: Accuracy vs K Value")

# Train final KNN model
knn_pred <- knn(
  train = X_train_scaled,
  test = X_test_scaled,
  cl = y_train,
  k = knn_tune$bestTune$k
)

# KNN Confusion Matrix
knn_cm <- confusionMatrix(knn_pred, y_test)
cat("\nKNN Confusion Matrix:\n")
print(knn_cm)

knn_accuracy <- knn_cm$overall["Accuracy"]
cat("\n*** KNN Accuracy:", round(knn_accuracy * 100, 2), "% ***\n")

# -----------------------------------------------------------------------------
# 10. MODEL 2: DECISION TREE
# -----------------------------------------------------------------------------

cat("\n=== MODEL 2: DECISION TREE ===\n")

# Train Decision Tree with cross-validation
set.seed(42)
dt_model <- train(
  x = X_train,
  y = y_train,
  method = "rpart",
  tuneLength = 10,
  trControl = trainControl(method = "cv", number = 5)
)

cat("Decision Tree Tuning Results:\n")
print(dt_model$results)

# Predictions
dt_pred <- predict(dt_model, X_test)

# Decision Tree Confusion Matrix
dt_cm <- confusionMatrix(dt_pred, y_test)
cat("\nDecision Tree Confusion Matrix:\n")
print(dt_cm)

dt_accuracy <- dt_cm$overall["Accuracy"]
cat("\n*** Decision Tree Accuracy:", round(dt_accuracy * 100, 2), "% ***\n")

# Plot Decision Tree - Display in RStudio
rpart.plot(dt_model$finalModel,
           main = "Decision Tree - Financial Account Ownership",
           extra = 104,
           fallen.leaves = TRUE,
           type = 4,
           box.palette = "BuGn")

# Save Decision Tree plot
png("Figure_Decision_Tree.png", width = 12, height = 8, units = "in", res = 300)
rpart.plot(dt_model$finalModel,
           main = "Decision Tree - Financial Account Ownership",
           extra = 104,
           fallen.leaves = TRUE,
           type = 4,
           box.palette = "BuGn")
dev.off()
cat("Saved: Figure_Decision_Tree.png\n")

# -----------------------------------------------------------------------------
# 11. MODEL 3: RANDOM FOREST
# -----------------------------------------------------------------------------

cat("\n=== MODEL 3: RANDOM FOREST ===\n")

# Train Random Forest
set.seed(42)
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = floor(sqrt(ncol(X_train))),
  importance = TRUE
)

cat("Random Forest Model Summary:\n")
print(rf_model)

# Plot RF error rate
plot(rf_model, main = "Random Forest: Error Rate vs Number of Trees")
legend("topright", colnames(rf_model$err.rate), col = 1:3, lty = 1:3)

# Predictions
rf_pred <- predict(rf_model, X_test)

# Random Forest Confusion Matrix
rf_cm <- confusionMatrix(rf_pred, y_test)
cat("\nRandom Forest Confusion Matrix:\n")
print(rf_cm)

rf_accuracy <- rf_cm$overall["Accuracy"]
cat("\n*** Random Forest Accuracy:", round(rf_accuracy * 100, 2), "% ***\n")

# Feature Importance
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
)
importance_df <- importance_df[order(-importance_df$MeanDecreaseAccuracy), ]

cat("\nFeature Importance (Random Forest):\n")
print(importance_df)

# FIGURE 3: Feature Importance Plot
fig3 <- ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy),
                                   y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Figure 3: Feature Importance from Random Forest Model",
       subtitle = "Determinants of Financial Account Ownership",
       x = "Feature",
       y = "Mean Decrease in Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(fig3)  # Display in RStudio
ggsave("Figure3_Feature_Importance.png", fig3, width = 10, height = 6, dpi = 300)
cat("Saved: Figure3_Feature_Importance.png\n")

# Variable Importance Plot (built-in)
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# -----------------------------------------------------------------------------
# 12. CROSS-VALIDATION FOR ALL MODELS
# -----------------------------------------------------------------------------

cat("\n=== CROSS-VALIDATION RESULTS ===\n")

# Set up 10-fold cross-validation
cv_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE
)

# KNN with CV
set.seed(42)
knn_cv <- train(
  x = X_train_scaled,
  y = y_train,
  method = "knn",
  tuneGrid = data.frame(k = knn_tune$bestTune$k),
  trControl = cv_control
)

# Decision Tree with CV
set.seed(42)
dt_cv <- train(
  x = X_train,
  y = y_train,
  method = "rpart",
  tuneGrid = data.frame(cp = dt_model$bestTune$cp),
  trControl = cv_control
)

# Random Forest with CV
set.seed(42)
rf_cv <- train(
  x = X_train,
  y = y_train,
  method = "rf",
  tuneGrid = data.frame(mtry = floor(sqrt(ncol(X_train)))),
  trControl = cv_control,
  ntree = 500
)

# Compile CV Results
cv_results <- data.frame(
  Model = c("KNN", "Decision Tree", "Random Forest"),
  CV_Accuracy = c(
    round(mean(knn_cv$resample$Accuracy) * 100, 2),
    round(mean(dt_cv$resample$Accuracy) * 100, 2),
    round(mean(rf_cv$resample$Accuracy) * 100, 2)
  ),
  CV_Kappa = c(
    round(mean(knn_cv$resample$Kappa), 4),
    round(mean(dt_cv$resample$Kappa), 4),
    round(mean(rf_cv$resample$Kappa), 4)
  ),
  Test_Accuracy = c(
    round(knn_accuracy * 100, 2),
    round(dt_accuracy * 100, 2),
    round(rf_accuracy * 100, 2)
  )
)

cat("\nCross-Validation and Test Set Performance:\n")
print(cv_results)

# -----------------------------------------------------------------------------
# 13. MODEL COMPARISON VISUALIZATION
# -----------------------------------------------------------------------------

cat("\n=== MODEL COMPARISON ===\n")

# FIGURE 4: Performance comparison plot
comparison_plot_data <- cv_results %>%
  pivot_longer(cols = c(CV_Accuracy, Test_Accuracy),
               names_to = "Metric",
               values_to = "Accuracy")

fig4 <- ggplot(comparison_plot_data, aes(x = Model, y = Accuracy, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Accuracy, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("steelblue", "coral"),
                    labels = c("Cross-Validation", "Test Set")) +
  labs(title = "Figure 4: Model Performance Comparison",
       subtitle = "Cross-Validation vs Test Set Accuracy",
       x = "Model",
       y = "Accuracy (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(fig4)  # Display in RStudio
ggsave("Figure4_Model_Comparison.png", fig4, width = 10, height = 6, dpi = 300)
cat("Saved: Figure4_Model_Comparison.png\n")

# -----------------------------------------------------------------------------
# 14. UK vs OTHER HIGH-INCOME: DETAILED COMPARISON
# -----------------------------------------------------------------------------

cat("\n=== UK vs OTHER HIGH-INCOME: DETAILED ANALYSIS ===\n")

uk_data <- model_data %>% filter(is_uk == 1)
other_data <- model_data %>% filter(is_uk == 0)

comparison_stats <- data.frame(
  Variable = c("Account Ownership", "Bank Account", "Mobile Account",
               "Digital Account", "Borrowed Money", "Saved at Bank"),
  UK = c(
    mean(uk_data$account_t_d, na.rm = TRUE),
    mean(uk_data$fiaccount_t_d, na.rm = TRUE),
    mean(uk_data$mobileaccount_t_d, na.rm = TRUE),
    mean(uk_data$dig_acc, na.rm = TRUE),
    mean(uk_data$borrow_any_t_d, na.rm = TRUE),
    mean(uk_data$fin17a, na.rm = TRUE)
  ),
  Other_High_Income = c(
    mean(other_data$account_t_d, na.rm = TRUE),
    mean(other_data$fiaccount_t_d, na.rm = TRUE),
    mean(other_data$mobileaccount_t_d, na.rm = TRUE),
    mean(other_data$dig_acc, na.rm = TRUE),
    mean(other_data$borrow_any_t_d, na.rm = TRUE),
    mean(other_data$fin17a, na.rm = TRUE)
  )
)

comparison_stats$Difference <- comparison_stats$UK - comparison_stats$Other_High_Income

cat("\nDetailed UK vs Other High-Income Comparison:\n")
print(comparison_stats)

# Save comparison table
write.csv(comparison_stats, "UK_vs_HighIncome_Comparison.csv", row.names = FALSE)
cat("Saved: UK_vs_HighIncome_Comparison.csv\n")

# -----------------------------------------------------------------------------
# 15. CONFUSION MATRICES VISUALIZATION
# -----------------------------------------------------------------------------

# Function to create confusion matrix heatmap
plot_confusion_matrix <- function(cm, title) {
  cm_table <- as.data.frame(cm$table)
  colnames(cm_table) <- c("Prediction", "Reference", "Freq")

  ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 8, color = "white", fontface = "bold") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "none")
}

# Create confusion matrix plots
cm_knn <- plot_confusion_matrix(knn_cm, "KNN Confusion Matrix")
cm_dt <- plot_confusion_matrix(dt_cm, "Decision Tree Confusion Matrix")
cm_rf <- plot_confusion_matrix(rf_cm, "Random Forest Confusion Matrix")

# FIGURE 5: Combined confusion matrices
fig5 <- grid.arrange(cm_knn, cm_dt, cm_rf, ncol = 3,
                     top = "Figure 5: Confusion Matrices for All Models")

print(fig5)  # Display in RStudio
ggsave("Figure5_Confusion_Matrices.png", fig5, width = 15, height = 5, dpi = 300)
cat("Saved: Figure5_Confusion_Matrices.png\n")

# -----------------------------------------------------------------------------
# 16. FINAL RESULTS SUMMARY
# -----------------------------------------------------------------------------

cat("\n")
cat("======================================================================\n")
cat("                    FINAL RESULTS SUMMARY\n")
cat("======================================================================\n\n")

cat("DATASET INFORMATION:\n")
cat("- Total observations (high-income): ", nrow(high_income_data), "\n")
cat("- Observations used for modeling: ", nrow(X), "\n")
cat("- Features used: ", length(ml_features), "\n")
cat("- Training set size: ", nrow(X_train), "\n")
cat("- Testing set size: ", nrow(X_test), "\n\n")

cat("MODEL PERFORMANCE (TEST SET):\n")
cat("----------------------------------------\n")
cat(sprintf("%-20s %s\n", "Model", "Accuracy"))
cat("----------------------------------------\n")
cat(sprintf("%-20s %.2f%%\n", "KNN", knn_accuracy * 100))
cat(sprintf("%-20s %.2f%%\n", "Decision Tree", dt_accuracy * 100))
cat(sprintf("%-20s %.2f%%\n", "Random Forest", rf_accuracy * 100))
cat("----------------------------------------\n\n")

cat("CROSS-VALIDATION PERFORMANCE (10-Fold):\n")
print(cv_results)
cat("\n")

cat("TOP 5 IMPORTANT FEATURES (Random Forest):\n")
print(head(importance_df, 5))
cat("\n")

cat("UK vs OTHER HIGH-INCOME COMPARISON:\n")
print(comparison_stats[1:3, ])
cat("\n")

cat("FILES GENERATED:\n")
cat("- Figure1_Account_Ownership_Distribution.png\n")
cat("- Figure2_UK_vs_HighIncome_Comparison.png\n")
cat("- Figure3_Feature_Importance.png\n")
cat("- Figure4_Model_Comparison.png\n")
cat("- Figure5_Confusion_Matrices.png\n")
cat("- Figure_Correlation_Heatmap.png\n")
cat("- Figure_Decision_Tree.png\n")
cat("- UK_vs_HighIncome_Comparison.csv\n")

cat("\n")
cat("======================================================================\n")
cat("                    ANALYSIS COMPLETE\n")
cat("======================================================================\n")

# -----------------------------------------------------------------------------
# 17. SAVE MODEL OBJECTS AND RESULTS
# -----------------------------------------------------------------------------

# Save model objects
save(knn_tune, dt_model, rf_model, file = "trained_models.RData")
cat("\nModel objects saved to: trained_models.RData\n")

# Save results summary
results_summary <- list(
  dataset_info = list(
    total_obs = nrow(high_income_data),
    model_obs = nrow(X),
    n_features = length(ml_features),
    train_size = nrow(X_train),
    test_size = nrow(X_test)
  ),
  model_accuracy = list(
    knn = knn_accuracy,
    decision_tree = dt_accuracy,
    random_forest = rf_accuracy
  ),
  cv_results = cv_results,
  feature_importance = importance_df,
  uk_comparison = comparison_stats
)

saveRDS(results_summary, "analysis_results.rds")
cat("Results summary saved to: analysis_results.rds\n")

# -----------------------------------------------------------------------------
# 18. VIEW ALL PLOTS AGAIN (OPTIONAL)
# -----------------------------------------------------------------------------

# Uncomment below to display all plots again at the end:
# print(fig1)
# print(fig2)
# print(fig3)
# print(fig4)
# print(fig5)

cat("\n*** To view any plot again, run: print(fig1), print(fig2), etc. ***\n")
