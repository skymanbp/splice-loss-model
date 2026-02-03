#!/usr/bin/env Rscript
# ============================================================
# Splice Loss Prediction using GLMM
# Main Entry Script
# Author: Zhe Zhang
# ============================================================

# Set working directory to script location (if run interactively)
if (interactive()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ============================================================
# 1. SETUP
# ============================================================

# Source all module files
source("R/utils.R")
source("R/data_processing.R")
source("R/visualization.R")
source("R/modeling.R")
source("R/prediction.R")

# Load required packages
required_packages <- c("lme4", "lmerTest", "readxl", "dplyr",
                       "ggplot2", "performance", "yaml")

cat("=== Splice Loss GLMM Model ===\n\n")

# Check packages
if (!check_packages(required_packages)) {
  stop("Please install missing packages before running.")
}

# Load packages
suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(performance)
  library(yaml)
})

# Load configuration
config <- load_config("config.yaml")
verbose <- config$logging$verbose

# Ensure output directory exists
ensure_output_dir(config)

# ============================================================
# 2. DATA PROCESSING
# ============================================================

log_message("Starting data processing...", verbose)

df <- load_and_preprocess_data(config, verbose)
print_data_summary(df)

# ============================================================
# 3. EXPLORATORY DATA ANALYSIS
# ============================================================

log_message("Creating exploratory plots...", verbose)

eda_plots <- create_eda_plots(df, config, verbose)

# ============================================================
# 4. MODEL BUILDING
# ============================================================

log_message("Building GLMM models...", verbose)

models <- build_models(df, config, verbose)

# ============================================================
# 5. MODEL COMPARISON AND SELECTION
# ============================================================

log_message("Comparing models...", verbose)

comparison_result <- compare_models(models, verbose)
final_model <- comparison_result$selected_model

# ============================================================
# 6. MODEL ANALYSIS
# ============================================================

log_message("Analyzing final model...", verbose)

analysis <- analyze_model(final_model, config, verbose)

# ============================================================
# 7. MODEL DIAGNOSTICS
# ============================================================

log_message("Running model diagnostics...", verbose)

df <- add_model_predictions(df, final_model)
diagnostic_plots <- create_diagnostic_plots(df, config, verbose)

# ============================================================
# 8. EXAMPLE PREDICTION
# ============================================================

log_message("Running example prediction...", verbose)

example_data <- create_example_data(df)
predicted_loss <- predict_splice_loss(example_data, final_model)

cat("\n=== Example Prediction ===\n")
cat("Input:\n")
print(example_data[, c("splice_type", "fiber1_dist_center", "fiber2_dist_center")])
cat(sprintf("\nPredicted splice loss: %.4f dB\n", predicted_loss))

# ============================================================
# 9. SAVE RESULTS
# ============================================================

# Save model
model_path <- get_output_path(config, "model_file")
save_model(final_model, model_path, verbose)

# Generate and save summary report
if (config$logging$save_summary) {
  report <- generate_summary_report(df, final_model, analysis)
  cat(report)
  save_summary(report, config$logging$summary_file)
}

# ============================================================
# 10. COMPLETION
# ============================================================

cat("\n")
log_message("=== Analysis Complete ===", verbose)
cat("\nOutput files saved to:", config$output$directory, "\n")
cat("  - Model:", config$output$model_file, "\n")
cat("  - Plots:", paste(unlist(config$output$plots), collapse = ", "), "\n")

# Return results for programmatic use
results <- list(
  data = df,
  models = models,
  final_model = final_model,
  analysis = analysis,
  config = config
)

invisible(results)
