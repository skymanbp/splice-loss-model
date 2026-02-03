# ============================================================
# Modeling Functions for Splice Loss GLMM Model
# ============================================================

#' Build multiple GLMM models for comparison
#' @param df Processed data frame
#' @param config Configuration list
#' @param verbose Print messages
#' @return List of fitted models
build_models <- function(df, config, verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required. Install with: install.packages('lme4')")
  }

  if (verbose) log_message("Building GLMM models...")

  use_reml <- config$model$use_reml

  # Model 1: Basic model
  model1 <- lme4::lmer(
    result ~ splice_type + fiber2_dist_center + pitch_diff +
      (1 | fiber1) + (1 | fiber2),
    data = df,
    REML = use_reml
  )
  if (verbose) log_message("Model 1 (Basic) fitted")

  # Model 2: Extended model
  model2 <- lme4::lmer(
    result ~ splice_type + fiber2_dist_center + fiber1_dist_center +
      pitch_diff + avg_pitch + core_no +
      (1 | fiber1) + (1 | fiber2),
    data = df,
    REML = use_reml
  )
  if (verbose) log_message("Model 2 (Extended) fitted")

  # Model 3: Model with interactions
  model3 <- lme4::lmer(
    result ~ splice_type * fiber2_dist_center +
      fiber1_dist_center + pitch_diff + core_no +
      (1 | fiber1) + (1 | fiber2) + (1 | test_no),
    data = df,
    REML = use_reml
  )
  if (verbose) log_message("Model 3 (Interactions) fitted")

  return(list(
    basic = model1,
    extended = model2,
    interactions = model3
  ))
}

#' Compare models and select the best one
#' @param models List of fitted models
#' @param verbose Print comparison results
#' @return List with comparison results and selected model
compare_models <- function(models, verbose = TRUE) {
  if (verbose) {
    cat("\n=== Model Comparison ===\n")
  }

  # AIC comparison
  aic_values <- sapply(models, AIC)
  bic_values <- sapply(models, BIC)

  comparison_df <- data.frame(
    Model = names(models),
    AIC = aic_values,
    BIC = bic_values
  )

  if (verbose) {
    cat("\nAIC/BIC Comparison:\n")
    print(comparison_df)
  }

  # Likelihood ratio tests
  if (verbose) {
    cat("\nLikelihood Ratio Test (Basic vs Extended):\n")
    print(anova(models$basic, models$extended))

    cat("\nLikelihood Ratio Test (Extended vs Interactions):\n")
    print(anova(models$extended, models$interactions))
  }

  # Select best model (default: extended model for balance)
  best_model_name <- "extended"
  best_model <- models[[best_model_name]]

  if (verbose) {
    log_message(sprintf("Selected model: %s", best_model_name))
  }

  return(list(
    comparison = comparison_df,
    selected_name = best_model_name,
    selected_model = best_model
  ))
}

#' Analyze the final model
#' @param model Fitted lmer model
#' @param config Configuration list
#' @param verbose Print results
#' @return List with model analysis results
analyze_model <- function(model, config, verbose = TRUE) {
  if (!requireNamespace("performance", quietly = TRUE)) {
    stop("Package 'performance' is required. Install with: install.packages('performance')")
  }

  if (verbose) {
    cat("\n=== Final Model Analysis ===\n")
    print(summary(model))
  }

  # Fixed effects
  fixed_eff <- lme4::fixef(model)
  if (verbose) {
    cat("\n--- Fixed Effects ---\n")
    print(fixed_eff)
  }

  # Random effects variance
  var_corr <- lme4::VarCorr(model)
  if (verbose) {
    cat("\n--- Random Effects Variance ---\n")
    print(var_corr)
  }

  # R-squared
  r2_values <- performance::r2(model)
  if (verbose) {
    cat("\n--- Model Performance (R-squared) ---\n")
    print(r2_values)
  }

  # Confidence intervals
  conf_level <- config$model$confidence_level
  ci <- confint(model, method = "Wald", level = conf_level)
  if (verbose) {
    cat(sprintf("\n--- %d%% Confidence Intervals ---\n", conf_level * 100))
    print(ci)
  }

  return(list(
    fixed_effects = fixed_eff,
    var_corr = var_corr,
    r_squared = r2_values,
    confidence_intervals = ci
  ))
}

#' Add fitted values and residuals to data frame
#' @param df Data frame
#' @param model Fitted model
#' @return Data frame with fitted and residuals columns
add_model_predictions <- function(df, model) {
  df$fitted <- fitted(model)
  df$residuals <- residuals(model)
  return(df)
}

#' Save model to file
#' @param model Fitted model
#' @param filepath Output file path
#' @param verbose Print message
save_model <- function(model, filepath, verbose = TRUE) {
  saveRDS(model, filepath)
  if (verbose) log_message(sprintf("Model saved to: %s", filepath))
}

#' Load model from file
#' @param filepath Model file path
#' @return Loaded model
load_model <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("Model file not found: %s", filepath))
  }
  return(readRDS(filepath))
}

#' Generate model summary report
#' @param df Data frame
#' @param model Fitted model
#' @param analysis Model analysis results
#' @return Character string with summary report
generate_summary_report <- function(df, model, analysis) {
  fe <- analysis$fixed_effects

  report <- paste0(
    "============================================================\n",
    "                    MODEL SUMMARY REPORT                     \n",
    "============================================================\n\n",
    sprintf("Data: %d splice observations\n", nrow(df)),
    "Response Variable: Splice Loss (dB)\n\n",
    "Fixed Effects:\n",
    "  - Splice Type (Self vs Cross)\n",
    "  - Fiber 1 Distance to Center (micron)\n",
    "  - Fiber 2 Distance to Center (micron)\n",
    "  - Pitch Difference\n",
    "  - Average Pitch\n",
    "  - Core Number\n\n",
    "Random Effects:\n",
    "  - Fiber 1 ID (accounts for fiber-to-fiber variation)\n",
    "  - Fiber 2 ID (accounts for fiber-to-fiber variation)\n\n",
    "Model Performance:\n",
    sprintf("  - Marginal R2: %.4f\n", analysis$r_squared$R2_marginal),
    sprintf("  - Conditional R2: %.4f\n", analysis$r_squared$R2_conditional),
    "\n============================================================\n"
  )

  return(report)
}
