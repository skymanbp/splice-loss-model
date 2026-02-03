# ============================================================
# Prediction Functions for Splice Loss GLMM Model
# ============================================================

#' Predict splice loss for new data
#' @param new_data Data frame with required predictors
#' @param model Fitted lmer model
#' @param allow_new_levels Allow prediction for new factor levels
#' @return Vector of predicted splice loss values
predict_splice_loss <- function(new_data, model, allow_new_levels = TRUE) {
  predictions <- predict(
    model,
    newdata = new_data,
    allow.new.levels = allow_new_levels
  )
  return(predictions)
}

#' Predict with confidence intervals using bootstrap
#' @param new_data Data frame with required predictors
#' @param model Fitted lmer model
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return Data frame with predictions and confidence intervals
predict_with_ci <- function(new_data, model, n_boot = 100, conf_level = 0.95) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required")
  }

  # Point predictions
  point_pred <- predict(model, newdata = new_data, allow.new.levels = TRUE)

  # Bootstrap for confidence intervals
  boot_preds <- matrix(NA, nrow = nrow(new_data), ncol = n_boot)

  # Use parametric bootstrap
  tryCatch({
    boot_samples <- lme4::bootMer(
      model,
      FUN = function(m) predict(m, newdata = new_data, allow.new.levels = TRUE),
      nsim = n_boot,
      use.u = TRUE,
      type = "parametric"
    )

    boot_preds <- t(boot_samples$t)
  }, error = function(e) {
    warning("Bootstrap failed, returning point predictions only: ", e$message)
  })

  # Calculate confidence intervals
  alpha <- 1 - conf_level
  ci_lower <- apply(boot_preds, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  ci_upper <- apply(boot_preds, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  result <- data.frame(
    prediction = point_pred,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    se = apply(boot_preds, 1, sd, na.rm = TRUE)
  )

  return(result)
}

#' Create example prediction data
#' @param df Original data frame (for factor levels)
#' @return Example data frame for prediction
create_example_data <- function(df) {
  example_data <- data.frame(
    splice_type = factor("Cross splice", levels = levels(df$splice_type)),
    fiber1_dist_center = 0.5,
    fiber2_dist_center = 0.8,
    pitch_diff = 0.2,
    avg_pitch = 40.3,
    core_no = factor(levels(df$core_no)[1], levels = levels(df$core_no)),
    fiber1 = df$fiber1[1],
    fiber2 = df$fiber2[1]
  )
  return(example_data)
}

#' Validate new data for prediction
#' @param new_data Data frame to validate
#' @param model Fitted model
#' @return TRUE if valid, error otherwise
validate_prediction_data <- function(new_data, model) {
  # Get required columns from model formula
  model_vars <- all.vars(formula(model))
  response_var <- model_vars[1]
  predictor_vars <- model_vars[-1]

  # Check for missing columns
  missing_cols <- setdiff(predictor_vars, colnames(new_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  return(TRUE)
}

#' Batch prediction for multiple scenarios
#' @param scenarios_df Data frame with multiple prediction scenarios
#' @param model Fitted model
#' @param verbose Print progress
#' @return Data frame with scenarios and predictions
batch_predict <- function(scenarios_df, model, verbose = TRUE) {
  validate_prediction_data(scenarios_df, model)

  if (verbose) {
    log_message(sprintf("Predicting for %d scenarios...", nrow(scenarios_df)))
  }

  predictions <- predict_splice_loss(scenarios_df, model)
  result <- cbind(scenarios_df, predicted_loss = predictions)

  if (verbose) {
    log_message("Batch prediction complete")
  }

  return(result)
}
