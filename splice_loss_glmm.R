# ============================================================
# Splice Loss Prediction using GLMM (Generalized Linear Mixed Model)
# Author: Zhe Zhang
# Description: Predicting optical fiber splice power losses 
#              based on geometric features using mixed effects models
# ============================================================

# Load required libraries
library(lme4)       # For mixed effects models
library(lmerTest)   # For p-values in lmer
library(readxl)     # For reading Excel files
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(performance) # For model diagnostics

# ============================================================
# 1. DATA LOADING AND PREPROCESSING
# ============================================================

# Load the data
df <- read_excel("splice_data.xlsx")

# View data structure
str(df)
summary(df)

# Clean column names (remove spaces and special characters)
colnames(df) <- c("fiber1", "fiber2", "splice_type", "test_no", "core_no",
                  "ref", "result", "diff", "prooftest", 
                  "fiber1_dist_center", "fiber2_dist_center",
                  "fiber1_pitch", "fiber2_pitch", "ffw", "unnamed")

# Remove unnecessary columns
df <- df %>% 
  select(-unnamed, -ref) %>%  # ref is highly correlated with result (reference value)
  na.omit()  # Remove rows with missing values

# Create derived features
df <- df %>%
  mutate(
    # Distance difference between two fibers
    dist_diff = abs(fiber1_dist_center - fiber2_dist_center),
    # Average distance to center
    avg_dist_center = (fiber1_dist_center + fiber2_dist_center) / 2,
    # Pitch difference
    pitch_diff = abs(fiber1_pitch - fiber2_pitch),
    # Average pitch
    avg_pitch = (fiber1_pitch + fiber2_pitch) / 2,
    # Convert to factors for random effects
    fiber1 = as.factor(fiber1),
    fiber2 = as.factor(fiber2),
    splice_type = as.factor(splice_type),
    core_no = as.factor(core_no),
    test_no = as.factor(test_no)
  )

# Summary of processed data
cat("\n=== Processed Data Summary ===\n")
cat("Number of observations:", nrow(df), "\n")
cat("Number of fiber types:", length(unique(c(as.character(df$fiber1), 
                                                as.character(df$fiber2)))), "\n")
cat("Splice types:", levels(df$splice_type), "\n")

# ============================================================
# 2. EXPLORATORY DATA ANALYSIS
# ============================================================

# Distribution of splice loss
p1 <- ggplot(df, aes(x = result)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Splice Loss",
       x = "Splice Loss (dB)", y = "Count") +
  theme_minimal()

# Splice loss by splice type
p2 <- ggplot(df, aes(x = splice_type, y = result, fill = splice_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Splice Loss by Splice Type",
       x = "Splice Type", y = "Splice Loss (dB)") +
  theme_minimal() +
  theme(legend.position = "none")

# Splice loss vs distance to center
p3 <- ggplot(df, aes(x = avg_dist_center, y = result, color = splice_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Splice Loss vs Average Distance to Center",
       x = "Average Distance to Center (micron)", y = "Splice Loss (dB)") +
  theme_minimal()

# Save plots
ggsave("splice_loss_distribution.png", p1, width = 8, height = 6)
ggsave("splice_loss_by_type.png", p2, width = 8, height = 6)
ggsave("splice_loss_vs_distance.png", p3, width = 10, height = 6)

# ============================================================
# 3. MODEL BUILDING - GLMM
# ============================================================

cat("\n=== Building GLMM Models ===\n")

# Model 1: Basic model with fiber as random effect
model1 <- lmer(
  result ~ splice_type + fiber2_dist_center + pitch_diff + 
    (1 | fiber1) + (1 | fiber2),
  data = df,
  REML = TRUE
)

cat("\n--- Model 1: Basic GLMM ---\n")
summary(model1)

# Model 2: Add more fixed effects
model2 <- lmer(
  result ~ splice_type + fiber2_dist_center + fiber1_dist_center + 
    pitch_diff + avg_pitch + core_no +
    (1 | fiber1) + (1 | fiber2),
  data = df,
  REML = TRUE
)

cat("\n--- Model 2: Extended GLMM ---\n")
summary(model2)

# Model 3: Include interaction terms
model3 <- lmer(
  result ~ splice_type * fiber2_dist_center + 
    fiber1_dist_center + pitch_diff + core_no +
    (1 | fiber1) + (1 | fiber2) + (1 | test_no),
  data = df,
  REML = TRUE
)

cat("\n--- Model 3: GLMM with Interactions ---\n")
summary(model3)

# ============================================================
# 4. MODEL COMPARISON AND SELECTION
# ============================================================

cat("\n=== Model Comparison ===\n")

# Compare models using AIC/BIC
models_aic <- AIC(model1, model2, model3)
models_bic <- BIC(model1, model2, model3)

cat("\nAIC Comparison:\n")
print(models_aic)

cat("\nBIC Comparison:\n")
print(models_bic)

# Likelihood ratio test
cat("\nLikelihood Ratio Test (Model 1 vs Model 2):\n")
print(anova(model1, model2))

cat("\nLikelihood Ratio Test (Model 2 vs Model 3):\n")
print(anova(model2, model3))

# ============================================================
# 5. FINAL MODEL ANALYSIS
# ============================================================

# Select best model (assuming model2 is best based on parsimony)
final_model <- model2

cat("\n=== Final Model Summary ===\n")
summary(final_model)

# Extract fixed effects
cat("\n--- Fixed Effects ---\n")
fixef(final_model)

# Extract random effects
cat("\n--- Random Effects Variance ---\n")
VarCorr(final_model)

# Calculate R-squared (marginal and conditional)
cat("\n--- Model Performance ---\n")
r2_values <- r2(final_model)
print(r2_values)

# Confidence intervals for fixed effects
cat("\n--- 95% Confidence Intervals ---\n")
confint(final_model, method = "Wald")

# ============================================================
# 6. MODEL DIAGNOSTICS
# ============================================================

cat("\n=== Model Diagnostics ===\n")

# Residual plots
df$fitted <- fitted(final_model)
df$residuals <- residuals(final_model)

# Residuals vs Fitted
p4 <- ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q plot
p5 <- ggplot(df, aes(sample = residuals)) +
  stat_qq(color = "steelblue", alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Actual vs Predicted
p6 <- ggplot(df, aes(x = result, y = fitted)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Splice Loss",
       x = "Actual (dB)", y = "Predicted (dB)") +
  theme_minimal()

ggsave("residuals_vs_fitted.png", p4, width = 8, height = 6)
ggsave("qq_plot.png", p5, width = 8, height = 6)
ggsave("actual_vs_predicted.png", p6, width = 8, height = 6)

# ============================================================
# 7. PREDICTION FUNCTION
# ============================================================

#' Predict splice loss for new data
#' @param new_data Data frame with required predictors
#' @param model The fitted lmer model
#' @return Predicted splice loss values
predict_splice_loss <- function(new_data, model = final_model) {
  predictions <- predict(model, newdata = new_data, 
                         allow.new.levels = TRUE)
  return(predictions)
}

# Example prediction
cat("\n=== Example Prediction ===\n")
example_data <- data.frame(
  splice_type = factor("Cross splice", levels = levels(df$splice_type)),
  fiber1_dist_center = 0.5,
  fiber2_dist_center = 0.8,
  pitch_diff = 0.2,
  avg_pitch = 40.3,
  core_no = factor("2", levels = levels(df$core_no)),
  fiber1 = df$fiber1[1],  # Use existing fiber level
  fiber2 = df$fiber2[1]
)

predicted_loss <- predict_splice_loss(example_data)
cat("Predicted splice loss:", round(predicted_loss, 3), "dB\n")

# ============================================================
# 8. SUMMARY REPORT
# ============================================================

cat("\n")
cat("============================================================\n")
cat("                    MODEL SUMMARY REPORT                     \n")
cat("============================================================\n")
cat("\n")
cat("Data: ", nrow(df), " splice observations\n")
cat("Response Variable: Splice Loss (dB)\n")
cat("\n")
cat("Fixed Effects:\n")
cat("  - Splice Type (Self vs Cross)\n")
cat("  - Fiber 1 Distance to Center (micron)\n")
cat("  - Fiber 2 Distance to Center (micron)\n")
cat("  - Pitch Difference\n")
cat("  - Average Pitch\n")
cat("  - Core Number\n")
cat("\n")
cat("Random Effects:\n")
cat("  - Fiber 1 ID (accounts for fiber-to-fiber variation)\n")
cat("  - Fiber 2 ID (accounts for fiber-to-fiber variation)\n")
cat("\n")
cat("Key Findings:\n")
fe <- fixef(final_model)
cat("  - Cross splice vs Self splice effect:", round(fe["splice_typeSelf splice"], 3), "dB\n")
cat("  - Fiber 2 distance effect:", round(fe["fiber2_dist_center"], 3), "dB/micron\n")
cat("\n")
cat("============================================================\n")

# Save the final model
saveRDS(final_model, "splice_loss_glmm_model.rds")
cat("\nModel saved to: splice_loss_glmm_model.rds\n")
