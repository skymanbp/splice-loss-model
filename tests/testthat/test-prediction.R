# Tests for prediction functions

# Create mock data and model for testing
create_mock_data <- function() {
  set.seed(42)
  n <- 100

  df <- data.frame(
    fiber1 = factor(sample(LETTERS[1:3], n, replace = TRUE)),
    fiber2 = factor(sample(LETTERS[4:6], n, replace = TRUE)),
    splice_type = factor(sample(c("Self splice", "Cross splice"), n, replace = TRUE)),
    core_no = factor(sample(1:4, n, replace = TRUE)),
    test_no = factor(sample(1:10, n, replace = TRUE)),
    fiber1_dist_center = runif(n, 0, 2),
    fiber2_dist_center = runif(n, 0, 2),
    fiber1_pitch = runif(n, 38, 42),
    fiber2_pitch = runif(n, 38, 42)
  )

  df$pitch_diff <- abs(df$fiber1_pitch - df$fiber2_pitch)
  df$avg_pitch <- (df$fiber1_pitch + df$fiber2_pitch) / 2
  df$result <- 0.1 + 0.05 * df$fiber2_dist_center + rnorm(n, 0, 0.02)

  return(df)
}

test_that("create_example_data creates valid data frame", {
  df <- create_mock_data()
  example <- create_example_data(df)

  expect_s3_class(example, "data.frame")
  expect_equal(nrow(example), 1)
  expect_true("splice_type" %in% colnames(example))
  expect_true("fiber1_dist_center" %in% colnames(example))
  expect_true("fiber2_dist_center" %in% colnames(example))
})

test_that("validate_prediction_data detects missing columns", {
  df <- create_mock_data()

  # Fit a simple model
  suppressWarnings({
    model <- lme4::lmer(
      result ~ splice_type + fiber2_dist_center + (1 | fiber1),
      data = df,
      REML = TRUE
    )
  })

  # Valid data should pass
  valid_data <- data.frame(
    splice_type = factor("Self splice", levels = levels(df$splice_type)),
    fiber2_dist_center = 0.5,
    fiber1 = df$fiber1[1]
  )
  expect_true(validate_prediction_data(valid_data, model))

  # Invalid data should fail
  invalid_data <- data.frame(
    splice_type = factor("Self splice", levels = levels(df$splice_type))
  )
  expect_error(validate_prediction_data(invalid_data, model), "Missing required columns")
})

test_that("predict_splice_loss returns numeric predictions", {
  df <- create_mock_data()

  suppressWarnings({
    model <- lme4::lmer(
      result ~ splice_type + fiber2_dist_center + (1 | fiber1),
      data = df,
      REML = TRUE
    )
  })

  new_data <- data.frame(
    splice_type = factor("Self splice", levels = levels(df$splice_type)),
    fiber2_dist_center = 0.5,
    fiber1 = df$fiber1[1]
  )

  predictions <- predict_splice_loss(new_data, model)

  expect_type(predictions, "double")
  expect_length(predictions, 1)
})

test_that("batch_predict handles multiple scenarios", {
  df <- create_mock_data()

  suppressWarnings({
    model <- lme4::lmer(
      result ~ splice_type + fiber2_dist_center + (1 | fiber1),
      data = df,
      REML = TRUE
    )
  })

  scenarios <- data.frame(
    splice_type = factor(c("Self splice", "Cross splice"),
                         levels = levels(df$splice_type)),
    fiber2_dist_center = c(0.5, 1.0),
    fiber1 = rep(df$fiber1[1], 2)
  )

  result <- batch_predict(scenarios, model, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("predicted_loss" %in% colnames(result))
})
