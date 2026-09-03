# Tests for model building
#
# The fixture reproduces the workbook's sign convention and scale: `ref` and
# `result` are absolute power levels in dB (both negative), `diff = result -
# ref`, and the physical splice loss is `-diff` — a small positive number
# (order 0.1 dB) for a lossy splice. A handful of rows carry a zero or
# slightly negative loss, the measurement-noise case that the pipeline has to
# remove before a Gamma(link = "log") fit can run.

make_splice_fixture <- function(n = 300, n_nonpositive = 8, seed = 20260903) {
  set.seed(seed)

  df <- data.frame(
    fiber1 = factor(sample(paste0("F", 1:4), n, replace = TRUE)),
    fiber2 = factor(sample(paste0("G", 1:4), n, replace = TRUE)),
    splice_type = factor(sample(c("Self splice", "Cross splice"), n, replace = TRUE)),
    test_no = factor(sample(1:6, n, replace = TRUE)),
    core_no = factor(sample(1:4, n, replace = TRUE)),
    fiber1_dist_center = runif(n, 0, 2),
    fiber2_dist_center = runif(n, 0, 2),
    fiber1_pitch = runif(n, 38, 42),
    fiber2_pitch = runif(n, 38, 42),
    prooftest = runif(n, 0, 1),
    ffw = runif(n, 0, 1)
  )

  # Loss on the log link, mean around 0.13 dB as in the reference data
  eta <- log(0.10) +
    0.25 * (df$splice_type == "Cross splice") +
    0.30 * df$fiber2_dist_center +
    0.10 * df$fiber1_dist_center +
    rnorm(nlevels(df$fiber1), 0, 0.20)[df$fiber1] +
    rnorm(nlevels(df$fiber2), 0, 0.20)[df$fiber2]
  loss <- rgamma(n, shape = 2, scale = exp(eta) / 2)

  # Measurement noise around a lossless splice: a few zero / negative losses
  idx <- sample(seq_len(n), n_nonpositive)
  loss[idx] <- c(rep(0, ceiling(n_nonpositive / 2)),
                 -runif(floor(n_nonpositive / 2), 0, 0.02))

  # Absolute power levels, as recorded in the workbook
  df$ref <- runif(n, -7, -4)
  df$result <- df$ref - loss
  df$diff <- df$result - df$ref

  return(df)
}

test_that("the fixture matches the workbook's sign convention", {
  df <- make_splice_fixture()

  expect_true(all(df$ref < 0))
  expect_true(all(df$result < 0))
  # diff = result - ref, so the loss is -diff and some rows are non-positive
  expect_equal(df$diff, df$result - df$ref)
  expect_true(any(-df$diff <= 0))
  expect_true(mean(-df$diff) > 0 && mean(-df$diff) < 1)
})

test_that("build_models fits three GLMMs on a strictly positive response", {
  skip_if_not_installed("lme4")

  raw <- make_splice_fixture()
  config <- list(data = list(drop_nonpositive_loss = TRUE, min_loss_db = 0))

  df <- create_derived_features(raw, verbose = FALSE)
  expect_true(any(df$splice_loss <= 0))

  df <- filter_nonpositive_loss(df, config, verbose = FALSE)

  # The response entering glmer must be strictly positive
  expect_true(all(df$splice_loss > 0))
  expect_equal(nrow(df), sum(-raw$diff > 0))

  suppressMessages(suppressWarnings({
    models <- build_models(df, config, verbose = FALSE)
  }))

  expect_type(models, "list")
  expect_named(models, c("basic", "extended", "interactions"))
  for (m in models) {
    expect_s4_class(m, "glmerMod")
    # The fitted response is the derived loss, not the raw power level
    expect_equal(as.character(formula(m))[2], "splice_loss")
    expect_true(all(model.response(model.frame(m)) > 0))
    expect_equal(family(m)$family, "Gamma")
    expect_equal(family(m)$link, "log")
  }

  # Fitted values are losses: positive and on the observed scale
  expect_true(all(fitted(models$extended) > 0))
})

test_that("build_models refuses a non-positive response", {
  skip_if_not_installed("lme4")

  df <- create_derived_features(make_splice_fixture(), verbose = FALSE)

  expect_error(
    build_models(df, list(), verbose = FALSE),
    "non-positive value"
  )
})

test_that("compare_models reports AIC/BIC and returns the extended model", {
  skip_if_not_installed("lme4")

  config <- list(data = list(drop_nonpositive_loss = TRUE, min_loss_db = 0))
  df <- filter_nonpositive_loss(
    create_derived_features(make_splice_fixture(), verbose = FALSE),
    config, verbose = FALSE)

  suppressMessages(suppressWarnings({
    models <- build_models(df, config, verbose = FALSE)
    result <- compare_models(models, verbose = FALSE)
  }))

  expect_equal(result$selected_name, "extended")
  expect_identical(result$selected_model, models$extended)
  expect_equal(sort(result$comparison$Model),
               sort(c("basic", "extended", "interactions")))
  expect_true(all(is.finite(result$comparison$AIC)))
  expect_true(all(is.finite(result$comparison$BIC)))
})
