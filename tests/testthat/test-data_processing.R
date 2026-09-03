# Tests for data processing functions

# A fixture with the workbook's sign convention: ref and result are absolute
# power levels in dB (both negative), diff = result - ref, and the physical
# loss is -diff. Row 3 carries a slightly negative loss, the measurement-noise
# case that filter_nonpositive_loss() removes.
make_raw_fixture <- function() {
  ref <- c(-4.5000, -4.2000, -4.8000)
  result <- c(-4.6200, -4.4500, -4.7900)
  data.frame(
    fiber1_dist_center = c(1.0, 2.0, 3.0),
    fiber2_dist_center = c(1.5, 2.5, 3.5),
    fiber1_pitch = c(40.0, 41.0, 42.0),
    fiber2_pitch = c(40.5, 41.5, 42.5),
    ref = ref,
    result = result,
    diff = result - ref
  )
}

test_that("create_derived_features creates correct columns", {
  df <- make_raw_fixture()

  result <- create_derived_features(df, verbose = FALSE)

  expect_true("dist_diff" %in% colnames(result))
  expect_true("avg_dist_center" %in% colnames(result))
  expect_true("pitch_diff" %in% colnames(result))
  expect_true("avg_pitch" %in% colnames(result))
  expect_true("splice_loss" %in% colnames(result))

  # Check calculations
  expect_equal(result$dist_diff[1], 0.5)
  expect_equal(result$avg_dist_center[1], 1.25)
  expect_equal(result$pitch_diff[1], 0.5)
  expect_equal(result$avg_pitch[1], 40.25)
})

test_that("create_derived_features derives splice_loss as ref - result", {
  df <- make_raw_fixture()

  result <- create_derived_features(df, verbose = FALSE)

  # loss = ref - result = -diff, positive for a lossy splice
  expect_equal(result$splice_loss, df$ref - df$result)
  expect_equal(result$splice_loss, -df$diff)
  expect_equal(result$splice_loss[1], 0.12)
  expect_true(result$splice_loss[3] < 0)
})

test_that("create_derived_features refuses to run without diff", {
  df <- make_raw_fixture()
  df$diff <- NULL

  expect_error(create_derived_features(df, verbose = FALSE), "'diff' is required")
})

test_that("filter_nonpositive_loss drops non-positive losses by default", {
  df <- create_derived_features(make_raw_fixture(), verbose = FALSE)
  config <- list(data = list(drop_nonpositive_loss = TRUE, min_loss_db = 0))

  kept <- filter_nonpositive_loss(df, config, verbose = FALSE)

  expect_equal(nrow(kept), 2)
  expect_true(all(kept$splice_loss > 0))
})

test_that("filter_nonpositive_loss honours min_loss_db and the opt-out", {
  df <- create_derived_features(make_raw_fixture(), verbose = FALSE)

  # A floor above the smaller positive loss removes that row too
  strict <- filter_nonpositive_loss(
    df, list(data = list(min_loss_db = 0.2)), verbose = FALSE)
  expect_equal(nrow(strict), 1)
  expect_true(all(strict$splice_loss > 0.2))

  # Opting out keeps every row, non-positive losses included
  unfiltered <- filter_nonpositive_loss(
    df, list(data = list(drop_nonpositive_loss = FALSE)), verbose = FALSE)
  expect_equal(nrow(unfiltered), nrow(df))

  # An empty config block defaults to dropping
  defaulted <- filter_nonpositive_loss(df, list(data = list()), verbose = FALSE)
  expect_true(all(defaulted$splice_loss > 0))
})

test_that("remove_columns removes specified columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  result <- remove_columns(df, c("b", "c"), verbose = FALSE)

  expect_true("a" %in% colnames(result))
  expect_false("b" %in% colnames(result))
  expect_false("c" %in% colnames(result))
})

test_that("remove_columns handles non-existent columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  result <- remove_columns(df, c("c", "d"), verbose = FALSE)

  expect_equal(ncol(result), 2)
})

test_that("convert_to_factors converts correct columns", {
  df <- data.frame(
    fiber1 = c("A", "B", "A"),
    fiber2 = c("C", "D", "C"),
    splice_type = c("Self", "Cross", "Self"),
    result = c(0.1, 0.2, 0.15)
  )

  result <- convert_to_factors(df, verbose = FALSE)

  expect_s3_class(result$fiber1, "factor")
  expect_s3_class(result$fiber2, "factor")
  expect_s3_class(result$splice_type, "factor")
  expect_type(result$result, "double")
})

test_that("apply_column_mapping renames columns", {
  df <- data.frame(V1 = 1:3, V2 = 4:6)
  config <- list(data = list(column_mapping = c("col_a", "col_b")))

  result <- apply_column_mapping(df, config)

  expect_equal(colnames(result), c("col_a", "col_b"))
})
