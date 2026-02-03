# Tests for data processing functions

test_that("create_derived_features creates correct columns", {
  df <- data.frame(
    fiber1_dist_center = c(1.0, 2.0, 3.0),
    fiber2_dist_center = c(1.5, 2.5, 3.5),
    fiber1_pitch = c(40.0, 41.0, 42.0),
    fiber2_pitch = c(40.5, 41.5, 42.5)
  )

  result <- create_derived_features(df, verbose = FALSE)

  expect_true("dist_diff" %in% colnames(result))
  expect_true("avg_dist_center" %in% colnames(result))
  expect_true("pitch_diff" %in% colnames(result))
  expect_true("avg_pitch" %in% colnames(result))

  # Check calculations
  expect_equal(result$dist_diff[1], 0.5)
  expect_equal(result$avg_dist_center[1], 1.25)
  expect_equal(result$pitch_diff[1], 0.5)
  expect_equal(result$avg_pitch[1], 40.25)
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
