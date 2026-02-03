# Tests for utility functions

test_that("load_config loads valid YAML file", {
  # Create temporary config file
  temp_config <- tempfile(fileext = ".yaml")
  writeLines(
    "data:\n  input_file: test.xlsx\noutput:\n  directory: output",
    temp_config
  )

  config <- load_config(temp_config)

  expect_type(config, "list")
  expect_equal(config$data$input_file, "test.xlsx")
  expect_equal(config$output$directory, "output")

  unlink(temp_config)
})

test_that("load_config throws error for missing file", {
  expect_error(
    load_config("nonexistent_file.yaml"),
    "Configuration file not found"
  )
})

test_that("ensure_output_dir creates directory", {
  temp_dir <- tempfile()
  config <- list(output = list(directory = temp_dir))

  expect_false(dir.exists(temp_dir))
  ensure_output_dir(config)
  expect_true(dir.exists(temp_dir))

  unlink(temp_dir, recursive = TRUE)
})

test_that("log_message prints with timestamp", {
  expect_output(log_message("test message"), "\\[\\d{4}-\\d{2}-\\d{2}")
  expect_silent(log_message("test message", verbose = FALSE))
})

test_that("check_packages returns TRUE for installed packages", {
  expect_true(check_packages("base"))
})

test_that("get_output_path constructs correct paths", {
  config <- list(
    output = list(
      directory = "output",
      model_file = "model.rds",
      plots = list(
        distribution = "dist.png",
        qq_plot = "qq.png"
      )
    )
  )

  expect_equal(get_output_path(config, "model_file"), "output/model.rds")
  expect_equal(get_output_path(config, "distribution"), "output/dist.png")
  expect_error(get_output_path(config, "unknown"), "Unknown output file type")
})
