# ============================================================
# Data Processing Functions for Splice Loss GLMM Model
# ============================================================

#' Load and preprocess splice data
#' @param config Configuration list
#' @param verbose Print progress messages
#' @return Processed data frame
load_and_preprocess_data <- function(config, verbose = TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Install with: install.packages('readxl')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }

  # Load data
  input_file <- config$data$input_file
  if (!file.exists(input_file)) {
    stop(sprintf("Data file not found: %s", input_file))
  }

  if (verbose) log_message(sprintf("Loading data from: %s", input_file))
  df <- readxl::read_excel(input_file)

  if (verbose) log_message(sprintf("Raw data: %d rows, %d columns", nrow(df), ncol(df)))

  # Apply column mapping
  df <- apply_column_mapping(df, config)

  # Remove unnecessary columns
  df <- remove_columns(df, config$data$columns_to_remove, verbose)

  # Create derived features, including the splice_loss response
  df <- create_derived_features(df, verbose)

  # Convert to factors
  df <- convert_to_factors(df, verbose)

  # Remove missing values
  n_before <- nrow(df)
  df <- na.omit(df)
  n_removed <- n_before - nrow(df)
  if (verbose && n_removed > 0) {
    log_message(sprintf("Removed %d rows with missing values", n_removed))
  }

  # Enforce a strictly positive response for the Gamma fits
  df <- filter_nonpositive_loss(df, config, verbose)

  if (verbose) {
    log_message(sprintf("Processed data: %d rows", nrow(df)))
  }

  return(df)
}

#' Apply column name mapping
#' @param df Data frame
#' @param config Configuration list
#' @return Data frame with renamed columns
apply_column_mapping <- function(df, config) {
  expected_cols <- length(config$data$column_mapping)
  actual_cols <- ncol(df)

  if (actual_cols != expected_cols) {
    warning(sprintf("Column count mismatch: expected %d, got %d",
                    expected_cols, actual_cols))
    # Use available column names
    n_cols <- min(actual_cols, expected_cols)
    colnames(df)[1:n_cols] <- config$data$column_mapping[1:n_cols]
  } else {
    colnames(df) <- config$data$column_mapping
  }

  return(df)
}

#' Remove specified columns from data frame
#' @param df Data frame
#' @param columns_to_remove Vector of column names to remove
#' @param verbose Print messages
#' @return Data frame with columns removed
remove_columns <- function(df, columns_to_remove, verbose = TRUE) {
  existing_cols <- intersect(columns_to_remove, colnames(df))

  if (length(existing_cols) > 0) {
    # drop = FALSE: base-R data.frame subsetting would collapse a single
    # remaining column to a vector; the contract is to return a data frame
    df <- df[, !(colnames(df) %in% existing_cols), drop = FALSE]
    if (verbose) {
      log_message(sprintf("Removed columns: %s", paste(existing_cols, collapse = ", ")))
    }
  }

  return(df)
}

#' Create derived features
#'
#' Derives the model response, `splice_loss`, alongside the geometric
#' features. The raw workbook records `ref` and `result` as absolute power
#' levels in dB (both negative), and `diff = result - ref`. The physical
#' quantity of interest is the power lost across the splice,
#' `loss = ref - result = -diff`, which is positive for a lossy splice and
#' close to zero for a lossless one. `result` itself is a power level, not a
#' loss, so it must never be used as the response.
#'
#' @param df Data frame carrying at least `diff` and the pitch/distance columns
#' @param verbose Print messages
#' @return Data frame with derived features, including `splice_loss` (dB)
create_derived_features <- function(df, verbose = TRUE) {
  if (verbose) log_message("Creating derived features...")

  if (!("diff" %in% colnames(df))) {
    stop("Column 'diff' is required to derive the response 'splice_loss' (= -diff)")
  }

  df <- dplyr::mutate(df,
    # Distance difference between two fibers
    dist_diff = abs(fiber1_dist_center - fiber2_dist_center),
    # Average distance to center
    avg_dist_center = (fiber1_dist_center + fiber2_dist_center) / 2,
    # Pitch difference
    pitch_diff = abs(fiber1_pitch - fiber2_pitch),
    # Average pitch
    avg_pitch = (fiber1_pitch + fiber2_pitch) / 2,
    # Response: power lost across the splice, in dB.
    # Diff = Result - Ref, so loss = Ref - Result = -Diff.
    splice_loss = -diff
  )

  return(df)
}

#' Drop rows whose measured loss is not strictly positive
#'
#' `Gamma(link = "log")` requires a strictly positive response. A splice with
#' no measurable loss returns `splice_loss` at or just below zero, because the
#' pre- and post-splice power readings differ only by measurement noise. Those
#' rows carry no information about a loss mechanism and cannot enter a Gamma
#' likelihood, so they are dropped and the count is reported.
#'
#' Controlled by `data.drop_nonpositive_loss` (default `TRUE`) and
#' `data.min_loss_db` (default `0`) in `config.yaml`: rows are kept when
#' `splice_loss > min_loss_db`.
#'
#' @param df Data frame containing `splice_loss`
#' @param config Configuration list
#' @param verbose Print messages
#' @return Data frame with non-positive losses removed
filter_nonpositive_loss <- function(df, config, verbose = TRUE) {
  drop_nonpositive <- config$data$drop_nonpositive_loss
  if (is.null(drop_nonpositive)) drop_nonpositive <- TRUE
  if (!isTRUE(drop_nonpositive)) {
    if (verbose) {
      log_message(paste("drop_nonpositive_loss is FALSE: keeping non-positive losses;",
                        "Gamma(link = 'log') will not fit this data"))
    }
    return(df)
  }

  if (!("splice_loss" %in% colnames(df))) {
    stop("Column 'splice_loss' not found; run create_derived_features() first")
  }

  min_loss <- config$data$min_loss_db
  if (is.null(min_loss)) min_loss <- 0

  keep <- df$splice_loss > min_loss
  n_dropped <- sum(!keep)

  if (verbose && n_dropped > 0) {
    log_message(sprintf(
      paste("Dropped %d of %d rows with splice_loss <= %g dB",
            "(measurement noise around a lossless splice;",
            "Gamma(link = 'log') needs a strictly positive response)"),
      n_dropped, nrow(df), min_loss))
  }

  return(df[keep, , drop = FALSE])
}

#' Convert categorical variables to factors
#' @param df Data frame
#' @param verbose Print messages
#' @return Data frame with factors
convert_to_factors <- function(df, verbose = TRUE) {
  factor_cols <- c("fiber1", "fiber2", "splice_type", "core_no", "test_no")
  existing_factor_cols <- intersect(factor_cols, colnames(df))

  for (col in existing_factor_cols) {
    df[[col]] <- as.factor(df[[col]])
  }

  if (verbose) {
    log_message(sprintf("Converted to factors: %s",
                        paste(existing_factor_cols, collapse = ", ")))
  }

  return(df)
}

#' Print data summary
#' @param df Processed data frame
print_data_summary <- function(df) {
  cat("\n=== Data Summary ===\n")
  cat("Number of observations:", nrow(df), "\n")

  if ("fiber1" %in% colnames(df) && "fiber2" %in% colnames(df)) {
    unique_fibers <- unique(c(as.character(df$fiber1), as.character(df$fiber2)))
    cat("Number of fiber types:", length(unique_fibers), "\n")
  }

  if ("splice_type" %in% colnames(df)) {
    cat("Splice types:", paste(levels(df$splice_type), collapse = ", "), "\n")
  }

  if ("splice_loss" %in% colnames(df)) {
    cat("\nResponse variable (splice_loss = -diff) statistics:\n")
    cat("  Mean:", round(mean(df$splice_loss, na.rm = TRUE), 4), "dB\n")
    cat("  SD:", round(sd(df$splice_loss, na.rm = TRUE), 4), "dB\n")
    cat("  Min:", round(min(df$splice_loss, na.rm = TRUE), 4), "dB\n")
    cat("  Max:", round(max(df$splice_loss, na.rm = TRUE), 4), "dB\n")
  }
}
