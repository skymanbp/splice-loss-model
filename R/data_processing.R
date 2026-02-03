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

  # Create derived features
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
    df <- df[, !(colnames(df) %in% existing_cols)]
    if (verbose) {
      log_message(sprintf("Removed columns: %s", paste(existing_cols, collapse = ", ")))
    }
  }

  return(df)
}

#' Create derived features
#' @param df Data frame
#' @param verbose Print messages
#' @return Data frame with derived features
create_derived_features <- function(df, verbose = TRUE) {
  if (verbose) log_message("Creating derived features...")

  df <- dplyr::mutate(df,
    # Distance difference between two fibers
    dist_diff = abs(fiber1_dist_center - fiber2_dist_center),
    # Average distance to center
    avg_dist_center = (fiber1_dist_center + fiber2_dist_center) / 2,
    # Pitch difference
    pitch_diff = abs(fiber1_pitch - fiber2_pitch),
    # Average pitch
    avg_pitch = (fiber1_pitch + fiber2_pitch) / 2
  )

  return(df)
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

  if ("result" %in% colnames(df)) {
    cat("\nResponse variable (result) statistics:\n")
    cat("  Mean:", round(mean(df$result, na.rm = TRUE), 4), "dB\n")
    cat("  SD:", round(sd(df$result, na.rm = TRUE), 4), "dB\n")
    cat("  Min:", round(min(df$result, na.rm = TRUE), 4), "dB\n")
    cat("  Max:", round(max(df$result, na.rm = TRUE), 4), "dB\n")
  }
}
