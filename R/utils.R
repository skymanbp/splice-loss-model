# ============================================================
# Utility Functions for Splice Loss GLMM Model
# ============================================================

#' Load configuration from YAML file
#' @param config_path Path to the configuration YAML file
#' @return A list containing configuration settings
load_config <- function(config_path = "config.yaml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with: install.packages('yaml')")
  }

  if (!file.exists(config_path)) {
    stop(sprintf("Configuration file not found: %s", config_path))
  }

  config <- yaml::read_yaml(config_path)
  return(config)
}

#' Ensure output directory exists
#' @param config Configuration list
ensure_output_dir <- function(config) {
  output_dir <- config$output$directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(sprintf("Created output directory: %s", output_dir))
  }
}

#' Log message with timestamp
#' @param message The message to log
#' @param verbose Whether to print the message
log_message <- function(message, verbose = TRUE) {
  if (verbose) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] %s\n", timestamp, message))
  }
}

#' Save summary to file
#' @param summary_text Text to save
#' @param file_path Path to save the summary
save_summary <- function(summary_text, file_path) {
  writeLines(summary_text, file_path)
  message(sprintf("Summary saved to: %s", file_path))
}

#' Check and install required packages
#' @param packages Vector of package names
check_packages <- function(packages) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    message("Missing packages: ", paste(missing_packages, collapse = ", "))
    message("Install with: install.packages(c('",
            paste(missing_packages, collapse = "', '"), "'))")
    return(FALSE)
  }
  return(TRUE)
}

#' Get output file path
#' @param config Configuration list
#' @param file_type Type of output file (e.g., "model_file", "distribution")
get_output_path <- function(config, file_type) {
  output_dir <- config$output$directory

  if (file_type == "model_file") {
    return(file.path(output_dir, config$output$model_file))
  } else if (file_type %in% names(config$output$plots)) {
    return(file.path(output_dir, config$output$plots[[file_type]]))
  } else {
    stop(sprintf("Unknown output file type: %s", file_type))
  }
}
