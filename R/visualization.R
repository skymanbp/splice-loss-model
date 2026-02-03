# ============================================================
# Visualization Functions for Splice Loss GLMM Model
# ============================================================

#' Create all exploratory data analysis plots
#' @param df Processed data frame
#' @param config Configuration list
#' @param verbose Print messages
create_eda_plots <- function(df, config, verbose = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }

  if (verbose) log_message("Creating EDA plots...")

  viz_config <- config$visualization
  output_config <- config$output

  # Create distribution plot
  p1 <- create_distribution_plot(df, viz_config)
  save_plot(p1, get_output_path(config, "distribution"),
            output_config$plot_width, output_config$plot_height, verbose)

  # Create boxplot by type
  p2 <- create_boxplot_by_type(df, viz_config)
  save_plot(p2, get_output_path(config, "by_type"),
            output_config$plot_width, output_config$plot_height, verbose)

  # Create scatter plot
  p3 <- create_scatter_plot(df, viz_config)
  save_plot(p3, get_output_path(config, "vs_distance"),
            output_config$plot_width + 2, output_config$plot_height, verbose)

  if (verbose) log_message("EDA plots saved successfully")

  return(list(distribution = p1, by_type = p2, vs_distance = p3))
}

#' Create distribution histogram
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_distribution_plot <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(x = result)) +
    ggplot2::geom_histogram(
      bins = viz_config$histogram_bins,
      fill = viz_config$colors$primary,
      color = "white",
      alpha = 0.7
    ) +
    ggplot2::labs(
      title = "Distribution of Splice Loss",
      x = "Splice Loss (dB)",
      y = "Count"
    ) +
    ggplot2::theme_minimal()
}

#' Create boxplot by splice type
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_boxplot_by_type <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(x = splice_type, y = result, fill = splice_type)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::labs(
      title = "Splice Loss by Splice Type",
      x = "Splice Type",
      y = "Splice Loss (dB)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' Create scatter plot of loss vs distance
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_scatter_plot <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(x = avg_dist_center, y = result, color = splice_type)) +
    ggplot2::geom_point(alpha = viz_config$point_alpha) +
    ggplot2::geom_smooth(method = "lm", se = TRUE) +
    ggplot2::labs(
      title = "Splice Loss vs Average Distance to Center",
      x = "Average Distance to Center (micron)",
      y = "Splice Loss (dB)"
    ) +
    ggplot2::theme_minimal()
}

#' Create diagnostic plots for model
#' @param df Data frame with fitted values and residuals
#' @param config Configuration list
#' @param verbose Print messages
create_diagnostic_plots <- function(df, config, verbose = TRUE) {
  if (verbose) log_message("Creating diagnostic plots...")

  viz_config <- config$visualization
  output_config <- config$output

  # Residuals vs Fitted
  p4 <- create_residuals_plot(df, viz_config)
  save_plot(p4, get_output_path(config, "residuals"),
            output_config$plot_width, output_config$plot_height, verbose)

  # Q-Q Plot
  p5 <- create_qq_plot(df, viz_config)
  save_plot(p5, get_output_path(config, "qq_plot"),
            output_config$plot_width, output_config$plot_height, verbose)

  # Actual vs Predicted
  p6 <- create_actual_vs_predicted_plot(df, viz_config)
  save_plot(p6, get_output_path(config, "actual_vs_predicted"),
            output_config$plot_width, output_config$plot_height, verbose)

  if (verbose) log_message("Diagnostic plots saved successfully")

  return(list(residuals = p4, qq = p5, actual_vs_pred = p6))
}

#' Create residuals vs fitted plot
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_residuals_plot <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = viz_config$point_alpha, color = viz_config$colors$primary) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = viz_config$colors$line) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = viz_config$colors$secondary) +
    ggplot2::labs(
      title = "Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()
}

#' Create Q-Q plot
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_qq_plot <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq(color = viz_config$colors$primary, alpha = viz_config$point_alpha) +
    ggplot2::stat_qq_line(color = viz_config$colors$line) +
    ggplot2::labs(
      title = "Q-Q Plot of Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()
}

#' Create actual vs predicted plot
#' @param df Data frame
#' @param viz_config Visualization configuration
#' @return ggplot object
create_actual_vs_predicted_plot <- function(df, viz_config) {
  ggplot2::ggplot(df, ggplot2::aes(x = result, y = fitted)) +
    ggplot2::geom_point(alpha = viz_config$point_alpha, color = viz_config$colors$primary) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = viz_config$colors$line) +
    ggplot2::labs(
      title = "Actual vs Predicted Splice Loss",
      x = "Actual (dB)",
      y = "Predicted (dB)"
    ) +
    ggplot2::theme_minimal()
}

#' Save plot to file
#' @param plot ggplot object
#' @param filepath Output file path
#' @param width Plot width
#' @param height Plot height
#' @param verbose Print messages
save_plot <- function(plot, filepath, width, height, verbose = TRUE) {
  ggplot2::ggsave(filepath, plot, width = width, height = height)
  if (verbose) log_message(sprintf("Saved: %s", filepath))
}
