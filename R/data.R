#' Generate Synthetic Count Data
#'
#' Generates synthetic count data with seasonality for testing BCCP methods.
#'
#' @param n Number of observations
#' @param start_date Start date for time series
#' @param seasonal_amplitude Amplitude of seasonal effect (default: 2)
#' @param base_rate Base Poisson rate (default: 3)
#' @param pred_noise Standard deviation of prediction noise (default: 0.5)
#' @param seed Random seed
#' @return Data frame with date, y (observed), pred (predicted), and month columns
#' @export
#' @examples
#' # Generate 2 years of weekly data
#' df <- generate_synthetic_data(n = 104, start_date = "2020-01-01")
#' head(df)
#'
#' # Plot seasonal pattern
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(df, aes(x = date, y = y)) +
#'     geom_point() +
#'     geom_line(aes(y = pred), color = "blue") +
#'     theme_minimal()
#' }
generate_synthetic_data <- function(n = 500,
                                    start_date = "2020-01-01",
                                    seasonal_amplitude = 2,
                                    base_rate = 3,
                                    pred_noise = 0.5,
                                    seed = 42) {
  set.seed(seed)

  dates <- seq(as.Date(start_date), by = "week", length.out = n)
  month <- as.integer(format(dates, "%m"))

  # Seasonal effect: peak in winter months (1-3, 11-12)
  seasonal_effect <- ifelse(month %in% c(1, 2, 3, 11, 12),
                            seasonal_amplitude,
                            1 / seasonal_amplitude)

  # True mean
  true_mean <- base_rate * seasonal_effect

  # Generate counts
  y <- stats::rpois(n, lambda = true_mean)

  # Predictions with noise
  pred <- pmax(0.1, true_mean + stats::rnorm(n, 0, pred_noise))

  data.frame(
    date = dates,
    y = y,
    pred = pred,
    month = month,
    true_mean = true_mean
  )
}


