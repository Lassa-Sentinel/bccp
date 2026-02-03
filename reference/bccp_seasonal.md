# Seasonal Bin-Conditional Conformal Prediction

Extends BCCP to handle non-stationarity by conditioning on both outcome
bins AND time period. This produces tighter intervals during
low-activity periods by computing separate thresholds for each time
stratum.

## Usage

``` r
bccp_seasonal(
  calib_set,
  test_set,
  y_col = "y",
  pred_col = "pred",
  time_col = NULL,
  date_col = "date",
  id_col = NULL,
  n_bins = 4,
  zero_bin = TRUE,
  alpha = 0.05,
  grid_length = 1000,
  min_stratum_size = 10,
  exclude_empty_strata = TRUE,
  season_mapping = NULL,
  seed = 42
)
```

## Arguments

- calib_set:

  Calibration data frame containing y, pred, and time info

- test_set:

  Test data frame to receive prediction intervals

- y_col:

  Name of the response column (default: "y")

- pred_col:

  Name of the prediction column (default: "pred")

- time_col:

  Column name for time stratification (e.g., "month", "season"). If NULL
  and date_col exists, months are extracted automatically.

- date_col:

  Name of date column (used if time_col is NULL)

- id_col:

  Optional column name with row IDs

- n_bins:

  Number of bins for positive values

- zero_bin:

  Whether to use a separate bin for zeros

- alpha:

  Target miscoverage rate (e.g., 0.1 for 90% coverage)

- grid_length:

  Number of grid points for interval construction

- min_stratum_size:

  Minimum calibration points per stratum. Strata with fewer points fall
  back to pooled (non-seasonal) thresholds.

- exclude_empty_strata:

  If TRUE (default), bins with zero observations in a time stratum are
  excluded from prediction sets. This produces tighter intervals during
  low-activity periods.

- season_mapping:

  Optional named vector mapping months to seasons, e.g., c("1"="winter",
  "2"="winter", ..., "6"="summer", ...).

- seed:

  Random seed for reproducibility

## Value

Data frame with prediction intervals and time_stratum column

## References

Blair, G., Coppock, A., & Moor, M. (2024). Bin-Conditional Conformal
Prediction. arXiv:2410.14507. <https://arxiv.org/abs/2410.14507>

## Examples

``` r
# Generate seasonal synthetic data
set.seed(42)
n <- 1000
dates <- seq(as.Date("2020-01-01"), by = "week", length.out = n)
month <- as.integer(format(dates, "%m"))

# Seasonal mean: higher in winter (months 1-3, 11-12)
seasonal_effect <- ifelse(month %in% c(1, 2, 3, 11, 12), 2, 0.5)
y <- rpois(n, lambda = seasonal_effect * 3)
pred <- seasonal_effect * 3  # Perfect seasonal prediction

# Add noise to predictions
pred <- pmax(0.1, pred + rnorm(n, 0, 0.5))

df <- data.frame(date = dates, y = y, pred = pred)
calib_set <- df[1:800, ]
test_set <- df[801:1000, ]

# Apply seasonal BCCP
result <- bccp_seasonal(calib_set, test_set, date_col = "date", alpha = 0.1)

# Check coverage
coverage <- mean(result$y >= result$lower & result$y <= result$upper, na.rm = TRUE)
cat("Coverage:", round(coverage, 3), "\n")
#> Coverage: 0.86 
```
