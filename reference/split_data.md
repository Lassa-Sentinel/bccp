# Split Data for Conformal Prediction

Splits data into calibration and test sets for conformal prediction.

## Usage

``` r
split_data(df, calib_prop = 0.8, by_group = NULL, date_col = NULL, seed = 42)
```

## Arguments

- df:

  Data frame to split

- calib_prop:

  Proportion of data for calibration (default: 0.8)

- by_group:

  Optional column name for stratified splitting

- date_col:

  Optional date column for temporal splitting

- seed:

  Random seed for reproducibility

## Value

List with calib_set and test_set

## Examples

``` r
# Random split
df <- data.frame(y = rpois(100, 5), pred = rep(5, 100))
splits <- split_data(df, calib_prop = 0.8)

# Temporal split
df$date <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
splits <- split_data(df, calib_prop = 0.8, date_col = "date")
```
