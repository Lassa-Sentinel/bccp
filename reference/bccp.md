# Bin-Conditional Conformal Prediction

Implements bin-conditional conformal prediction for generating valid
prediction intervals. Based on Blair, Coppock & Moor (2024).

## Usage

``` r
bccp(
  calib_set,
  test_set,
  y_col = "y",
  pred_col = "pred",
  id_col = NULL,
  n_bins = 4,
  zero_bin = TRUE,
  alpha = 0.05,
  grid_length = 1000,
  return_full = FALSE,
  seed = 42
)
```

## Arguments

- calib_set:

  Calibration data frame containing observed values and predictions

- test_set:

  Test data frame to receive prediction intervals

- y_col:

  Name of the response column (default: "y")

- pred_col:

  Name of the prediction column (default: "pred")

- id_col:

  Optional column name with row IDs

- n_bins:

  Number of bins for outcome values

- zero_bin:

  Whether to use a separate bin for zeros (useful for count data)

- alpha:

  Target miscoverage rate (e.g., 0.1 for 90% coverage)

- grid_length:

  Number of grid points for interval construction

- return_full:

  If TRUE, returns all bin-wise intervals

- seed:

  Random seed for reproducibility

## Value

Data frame with prediction intervals (lower, upper columns)

## References

Blair, G., Coppock, A., & Moor, M. (2024). Bin-Conditional Conformal
Prediction. arXiv:2410.14507. <https://arxiv.org/abs/2410.14507>

## Examples

``` r
# Generate synthetic data
set.seed(42)
n <- 500
x <- runif(n, 0, 10)
y <- rpois(n, lambda = exp(0.5 + 0.2 * x))
pred <- exp(0.5 + 0.2 * x)  # True mean

# Split into calibration and test
calib_idx <- 1:400
test_idx <- 401:500

calib_set <- data.frame(y = y[calib_idx], pred = pred[calib_idx])
test_set <- data.frame(y = y[test_idx], pred = pred[test_idx])

# Apply BCCP
result <- bccp(calib_set, test_set, alpha = 0.1)

# Check coverage
coverage <- mean(result$y >= result$lower & result$y <= result$upper)
cat("Coverage:", round(coverage, 3), "\n")
#> Coverage: 0.91 
```
