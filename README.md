# bccp

Bin-Conditional Conformal Prediction for R

## Overview

The `bccp` package implements bin-conditional conformal prediction (BCCP) for generating valid prediction intervals with coverage guarantees within user-specified outcome bins. This is particularly useful for count data and time series forecasting where traditional conformal prediction methods may not provide adequate coverage across different outcome ranges.

Based on Blair, Coppock & Moor (2024) [arXiv:2410.14507](https://arxiv.org/abs/2410.14507).

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("lassa-sentinel/bccp")
```

## Features

- **Bin-conditional intervals**: Separate prediction intervals for different outcome ranges, ensuring coverage guarantees within each bin
- **Zero-inflation handling**: Optional separate bin for zero values, useful for count data with excess zeros
- **Seasonal extension**: `bccp_seasonal()` handles non-stationarity by conditioning on both outcome bins AND time periods
- **Coverage metrics**: Built-in functions to evaluate interval coverage and width

## Quick Start

```r
library(bccp)

# Generate synthetic data
set.seed(42)
n <- 500
x <- runif(n, 0, 10)
y <- rpois(n, lambda = exp(0.5 + 0.2 * x))
pred <- exp(0.5 + 0.2 * x)

# Split into calibration and test
calib_set <- data.frame(y = y[1:400], pred = pred[1:400])
test_set <- data.frame(y = y[401:500], pred = pred[401:500])

# Apply BCCP with 90% target coverage
result <- bccp(calib_set, test_set, alpha = 0.1)

# Check coverage
metrics <- coverage_metrics(result)
print(metrics)
```

## Seasonal BCCP

For time series with seasonality or non-stationarity:
```r
# Generate seasonal data
df <- generate_synthetic_data(n = 500, start_date = "2020-01-01")

# Split temporally
splits <- split_data(df, calib_prop = 0.8, date_col = "date")

# Apply seasonal BCCP
result <- bccp_seasonal(
  splits$calib_set,
  splits$test_set,
  date_col = "date",
  alpha = 0.1
)

# Evaluate
metrics <- coverage_metrics(result)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `bccp()` | Bin-conditional conformal prediction |
| `bccp_seasonal()` | Seasonal extension with time stratification |
| `coverage_metrics()` | Calculate coverage and interval width statistics |
| `split_data()` | Split data into calibration and test sets |
| `generate_synthetic_data()` | Generate synthetic count data for testing |

## References

Blair, G., Coppock, A., & Moor, M. (2024). Bin-Conditional Conformal Prediction. arXiv:2410.14507. https://arxiv.org/abs/2410.14507

## License
MIT
