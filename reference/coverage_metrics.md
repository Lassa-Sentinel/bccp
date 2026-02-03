# Calculate Coverage Metrics

Calculates coverage and interval width statistics for conformal
prediction intervals.

## Usage

``` r
coverage_metrics(result, y_col = "y", lower_col = "lower", upper_col = "upper")
```

## Arguments

- result:

  Output from bccp or bccp_seasonal function

- y_col:

  Name of the observed values column (default: "y")

- lower_col:

  Name of the lower bound column (default: "lower")

- upper_col:

  Name of the upper bound column (default: "upper")

## Value

Named list with coverage and mean_width

## Examples

``` r
# Generate data and apply BCCP
set.seed(42)
n <- 500
y <- rpois(n, lambda = 5)
pred <- rep(5, n)

calib_set <- data.frame(y = y[1:400], pred = pred[1:400])
test_set <- data.frame(y = y[401:500], pred = pred[401:500])

result <- bccp(calib_set, test_set, alpha = 0.1)
metrics <- coverage_metrics(result)
print(metrics)
#> $coverage
#> [1] 1
#> 
#> $mean_width
#> [1] 11.62763
#> 
#> $median_width
#> [1] 11.62763
#> 
#> $n_valid
#> [1] 100
#> 
#> $n_total
#> [1] 100
#> 
```
