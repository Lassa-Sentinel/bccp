test_that("bccp produces valid intervals", {
  set.seed(42)
  n <- 500
  y <- rpois(n, lambda = 5)
  pred <- rep(5, n)

  calib_set <- data.frame(y = y[1:400], pred = pred[1:400])
  test_set <- data.frame(y = y[401:500], pred = pred[401:500])

  result <- bccp(calib_set, test_set, alpha = 0.1)

  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_equal(nrow(result), 100)

  # Check intervals are valid (lower <= upper)
  valid_rows <- !is.na(result$lower) & !is.na(result$upper)
  expect_true(all(result$lower[valid_rows] <= result$upper[valid_rows]))
})

test_that("bccp achieves approximate coverage", {
  set.seed(123)
  n <- 1000
  y <- rpois(n, lambda = 5)
  pred <- rep(5, n)

  calib_set <- data.frame(y = y[1:800], pred = pred[1:800])
  test_set <- data.frame(y = y[801:1000], pred = pred[801:1000])

  result <- bccp(calib_set, test_set, alpha = 0.1)

  coverage <- mean(result$y >= result$lower & result$y <= result$upper, na.rm = TRUE)

  # Coverage should be at least 1 - alpha (with some tolerance)
  expect_true(coverage >= 0.85)
})

test_that("bccp handles zero-inflated data", {
  set.seed(42)
  n <- 500
  # Zero-inflated Poisson
  y <- ifelse(runif(n) < 0.5, 0, rpois(n, lambda = 5))
  pred <- rep(2.5, n)  # Mean of zero-inflated


  calib_set <- data.frame(y = y[1:400], pred = pred[1:400])
  test_set <- data.frame(y = y[401:500], pred = pred[401:500])

  result <- bccp(calib_set, test_set, zero_bin = TRUE, alpha = 0.1)

  expect_true("lower" %in% names(result))
  expect_true(all(result$lower >= 0, na.rm = TRUE))
})

test_that("bccp_seasonal produces time-varying intervals", {
  set.seed(42)
  n <- 500
  dates <- seq(as.Date("2020-01-01"), by = "week", length.out = n)
  month <- as.integer(format(dates, "%m"))

  # Strong seasonality
  seasonal <- ifelse(month %in% c(1, 2, 12), 10, 1)
  y <- rpois(n, lambda = seasonal)
  pred <- seasonal

  df <- data.frame(date = dates, y = y, pred = pred)
  calib_set <- df[1:400, ]
  test_set <- df[401:500, ]

  result <- bccp_seasonal(calib_set, test_set, date_col = "date", alpha = 0.1)

  expect_true("time_stratum" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
})

test_that("bccp_seasonal excludes empty strata correctly", {
  set.seed(42)
  n <- 500
  dates <- seq(as.Date("2020-01-01"), by = "week", length.out = n)
  month <- as.integer(format(dates, "%m"))

  # Only high counts in winter
  y <- ifelse(month %in% c(1, 2, 12), rpois(n, 20), rpois(n, 1))
  pred <- ifelse(month %in% c(1, 2, 12), 20, 1)

  df <- data.frame(date = dates, y = y, pred = pred)
  calib_set <- df[1:400, ]
  test_set <- df[401:500, ]

  result_exclude <- bccp_seasonal(calib_set, test_set, date_col = "date",
                                   exclude_empty_strata = TRUE, alpha = 0.1)
  result_pooled <- bccp_seasonal(calib_set, test_set, date_col = "date",
                                  exclude_empty_strata = FALSE, alpha = 0.1)

  # Excluding empty strata should produce narrower intervals
  width_exclude <- mean(result_exclude$upper - result_exclude$lower, na.rm = TRUE)
  width_pooled <- mean(result_pooled$upper - result_pooled$lower, na.rm = TRUE)

  expect_true(width_exclude <= width_pooled)
})

test_that("coverage_metrics calculates correctly", {
  result <- data.frame(
    y = c(1, 2, 3, 4, 5),
    lower = c(0, 1, 2, 3, 4),
    upper = c(2, 3, 4, 5, 6)
  )

  metrics <- coverage_metrics(result)

  expect_equal(metrics$coverage, 1.0)
  expect_equal(metrics$mean_width, 2.0)
  expect_equal(metrics$n_total, 5)
})

test_that("split_data works correctly", {
  df <- data.frame(y = 1:100, pred = rep(50, 100))

  # Random split
  splits <- split_data(df, calib_prop = 0.8)
  expect_equal(nrow(splits$calib_set), 80)
  expect_equal(nrow(splits$test_set), 20)

  # Temporal split
  df$date <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  splits_temporal <- split_data(df, calib_prop = 0.8, date_col = "date")
  expect_equal(nrow(splits_temporal$calib_set), 80)

  # Check temporal ordering
  expect_true(max(splits_temporal$calib_set$date) < min(splits_temporal$test_set$date))
})

test_that("generate_synthetic_data produces valid output", {
  df <- generate_synthetic_data(n = 100)

  expect_equal(nrow(df), 100)
  expect_true("date" %in% names(df))
  expect_true("y" %in% names(df))
  expect_true("pred" %in% names(df))
  expect_true("month" %in% names(df))

  # y should be non-negative integers

  expect_true(all(df$y >= 0))
  expect_true(all(df$y == floor(df$y)))

  # pred should be positive
  expect_true(all(df$pred > 0))
})
