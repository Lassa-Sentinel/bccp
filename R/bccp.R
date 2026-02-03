#' @title Bin-Conditional Conformal Prediction
#' @description Implements bin-conditional conformal prediction for generating
#'   valid prediction intervals with coverage guarantees within user-specified
#'   outcome bins.
#' @name bccp-package
#' @importFrom stats quantile setNames
#' @importFrom dplyr filter mutate group_by slice_head slice_tail ungroup arrange summarise n
NULL

#' Bin-Conditional Conformal Prediction
#'
#' Implements bin-conditional conformal prediction for generating valid
#' prediction intervals. Based on Blair, Coppock & Moor (2024).
#'
#' @param calib_set Calibration data frame containing observed values and predictions
#' @param test_set Test data frame to receive prediction intervals
#' @param y_col Name of the response column (default: "y")
#' @param pred_col Name of the prediction column (default: "pred")
#' @param id_col Optional column name with row IDs
#' @param n_bins Number of bins for outcome values
#' @param zero_bin Whether to use a separate bin for zeros (useful for count data)
#' @param alpha Target miscoverage rate (e.g., 0.1 for 90% coverage)
#' @param grid_length Number of grid points for interval construction
#' @param return_full If TRUE, returns all bin-wise intervals
#' @param seed Random seed for reproducibility
#' @return Data frame with prediction intervals (lower, upper columns)
#' @export
#' @references
#' Blair, G., Coppock, A., & Moor, M. (2024). Bin-Conditional Conformal Prediction.
#' arXiv:2410.14507. \url{https://arxiv.org/abs/2410.14507}
#' @examples
#' # Generate synthetic data
#' set.seed(42)
#' n <- 500
#' x <- runif(n, 0, 10)
#' y <- rpois(n, lambda = exp(0.5 + 0.2 * x))
#' pred <- exp(0.5 + 0.2 * x)  # True mean
#'
#' # Split into calibration and test
#' calib_idx <- 1:400
#' test_idx <- 401:500
#'
#' calib_set <- data.frame(y = y[calib_idx], pred = pred[calib_idx])
#' test_set <- data.frame(y = y[test_idx], pred = pred[test_idx])
#'
#' # Apply BCCP
#' result <- bccp(calib_set, test_set, alpha = 0.1)
#'
#' # Check coverage
#' coverage <- mean(result$y >= result$lower & result$y <= result$upper)
#' cat("Coverage:", round(coverage, 3), "\n")
bccp <- function(calib_set,
                 test_set,
                 y_col = "y",
                 pred_col = "pred",
                 id_col = NULL,
                 n_bins = 4,
                 zero_bin = TRUE,
                 alpha = 0.05,
                 grid_length = 1000,
                 return_full = FALSE,
                 seed = 42) {
  set.seed(seed)

  # Pull columns

  y_calib <- calib_set[[y_col]]
  pred_calib <- calib_set[[pred_col]]
  pred_test <- test_set[[pred_col]]
  y_test <- test_set[[y_col]]

  # Non-conformity scores
  ncs_cal <- abs(y_calib - pred_calib)
  y_cal <- y_calib

  # Build bins
  if (zero_bin) {
    is_zero <- y_cal == 0 | is.na(y_cal)
    y_pos <- y_cal[!is_zero]
    y_pos_valid <- y_pos[!is.na(y_pos)]

    if (length(y_pos_valid) == 0) {
      # All values are zero or NA
      lower <- rep(0, nrow(test_set))
      upper <- rep(max(1, max(pred_test, na.rm = TRUE) * 2), nrow(test_set))
      result_df <- data.frame(
        id = if (!is.null(id_col)) test_set[[id_col]] else seq_len(nrow(test_set)),
        y = y_test,
        pred = pred_test,
        lower = lower,
        upper = upper
      )
      return(result_df)
    }

    br_pos <- seq(0.5, max(y_pos_valid), length.out = n_bins + 1)

    bins_cal <- factor(
      ifelse(is_zero, "zero",
             as.character(cut(y_cal, br_pos, include.lowest = TRUE, right = TRUE))),
      levels = c("zero", levels(cut(y_pos_valid, br_pos, include.lowest = TRUE, right = TRUE)))
    )

    edges <- rbind(
      zero = c(lower = 0, upper = 0.5),
      cbind(lower = br_pos[-length(br_pos)], upper = br_pos[-1])
    )
    rownames(edges) <- levels(bins_cal)
  } else {
    br_all <- seq(min(y_cal, na.rm = TRUE), max(y_cal, na.rm = TRUE),
                  length.out = n_bins + 1)
    br_all[1] <- -Inf
    br_all[length(br_all)] <- Inf

    bins_cal <- cut(y_cal, br_all, include.lowest = TRUE, right = TRUE)
    edges <- cbind(lower = br_all[-length(br_all)], upper = br_all[-1])
    rownames(edges) <- levels(bins_cal)
  }

  # Per-bin quantile of non-conformity scores
  bin_levels <- levels(bins_cal)
  bin_info <- lapply(bin_levels, function(bl) {
    idx <- which(bins_cal == bl)
    list(q_hat = stats::quantile(ncs_cal[idx], probs = 1 - alpha, type = 8))
  })
  names(bin_info) <- bin_levels

  min_y <- min(y_cal, na.rm = TRUE)
  max_y <- max(y_cal, na.rm = TRUE)

  # Function to determine bin for a hypothetical y value
  get_bin <- function(y_val) {
    if (zero_bin && y_val <= 0.5) {
      return("zero")
    }
    for (bl in bin_levels) {
      if (bl == "zero") next
      lo <- edges[bl, "lower"]
      hi <- edges[bl, "upper"]
      if (y_val > lo && y_val <= hi) {
        return(bl)
      }
      if (lo == 0.5 && y_val > 0.5 && y_val <= hi) {
        return(bl)
      }
    }
    if (y_val > max_y) return(bin_levels[length(bin_levels)])
    return(bin_levels[2])
  }

  # Build intervals for one test point
  build_intervals <- function(pred_i) {
    y_grid <- seq(max(0, min_y), max_y, length.out = grid_length)

    in_set <- vapply(y_grid, function(y_val) {
      bl <- get_bin(y_val)
      q <- bin_info[[bl]]$q_hat
      if (is.na(q)) return(FALSE)
      abs(y_val - pred_i) <= q
    }, logical(1))

    if (!any(in_set)) {
      if (return_full) return(list())
      return(c(NA, NA))
    }

    y_in_set <- y_grid[in_set]

    if (return_full) {
      per_bin <- lapply(bin_levels, function(bl) {
        lo <- edges[bl, "lower"]
        hi <- edges[bl, "upper"]
        if (!is.finite(lo)) lo <- min_y
        if (!is.finite(hi)) hi <- max_y
        y_in_bin <- y_in_set[y_in_set >= lo & y_in_set <= hi]
        if (length(y_in_bin) == 0) return(NULL)
        c(min(y_in_bin), max(y_in_bin))
      })
      names(per_bin) <- bin_levels
      per_bin <- per_bin[!vapply(per_bin, is.null, logical(1))]
      return(per_bin)
    }

    c(min(y_in_set), max(y_in_set))
  }

  # Handle empty test set
  if (nrow(test_set) == 0) {
    return(data.frame(lower = numeric(0), upper = numeric(0)))
  }

  interval_list <- lapply(pred_test, build_intervals)

  # Package output
  n_test <- nrow(test_set)
  id_vals <- if (!is.null(id_col)) test_set[[id_col]] else seq_len(n_test)

  result_df <- data.frame(
    id = id_vals,
    y = y_test,
    pred = pred_test
  )

  if (return_full) {
    result_df$intervals_by_bin <- I(interval_list)
  } else {
    result_df$lower <- vapply(interval_list, `[[`, numeric(1), 1)
    result_df$upper <- vapply(interval_list, `[[`, numeric(1), 2)
  }

  # Store bin info as attribute

  attr(result_df, "bin_thresholds") <- bin_info
  attr(result_df, "bin_edges") <- edges


  result_df
}


#' Seasonal Bin-Conditional Conformal Prediction
#'
#' Extends BCCP to handle non-stationarity by conditioning on both outcome bins
#' AND time period. This produces tighter intervals during low-activity periods
#' by computing separate thresholds for each time stratum.
#'
#' @param calib_set Calibration data frame containing y, pred, and time info
#' @param test_set Test data frame to receive prediction intervals
#' @param y_col Name of the response column (default: "y")
#' @param pred_col Name of the prediction column (default: "pred")
#' @param time_col Column name for time stratification (e.g., "month", "season").
#'   If NULL and date_col exists, months are extracted automatically.
#' @param date_col Name of date column (used if time_col is NULL)
#' @param id_col Optional column name with row IDs
#' @param n_bins Number of bins for positive values
#' @param zero_bin Whether to use a separate bin for zeros
#' @param alpha Target miscoverage rate (e.g., 0.1 for 90% coverage)
#' @param grid_length Number of grid points for interval construction
#' @param min_stratum_size Minimum calibration points per stratum. Strata with
#'   fewer points fall back to pooled (non-seasonal) thresholds.
#' @param exclude_empty_strata If TRUE (default), bins with zero observations in
#'   a time stratum are excluded from prediction sets. This produces tighter
#'   intervals during low-activity periods.
#' @param season_mapping Optional named vector mapping months to seasons,
#'   e.g., c("1"="winter", "2"="winter", ..., "6"="summer", ...).
#' @param seed Random seed for reproducibility
#' @return Data frame with prediction intervals and time_stratum column
#' @export
#' @references
#' Blair, G., Coppock, A., & Moor, M. (2024). Bin-Conditional Conformal Prediction.
#' arXiv:2410.14507. \url{https://arxiv.org/abs/2410.14507}
#' @examples
#' # Generate seasonal synthetic data
#' set.seed(42)
#' n <- 1000
#' dates <- seq(as.Date("2020-01-01"), by = "week", length.out = n)
#' month <- as.integer(format(dates, "%m"))
#'
#' # Seasonal mean: higher in winter (months 1-3, 11-12)
#' seasonal_effect <- ifelse(month %in% c(1, 2, 3, 11, 12), 2, 0.5)
#' y <- rpois(n, lambda = seasonal_effect * 3)
#' pred <- seasonal_effect * 3  # Perfect seasonal prediction
#'
#' # Add noise to predictions
#' pred <- pmax(0.1, pred + rnorm(n, 0, 0.5))
#'
#' df <- data.frame(date = dates, y = y, pred = pred)
#' calib_set <- df[1:800, ]
#' test_set <- df[801:1000, ]
#'
#' # Apply seasonal BCCP
#' result <- bccp_seasonal(calib_set, test_set, date_col = "date", alpha = 0.1)
#'
#' # Check coverage
#' coverage <- mean(result$y >= result$lower & result$y <= result$upper, na.rm = TRUE)
#' cat("Coverage:", round(coverage, 3), "\n")
bccp_seasonal <- function(calib_set,
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
                          seed = 42) {
  set.seed(seed)

  # Determine time strata
  get_time_stratum <- function(df) {
    if (!is.null(time_col) && time_col %in% names(df)) {
      return(as.character(df[[time_col]]))
    }
    if (date_col %in% names(df)) {
      months <- as.integer(format(as.Date(df[[date_col]]), "%m"))
      if (!is.null(season_mapping)) {
        return(season_mapping[as.character(months)])
      }
      return(as.character(months))
    }
    return(rep("all", nrow(df)))
  }

  time_cal <- get_time_stratum(calib_set)
  time_test <- get_time_stratum(test_set)

  # Pull columns
  y_calib <- calib_set[[y_col]]
  pred_calib <- calib_set[[pred_col]]
  pred_test <- test_set[[pred_col]]
  y_test <- test_set[[y_col]]

  # Non-conformity scores
  ncs_cal <- abs(y_calib - pred_calib)
  y_cal <- y_calib

  # Build outcome bins
  if (zero_bin) {
    is_zero <- y_cal == 0 | is.na(y_cal)
    y_pos <- y_cal[!is_zero]
    y_pos_valid <- y_pos[!is.na(y_pos)]

    if (length(y_pos_valid) == 0) {
      lower <- rep(0, nrow(test_set))
      upper <- rep(max(1, max(pred_test, na.rm = TRUE) * 2), nrow(test_set))
      result_df <- data.frame(
        id = if (!is.null(id_col)) test_set[[id_col]] else seq_len(nrow(test_set)),
        y = y_test,
        pred = pred_test,
        lower = lower,
        upper = upper,
        time_stratum = time_test
      )
      return(result_df)
    }

    br_pos <- seq(0.5, max(y_pos_valid), length.out = n_bins + 1)

    bins_cal <- factor(
      ifelse(is_zero, "zero",
             as.character(cut(y_cal, br_pos, include.lowest = TRUE, right = TRUE))),
      levels = c("zero", levels(cut(y_pos_valid, br_pos, include.lowest = TRUE, right = TRUE)))
    )

    edges <- rbind(
      zero = c(lower = 0, upper = 0.5),
      cbind(lower = br_pos[-length(br_pos)], upper = br_pos[-1])
    )
    rownames(edges) <- levels(bins_cal)
  } else {
    br_all <- seq(min(y_cal, na.rm = TRUE), max(y_cal, na.rm = TRUE),
                  length.out = n_bins + 1)
    br_all[1] <- -Inf
    br_all[length(br_all)] <- Inf

    bins_cal <- cut(y_cal, br_all, include.lowest = TRUE, right = TRUE)
    edges <- cbind(lower = br_all[-length(br_all)], upper = br_all[-1])
    rownames(edges) <- levels(bins_cal)
  }

  bin_levels <- levels(bins_cal)
  time_levels <- unique(c(time_cal, time_test))

  # Compute pooled thresholds as fallback
  pooled_thresholds <- lapply(bin_levels, function(bl) {
    idx <- which(bins_cal == bl)
    if (length(idx) == 0) return(NA)
    stats::quantile(ncs_cal[idx], probs = 1 - alpha, type = 8)
  })
  names(pooled_thresholds) <- bin_levels

  # Compute time-stratified thresholds
  stratum_thresholds <- list()
  stratum_counts <- list()

  for (t in time_levels) {
    stratum_thresholds[[t]] <- list()
    stratum_counts[[t]] <- list()

    for (bl in bin_levels) {
      idx <- which(bins_cal == bl & time_cal == t)
      stratum_counts[[t]][[bl]] <- length(idx)

      if (length(idx) >= min_stratum_size) {
        stratum_thresholds[[t]][[bl]] <- stats::quantile(ncs_cal[idx], probs = 1 - alpha, type = 8)
      } else if (length(idx) == 0 && exclude_empty_strata) {
        stratum_thresholds[[t]][[bl]] <- 0
      } else {
        stratum_thresholds[[t]][[bl]] <- pooled_thresholds[[bl]]
      }
    }
  }

  min_y <- min(y_cal, na.rm = TRUE)
  max_y <- max(y_cal, na.rm = TRUE)

  get_bin <- function(y_val) {
    if (zero_bin && y_val <= 0.5) {
      return("zero")
    }
    for (bl in bin_levels) {
      if (bl == "zero") next
      lo <- edges[bl, "lower"]
      hi <- edges[bl, "upper"]
      if (y_val > lo && y_val <= hi) {
        return(bl)
      }
      if (lo == 0.5 && y_val > 0.5 && y_val <= hi) {
        return(bl)
      }
    }
    if (y_val > max_y) return(bin_levels[length(bin_levels)])
    return(bin_levels[2])
  }

  build_intervals_seasonal <- function(pred_i, t_i) {
    y_grid <- seq(max(0, min_y), max_y, length.out = grid_length)

    thresholds <- stratum_thresholds[[t_i]]
    if (is.null(thresholds)) {
      thresholds <- pooled_thresholds
    }

    in_set <- vapply(y_grid, function(y_val) {
      bl <- get_bin(y_val)
      q <- thresholds[[bl]]
      if (is.null(q) || is.na(q)) return(FALSE)
      abs(y_val - pred_i) <= q
    }, logical(1))

    if (!any(in_set)) {
      return(c(NA, NA))
    }

    y_in_set <- y_grid[in_set]
    c(min(y_in_set), max(y_in_set))
  }

  if (nrow(test_set) == 0) {
    return(data.frame(lower = numeric(0), upper = numeric(0), time_stratum = character(0)))
  }

  interval_list <- mapply(
    build_intervals_seasonal,
    pred_test,
    time_test,
    SIMPLIFY = FALSE
  )

  n_test <- nrow(test_set)
  id_vals <- if (!is.null(id_col)) test_set[[id_col]] else seq_len(n_test)

  result_df <- data.frame(
    id = id_vals,
    y = y_test,
    pred = pred_test,
    time_stratum = time_test
  )

  result_df$lower <- vapply(interval_list, `[[`, numeric(1), 1)
  result_df$upper <- vapply(interval_list, `[[`, numeric(1), 2)

  attr(result_df, "stratum_thresholds") <- stratum_thresholds
  attr(result_df, "stratum_counts") <- stratum_counts
  attr(result_df, "pooled_thresholds") <- pooled_thresholds

  result_df
}


#' Calculate Coverage Metrics
#'
#' Calculates coverage and interval width statistics for conformal prediction intervals.
#'
#' @param result Output from bccp or bccp_seasonal function
#' @param y_col Name of the observed values column (default: "y")
#' @param lower_col Name of the lower bound column (default: "lower")
#' @param upper_col Name of the upper bound column (default: "upper")
#' @return Named list with coverage and mean_width
#' @export
#' @examples
#' # Generate data and apply BCCP
#' set.seed(42)
#' n <- 500
#' y <- rpois(n, lambda = 5)
#' pred <- rep(5, n)
#'
#' calib_set <- data.frame(y = y[1:400], pred = pred[1:400])
#' test_set <- data.frame(y = y[401:500], pred = pred[401:500])
#'
#' result <- bccp(calib_set, test_set, alpha = 0.1)
#' metrics <- coverage_metrics(result)
#' print(metrics)
coverage_metrics <- function(result,
                             y_col = "y",
                             lower_col = "lower",
                             upper_col = "upper") {
  y <- result[[y_col]]
  lower <- result[[lower_col]]
  upper <- result[[upper_col]]

  covered <- !is.na(lower) & !is.na(upper) &
    y >= lower & y <= upper
  width <- upper - lower

  list(
    coverage = mean(covered, na.rm = TRUE),
    mean_width = mean(width, na.rm = TRUE),
    median_width = stats::median(width, na.rm = TRUE),
    n_valid = sum(!is.na(lower) & !is.na(upper)),
    n_total = length(y)
  )
}


#' Split Data for Conformal Prediction
#'
#' Splits data into calibration and test sets for conformal prediction.
#'
#' @param df Data frame to split
#' @param calib_prop Proportion of data for calibration (default: 0.8)
#' @param by_group Optional column name for stratified splitting
#' @param date_col Optional date column for temporal splitting
#' @param seed Random seed for reproducibility
#' @return List with calib_set and test_set
#' @export
#' @examples
#' # Random split
#' df <- data.frame(y = rpois(100, 5), pred = rep(5, 100))
#' splits <- split_data(df, calib_prop = 0.8)
#'
#' # Temporal split
#' df$date <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
#' splits <- split_data(df, calib_prop = 0.8, date_col = "date")
split_data <- function(df,
                       calib_prop = 0.8,
                       by_group = NULL,
                       date_col = NULL,
                       seed = 42) {
  set.seed(seed)
  n <- nrow(df)

  if (!is.null(date_col) && date_col %in% names(df)) {
    # Temporal split: use first calib_prop for calibration
    df <- df[order(df[[date_col]]), ]
    calib_n <- floor(n * calib_prop)
    calib_set <- df[1:calib_n, ]
    test_set <- df[(calib_n + 1):n, ]
  } else if (!is.null(by_group) && by_group %in% names(df)) {
    # Stratified split
    calib_set <- df %>%
      dplyr::group_by(.data[[by_group]]) %>%
      dplyr::slice_head(prop = calib_prop) %>%
      dplyr::ungroup()

    test_set <- df %>%
      dplyr::group_by(.data[[by_group]]) %>%
      dplyr::slice_tail(prop = 1 - calib_prop) %>%
      dplyr::ungroup()
  } else {
    # Random split
    idx <- sample(n)
    calib_n <- floor(n * calib_prop)
    calib_set <- df[idx[1:calib_n], ]
    test_set <- df[idx[(calib_n + 1):n], ]
  }

  list(calib_set = calib_set, test_set = test_set)
}
