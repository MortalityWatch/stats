# Request Handlers
# Functions for handling forecast and cumulative forecast requests

# Load custom MEDIAN model
# Use dirname to find path relative to this script
handlers_dir <- if (exists("ofile") && !is.null(sys.frames()[[1]]$ofile)) {
  dirname(sys.frames()[[1]]$ofile)
} else {
  # Fallback: try multiple paths for different execution contexts
  paths <- c("src/median_model.r", "median_model.r", "../src/median_model.r")
  found_path <- NULL
  for (p in paths) {
    if (file.exists(p)) {
      found_path <- dirname(p)
      break
    }
  }
  if (is.null(found_path)) {
    stop("Could not find median_model.r in any expected location")
  }
  found_path
}

median_model_path <- file.path(handlers_dir, "median_model.r")
if (!file.exists(median_model_path)) {
  # Final fallback: try relative paths
  for (path in c("src/median_model.r", "median_model.r", "../src/median_model.r")) {
    if (file.exists(path)) {
      median_model_path <- path
      break
    }
  }
}

if (!file.exists(median_model_path)) {
  stop("Could not find median_model.r")
}

source(median_model_path)

#' Calculate z-scores for all periods (pre-baseline, baseline, post-baseline, forecast)
#'
#' @param y_full Full dataset including all observed data
#' @param bs Baseline start index (1-indexed)
#' @param be Baseline end index (1-indexed)
#' @param baseline_residuals Residuals from baseline model fit
#' @param mdl Fitted model object
#' @param result_length Total length of result vector (includes forecast)
#' @param h Forecast horizon
#' @param df_baseline Baseline tsibble (for generating pre-baseline predictions)
#' @return Vector of z-scores
calculate_zscores <- function(y_full, bs, be, baseline_residuals,
                               mdl, result_length, h, df_baseline) {
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)
  zscores <- rep(NA, result_length)

  # Pre-baseline z-scores (if bs > 1)
  if (bs > 1) {
    pre_baseline_data <- y_full[1:(bs - 1)]
    pre_baseline_non_na_idx <- which(!is.na(pre_baseline_data))
    pre_baseline_clean <- pre_baseline_data[pre_baseline_non_na_idx]

    if (length(pre_baseline_clean) > 0) {
      # Create new_data for pre-baseline period by going backwards from baseline start
      # We need to forecast "backwards" using new_data
      first_baseline_idx <- df_baseline$year[1]

      # Create pre-baseline time indices
      if (inherits(first_baseline_idx, "yearquarter")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else if (inherits(first_baseline_idx, "yearmonth")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else if (inherits(first_baseline_idx, "yearweek")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else {
        # Annual/integer index
        pre_indices <- (first_baseline_idx - (bs - 1)):(first_baseline_idx - 1)
      }

      pre_baseline_df <- tibble(year = pre_indices, asmr = pre_baseline_data) |>
        as_tsibble(index = year)

      # Use forecast with new_data to get predictions for pre-baseline
      pre_baseline_pred <- mdl |> forecast(new_data = pre_baseline_df)
      pre_baseline_fitted <- as_tibble(pre_baseline_pred) |> pull(.mean)

      # Calculate z-scores for pre-baseline (only non-NA positions)
      pre_baseline_residuals <- pre_baseline_clean - pre_baseline_fitted[pre_baseline_non_na_idx]
      pre_baseline_zscores <- pre_baseline_residuals / residual_sd

      zscores[pre_baseline_non_na_idx] <- round(pre_baseline_zscores, 3)
    }
  }

  # Baseline period z-scores (from model residuals)
  n_baseline_values <- length(baseline_residuals)
  zscores[bs:(bs + n_baseline_values - 1)] <-
    round(baseline_residuals / residual_sd, 3)

  # Post-baseline observed data z-scores
  if (be < length(y_full)) {
    post_baseline_data <- y_full[(be + 1):length(y_full)]
    n_post_baseline_total <- length(post_baseline_data)

    # Track which positions have non-NA values for correct z-score alignment
    post_baseline_non_na_idx <- which(!is.na(post_baseline_data))
    post_baseline_clean <- post_baseline_data[post_baseline_non_na_idx]

    if (length(post_baseline_clean) > 0) {
      # Generate fitted values for post-baseline period using baseline model
      fc_post <- mdl |> forecast(h = length(post_baseline_clean))
      post_baseline_fitted <- as_tibble(fc_post) |> pull(.mean)

      # Calculate z-scores for post-baseline
      post_baseline_residuals <- post_baseline_clean - post_baseline_fitted
      post_baseline_zscores <- post_baseline_residuals / residual_sd

      # Assign z-scores to correct positions
      zscore_indices <- be + post_baseline_non_na_idx
      zscores[zscore_indices] <- round(post_baseline_zscores, 3)
    }
  }

  # Forecast period z-scores are NA (no observed data to compare)
  # They are already NA from initialization, no need to set them

  zscores
}

#' Calculate STL residual z-scores for periodic series
#'
#' Uses STL decomposition with periodic seasonal component and trend window
#' ~3 full periods. Missing observed values are linearly interpolated for
#' decomposition only; z-scores are emitted only for originally non-NA points.
#'
#' @param y_full Full observed data vector (no forecast extension)
#' @param bs Baseline start index (1-indexed)
#' @param be Baseline end index (1-indexed)
#' @param period Seasonal period (e.g., 52 weekly, 12 monthly, 4 quarterly)
#' @param result_length Total length of output z-score vector (includes forecast)
#' @return Vector of z-scores (forecast positions are NA)
calculate_stl_residual_zscores <- function(y_full, bs, be, period, result_length) {
  zscores <- rep(NA_real_, result_length)

  if (length(y_full) < period * 2) {
    return(NULL)
  }

  observed_non_na <- which(!is.na(y_full))
  if (length(observed_non_na) < 3) {
    return(NULL)
  }

  # Interpolate missing values for decomposition only
  x <- seq_along(y_full)
  y_interp <- y_full
  y_interp[is.na(y_interp)] <- approx(
    x = x[observed_non_na],
    y = y_full[observed_non_na],
    xout = x[is.na(y_interp)],
    method = "linear",
    rule = 2
  )$y

  ts_data <- stats::ts(y_interp, frequency = period)
  max_trend_window <- length(y_interp) - 1
  trend_window <- min(period * 3, max_trend_window)
  if (trend_window < 3) {
    return(NULL)
  }
  if (trend_window %% 2 == 0) {
    trend_window <- trend_window - 1
  }

  stl_fit <- tryCatch(
    stats::stl(ts_data, s.window = "periodic", t.window = trend_window),
    error = function(e) NULL
  )
  if (is.null(stl_fit)) {
    return(NULL)
  }

  remainder <- as.numeric(stl_fit$time.series[, "remainder"])

  # Baseline SD from baseline window only (observed points)
  baseline_idx <- seq.int(bs, be)
  baseline_idx <- baseline_idx[baseline_idx >= 1 & baseline_idx <= length(y_full)]
  baseline_idx <- baseline_idx[!is.na(y_full[baseline_idx])]

  if (length(baseline_idx) < 3) {
    return(NULL)
  }

  baseline_sd <- sd(remainder[baseline_idx], na.rm = TRUE)
  if (is.na(baseline_sd) || baseline_sd <= 0) {
    return(NULL)
  }

  z_obs <- remainder / baseline_sd
  zscores[observed_non_na] <- round(z_obs[observed_non_na], 3)
  zscores
}

#' Parse xs (start time index) string into appropriate tsibble index
#'
#' @param xs Character string representing start time (e.g., "2020W10", "2020-01", "2020Q1", "2020")
#' @param s Integer seasonality type: 1=year, 2=quarter, 3=month, 4=week
#' @return Appropriate tsibble time index object
parse_xs <- function(xs, s) {
  if (is.null(xs)) return(NULL)

  xs <- as.character(xs)

  if (s == 4) {
    # Weekly: "2020W10" or "2020-W10"
    xs_normalized <- gsub("-", "", toupper(xs))
    year <- as.integer(substr(xs_normalized, 1, 4))
    week <- as.integer(sub(".*W", "", xs_normalized))
    return(make_yearweek(year, week))
  } else if (s == 3) {
    # Monthly: "2020-01" or "202001"
    xs_normalized <- gsub("-", "", xs)
    year <- as.integer(substr(xs_normalized, 1, 4))
    month <- as.integer(substr(xs_normalized, 5, 6))
    return(make_yearmonth(year, month))
  } else if (s == 2) {
    # Quarterly: "2020Q1" or "2020-Q1"
    xs_normalized <- gsub("-", "", toupper(xs))
    year <- as.integer(substr(xs_normalized, 1, 4))
    quarter <- as.integer(sub(".*Q", "", xs_normalized))
    return(make_yearquarter(year, quarter))
  } else {
    # Yearly: "2020"
    return(as.integer(xs))
  }
}

#' Handle standard forecast request
#'
#' Performs time series forecasting using various methods (naive, mean, linear regression, exponential smoothing)
#'
#' @param y Numeric vector of observed values (full dataset)
#' @param h Integer horizon for forecasting
#' @param m Character method: "naive", "mean", "lin_reg", "exp", "median"
#' @param s Integer seasonality type: 1=year, 2=quarter, 3=month, 4=week
#' @param t Boolean whether to include trend
#' @param bs Baseline start index (1-indexed, optional). If NULL, defaults to 1.
#' @param be Baseline end index (1-indexed, optional). If NULL, defaults to length(y).
#' @param xs Character start time index (optional). Format depends on s:
#'   - s=4 (weekly): "2020W10" or "2020-W10"
#'   - s=3 (monthly): "2020-01" or "202001"
#'   - s=2 (quarterly): "2020Q1" or "2020-Q1"
#'   - s=1 (yearly): "2020"
#'   If NULL, uses synthetic time indices starting from 2000.
#'
#' @details
#' Z-score Calculation:
#' Z-scores measure how much each observation deviates from what the baseline model predicts:
#' - Pre-baseline period: z = (observed - predicted) / baseline_sd (model extrapolates backwards)
#' - Baseline period: z = (observed - fitted) / baseline_sd (standard residuals)
#' - Post-baseline period: z = (observed - predicted) / baseline_sd (model forecasts forward)
#' - Forecast period: z = NA (no observed data to compare)
#'
#' This means z-scores measure deviation from the baseline model's prediction,
#' NOT from the baseline period's mean. A high z-score indicates the observed
#' value differs significantly from what the baseline model would have predicted.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleForecast <- function(y, h, m, s, t, bs = NULL, be = NULL, xs = NULL) {
  y_full <- y

  # Set defaults for baseline start/end if not provided
  if (is.null(bs)) bs <- 1L
  if (is.null(be)) be <- length(y)

  # Extract baseline data
  y_baseline <- y[bs:be]

  # Count leading NAs in the full input (for output alignment)
  # Leading NAs are NA values before the first non-NA value in the input
  first_non_na_idx <- which(!is.na(y))[1]
  leading_NA <- if (is.na(first_non_na_idx)) 0 else first_non_na_idx - 1

  # Create time series index starting from baseline start
  # Adjust for leading NAs when bs=1 (backwards compatible behavior)
  actual_bs <- if (bs == 1 && leading_NA > 0) leading_NA + 1 else bs
  y_baseline_clean <- if (bs == 1 && leading_NA > 0) y[(leading_NA + 1):be] else y_baseline

  df_baseline <- tibble(year = seq.int(actual_bs, be), asmr = y_baseline_clean)

  # Convert to appropriate time series index based on seasonality
  # Use xs if provided, otherwise fall back to synthetic indices starting from 2000

  start_index <- parse_xs(xs, s)

  if (s == 2) {
    if (is.null(start_index)) {
      start_index <- make_yearquarter(2000, 1)
    }
    # Offset start_index if baseline doesn't start at position 1
    df_baseline$year <- (start_index + (actual_bs - 1)) + 0:(length(y_baseline_clean) - 1)
  } else if (s == 3) {
    if (is.null(start_index)) {
      start_index <- make_yearmonth(2000, 1)
    }
    df_baseline$year <- (start_index + (actual_bs - 1)) + 0:(length(y_baseline_clean) - 1)
  } else if (s == 4) {
    if (is.null(start_index)) {
      start_index <- make_yearweek(2000, 1)
    }
    df_baseline$year <- (start_index + (actual_bs - 1)) + 0:(length(y_baseline_clean) - 1)
  } else if (s == 1 && !is.null(start_index)) {
    # Yearly with explicit start
    df_baseline$year <- (start_index + (actual_bs - 1)) + 0:(length(y_baseline_clean) - 1)
  }

  # Convert to tsibble (keep NAs to preserve regular time index for seasonal models)
  df_baseline <- df_baseline |>
    as_tsibble(index = year)

  # Interpolate interspersed NAs for model fitting (ETS requires no NAs,
  # and filtering NAs breaks tsibble regularity needed by season())
  if (any(is.na(df_baseline$asmr))) {
    idx <- seq_len(nrow(df_baseline))
    non_na <- !is.na(df_baseline$asmr)
    if (sum(non_na) >= 2) {
      df_baseline$asmr <- approx(idx[non_na], df_baseline$asmr[non_na], idx, rule = 2)$y
    }
  }

  # Fit model based on method
  if (m == "naive") {
    mdl <- df_baseline |> model(NAIVE(asmr))
  } else if (m == "mean") {
    if (s > 1) {
      mdl <- df_baseline |> model(TSLM(asmr ~ season()))
    } else {
      mdl <- df_baseline |> model(TSLM(asmr))
    }
  } else if (m == "median") {
    if (s > 1) {
      mdl <- df_baseline |> model(MEDIAN(asmr ~ season()))
    } else {
      mdl <- df_baseline |> model(MEDIAN(asmr))
    }
  } else if (m == "lin_reg") {
    if (t) {
      if (s > 1) {
        mdl <- df_baseline |> model(TSLM(asmr ~ trend() + season()))
      } else {
        mdl <- df_baseline |> model(TSLM(asmr ~ trend()))
      }
    } else {
      if (s > 1) {
        mdl <- df_baseline |> model(TSLM(asmr ~ season()))
      } else {
        mdl <- df_baseline |> model(TSLM(asmr))
      }
    }
  } else if (m == "exp") {
    if (s > 1) {
      mdl <- df_baseline |> model(ETS(asmr ~ error() + trend() + season()))
    } else {
      mdl <- df_baseline |> model(ETS(asmr ~ error("A") + trend("Ad")))
    }
  } else {
    stop(paste("Unknown method:", m))
  }

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Calculate predictions for all periods
  # Account for leading NAs when bs=1
  n_pre_baseline <- if (bs == 1 && leading_NA > 0) 0 else (bs - 1)
  n_post_baseline <- length(y_full) - be

  # Generate predictions for pre-baseline period (if any) - no PI
  pre_baseline_result <- if (n_pre_baseline > 0) {
    first_baseline_idx <- df_baseline$year[1]
    # Create pre-baseline time indices
    if (inherits(first_baseline_idx, "yearquarter")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else if (inherits(first_baseline_idx, "yearmonth")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else if (inherits(first_baseline_idx, "yearweek")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else {
      pre_indices <- (first_baseline_idx - n_pre_baseline):(first_baseline_idx - 1)
    }
    pre_df <- tibble(year = pre_indices, asmr = y_full[1:n_pre_baseline]) |>
      as_tsibble(index = year)
    fc_pre <- mdl |> forecast(new_data = pre_df)
    tibble(y = as_tibble(fc_pre) |> pull(.mean), lower = NA, upper = NA)
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate predictions for post-baseline period with PI (this is the "forecast" region)
  # When bs/be are explicitly provided, post-baseline gets PI
  # h is only used for extending beyond observed data (legacy mode or explicit extension)
  post_baseline_result <- if (n_post_baseline > 0) {
    fc_post <- mdl |> forecast(h = n_post_baseline)
    fc_post_hilo <- fabletools::hilo(fc_post, 95) |>
      unpack_hilo(cols = `95%`) |>
      as_tibble() |>
      select(.mean, "95%_lower", "95%_upper") |>
      setNames(c("y", "lower", "upper"))
    fc_post_hilo
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate additional forecast beyond observed data (if h > 0)
  fc_beyond_result <- if (h > 0) {
    # Forecast h periods beyond the last observed data point
    fc_beyond <- mdl |> forecast(h = n_post_baseline + h)
    # Take only the last h periods (beyond observed data)
    fc_beyond_hilo <- fabletools::hilo(fc_beyond, 95) |>
      unpack_hilo(cols = `95%`) |>
      as_tibble() |>
      select(.mean, "95%_lower", "95%_upper") |>
      setNames(c("y", "lower", "upper")) |>
      tail(h)
    fc_beyond_hilo
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Combine all predictions: leading NAs, pre-baseline, baseline, post-baseline (with PI), forecast beyond (with PI)
  result <- bind_rows(
    tibble(y = rep(NA, leading_NA), lower = NA, upper = NA),
    pre_baseline_result,
    tibble(y = bl$.mean, lower = NA, upper = NA),
    post_baseline_result,
    fc_beyond_result
  ) |>
    mutate_if(is.numeric, round, 2)

  # Calculate z-scores using helper function
  # Use actual_bs for z-score calculation when there are leading NAs
  effective_bs <- if (bs == 1 && leading_NA > 0) actual_bs else bs
  baseline_residuals <- y_baseline_clean - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    bs = effective_bs,
    be = be,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = length(result$y),
    h = h,
    df_baseline = df_baseline
  )

  # For periodic series, use STL-residual z-scores (remainder / sd(remainder_baseline))
  # instead of model residual z-scores.
  period <- switch(as.character(s), `2` = 4L, `3` = 12L, `4` = 52L, NULL)
  if (!is.null(period)) {
    stl_zscores <- calculate_stl_residual_zscores(
      y_full = y_full,
      bs = effective_bs,
      be = be,
      period = period,
      result_length = length(result$y)
    )
    if (!is.null(stl_zscores)) {
      zscores <- stl_zscores
    }
  }

  list(y = result$y, lower = result$lower, upper = result$upper, zscore = zscores)
}

#' Cumulative forecast for single horizon
#'
#' @param df_train Training data
#' @param df_test Test data
#' @param mdl Fitted model
#' @return Tibble with cumulative mean, lower, and upper bounds
cumForecastN <- function(df_train, df_test, mdl) {
  oo <- lm_predict_tslm(model = mdl, newdata = df_test, FALSE)

  fc_sum_mean <- sum(oo$fit)
  fc_sum_variance <- sum(oo$var.fit)

  n <- ncol(lengths(oo$var.fit))
  res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)

  tibble(
    asmr = round(fc_sum_mean, 2),
    lower = round(res$PI[1], 2),
    upper = round(res$PI[2], 2)
  )
}

#' Handle cumulative forecast request
#'
#' Performs cumulative forecasting for annual data with trend or mean baseline
#'
#' @param y Numeric vector of cumulative observed values (full dataset)
#' @param h Integer horizon for forecasting
#' @param t Boolean whether to include trend
#' @param bs Baseline start index (1-indexed, optional). If NULL, defaults to 1.
#' @param be Baseline end index (1-indexed, optional). If NULL, defaults to length(y).
#'
#' @details
#' Z-score calculation follows the same semantics as handleForecast():
#' measures deviation from the baseline model's prediction using baseline period's
#' standard deviation. See handleForecast() documentation for details.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleCumulativeForecast <- function(y, h, t, bs = NULL, be = NULL) {
  y_full <- y

  # Set defaults for baseline start/end if not provided
  if (is.null(bs)) bs <- 1L
  if (is.null(be)) be <- length(y)

  # Count leading NAs in the full input (for output alignment)
  # Leading NAs are NA values before the first non-NA value in the input
  first_non_na_idx <- which(!is.na(y))[1]
  leading_NA <- if (is.na(first_non_na_idx)) 0 else first_non_na_idx - 1

  # Extract baseline data (should not include leading NAs)
  y_baseline <- y[bs:be]
  n <- length(y_baseline)

  # Remove any NA values from baseline for model fitting
  y_baseline_clean <- y_baseline[!is.na(y_baseline)]

  df_baseline <- tibble(year = seq.int(bs, bs + length(y_baseline_clean) - 1), asmr = y_baseline_clean) |>
    as_tsibble(index = year)

  # Fit model with or without trend
  if (t) {
    mdl <- df_baseline |> model(lm = TSLM(asmr ~ trend()))
  } else {
    mdl <- df_baseline |> model(lm = TSLM(asmr))
  }

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Calculate pre-baseline and post-baseline lengths
  # Pre-baseline excludes leading NAs (those remain as NA in output)
  n_pre_baseline_total <- bs - 1
  n_pre_baseline_non_na <- max(0, n_pre_baseline_total - leading_NA)
  n_post_baseline <- length(y_full) - be

  # Generate predictions for non-NA pre-baseline period (if any) - no PI
  # Leading NA positions will be filled with NA in the output
  pre_baseline_predicted <- if (n_pre_baseline_non_na > 0) {
    first_baseline_idx <- df_baseline$year[1]
    pre_indices <- (first_baseline_idx - n_pre_baseline_non_na):(first_baseline_idx - 1)
    pre_df <- tibble(year = pre_indices, asmr = y_full[(leading_NA + 1):(bs - 1)]) |>
      as_tsibble(index = year)
    fc_pre <- mdl |> forecast(new_data = pre_df)
    as_tibble(fc_pre) |> pull(.mean)
  } else {
    numeric(0)
  }

  # Generate cumulative forecasts for post-baseline period (with PI)
  post_baseline_result <- if (n_post_baseline > 0) {
    post_result <- tibble()
    for (h_ in 1:n_post_baseline) {
      df_test <- new_data(df_baseline, n = h_) |>
        mutate(asmr = 0)
      post_result <- rbind(post_result, cumForecastN(df_baseline, df_test, mdl))
    }
    post_result
  } else {
    tibble(asmr = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate cumulative forecasts for h periods beyond observed data (if h > 0)
  fc_beyond_result <- if (h > 0) {
    beyond_result <- tibble()
    for (h_ in (n_post_baseline + 1):(n_post_baseline + h)) {
      df_test <- new_data(df_baseline, n = h_) |>
        mutate(asmr = 0)
      beyond_result <- rbind(beyond_result, cumForecastN(df_baseline, df_test, mdl))
    }
    beyond_result
  } else {
    tibble(asmr = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Calculate total result length (includes leading NAs)
  result_length <- leading_NA + n_pre_baseline_non_na + nrow(bl) + n_post_baseline + h

  # Calculate z-scores using helper function
  baseline_residuals <- df_baseline$asmr - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    bs = bs,
    be = be,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = result_length,
    h = h,
    df_baseline = df_baseline
  )

  # Combine all results with PI for post-baseline and beyond
  # Keep cumulative values (cumForecastN already returns cumulative sums)

  # Calculate cumulative baseline
  baseline_cumulative <- cumsum(bl$.mean)
  baseline_total <- sum(bl$.mean)

  # Pre-baseline (non-NA only): cumulative sum of predictions
  pre_cumulative <- if (n_pre_baseline_non_na > 0) cumsum(pre_baseline_predicted) else numeric(0)
  pre_total <- if (n_pre_baseline_non_na > 0) sum(pre_baseline_predicted) else 0

  # Post-baseline: cumForecastN returns cumulative sums from baseline end,
  # add only baseline total (NOT pre_total) for proper excess mortality calculation
  # Pre-baseline predictions should not inflate the cumulative offset
  cumulative_offset <- baseline_total
  post_y <- if (nrow(post_baseline_result) > 0) post_baseline_result$asmr + cumulative_offset else numeric(0)
  post_lower <- if (nrow(post_baseline_result) > 0) post_baseline_result$lower + cumulative_offset else numeric(0)
  post_upper <- if (nrow(post_baseline_result) > 0) post_baseline_result$upper + cumulative_offset else numeric(0)

  # Beyond: same treatment as post-baseline
  beyond_y <- if (nrow(fc_beyond_result) > 0) fc_beyond_result$asmr + cumulative_offset else numeric(0)
  beyond_lower <- if (nrow(fc_beyond_result) > 0) fc_beyond_result$lower + cumulative_offset else numeric(0)
  beyond_upper <- if (nrow(fc_beyond_result) > 0) fc_beyond_result$upper + cumulative_offset else numeric(0)

  # Pre-baseline cumulative is separate and does not chain into baseline/post-baseline
  # This ensures excess mortality calculations use only baseline-forward predictions

  # Build result vectors with leading NAs preserved
  result_y <- round(c(rep(NA, leading_NA), pre_cumulative, baseline_cumulative, post_y, beyond_y), 2)
  result_lower <- round(c(rep(NA, leading_NA + n_pre_baseline_non_na + nrow(bl)), post_lower, beyond_lower), 2)
  result_upper <- round(c(rep(NA, leading_NA + n_pre_baseline_non_na + nrow(bl)), post_upper, beyond_upper), 2)

  # Validate: y must be within [lower, upper] where bounds exist
  has_bounds <- !is.na(result_lower) & !is.na(result_upper)
  if (any(has_bounds)) {
    y_below_lower <- result_y[has_bounds] < result_lower[has_bounds]
    y_above_upper <- result_y[has_bounds] > result_upper[has_bounds]
    if (any(y_below_lower) || any(y_above_upper)) {
      stop("Internal error: predicted y values outside prediction interval bounds")
    }
  }

  list(
    y = result_y,
    lower = result_lower,
    upper = result_upper,
    zscore = zscores
  )
}

#' Handle ASD (Age-Standardized Deaths) request
#'
#' Calculates age-standardized deaths using the Levitt method.
#' Requires age-stratified data: deaths and population for each age group.
#' This properly accounts for changes in age structure over time.
#'
#' @param age_groups List of lists, each containing 'deaths' and 'population' vectors
#' @param h Integer horizon for forecasting beyond observed data
#' @param m Character method: "naive", "mean", "median", "lin_reg", or "exp"
#' @param t Boolean whether to include trend (only used with lin_reg)
#' @param bs Baseline start index (1-indexed, optional). If NULL, defaults to 1.
#' @param be Baseline end index (1-indexed, optional). If NULL, defaults to data length.
#'
#' @details
#' The ASD calculation for each age group:
#' - rate(t) = deaths(t) / population(t)
#' - Fit baseline model on rates
#' - expected_deaths(t) = predicted_rate(t) × population(t)
#'
#' Then sum across age groups:
#' - asd = sum of actual deaths across all age groups
#' - asd_bl = sum of expected deaths across all age groups
#'
#' This properly handles changes in age structure because each age group's
#' baseline rate is applied to that group's current population.
#'
#' @return List with:
#'   - asd: Total actual deaths (sum across age groups)
#'   - asd_bl: Total expected deaths (sum across age groups)
#'   - lower: Lower 95% prediction interval
#'   - upper: Upper 95% prediction interval
#'   - zscore: Z-scores for each period
handleASD <- function(age_groups, h, m, t, bs = NULL, be = NULL) {
  n_groups <- length(age_groups)
  if (n_groups == 0) {
    stop("age_groups must contain at least one age group")
  }

  # Get data length from first age group
  first_group <- age_groups[[1]]
  data_length <- length(first_group$deaths)

  # Set defaults for baseline start/end if not provided
  if (is.null(bs)) bs <- 1L
  if (is.null(be)) be <- data_length

  # Initialize accumulators for summing across age groups
  total_deaths <- rep(0, data_length + h)
  total_expected <- rep(0, data_length + h)
  total_lower <- rep(0, data_length + h)
  total_upper <- rep(0, data_length + h)

  # Track which positions have valid PI (post-baseline only)
  has_pi <- rep(FALSE, data_length + h)
  if (be < data_length || h > 0) {
    has_pi[(be + 1):(data_length + h)] <- TRUE
  }

  # Process each age group
  for (i in seq_len(n_groups)) {
    group <- age_groups[[i]]
    deaths <- group$deaths
    population <- group$population

    # Calculate mortality rates for this age group
    rates <- deaths / population

    # Count leading NAs (for output alignment)
    first_non_na_idx <- which(!is.na(rates))[1]
    leading_NA <- if (is.na(first_non_na_idx)) 0 else first_non_na_idx - 1

    # Adjust baseline start for leading NAs when bs=1
    actual_bs <- if (bs == 1 && leading_NA > 0) leading_NA + 1 else bs
    rates_baseline_clean <- if (bs == 1 && leading_NA > 0) {
      rates[(leading_NA + 1):be]
    } else {
      rates[bs:be]
    }

    # Create baseline tsibble (keep NAs to preserve regular time index)
    df_baseline <- tibble(year = seq.int(actual_bs, be), rate = rates_baseline_clean) |>
      as_tsibble(index = year)

    # Interpolate interspersed NAs for model fitting
    if (any(is.na(df_baseline$rate))) {
      idx <- seq_len(nrow(df_baseline))
      non_na <- !is.na(df_baseline$rate)
      if (sum(non_na) >= 2) {
        df_baseline$rate <- approx(idx[non_na], df_baseline$rate[non_na], idx, rule = 2)$y
      }
    }

    # Fit model based on method
    if (m == "naive") {
      mdl <- df_baseline |> model(NAIVE(rate))
    } else if (m == "mean") {
      mdl <- df_baseline |> model(TSLM(rate))
    } else if (m == "median") {
      mdl <- df_baseline |> model(MEDIAN(rate))
    } else if (m == "lin_reg") {
      if (t) {
        mdl <- df_baseline |> model(TSLM(rate ~ trend()))
      } else {
        mdl <- df_baseline |> model(TSLM(rate))
      }
    } else if (m == "exp") {
      mdl <- df_baseline |> model(ETS(rate ~ error("A") + trend("Ad")))
    } else {
      stop(paste("Unknown method for ASD:", m))
    }

    # Get fitted values for baseline period
    bl <- mdl |>
      augment() |>
      rename(.mean = .fitted)

    # Calculate pre-baseline and post-baseline lengths
    n_pre_baseline <- if (bs == 1 && leading_NA > 0) 0 else (bs - 1)
    n_post_baseline <- data_length - be

    # Generate predictions for pre-baseline period (if any)
    pre_baseline_rates <- if (n_pre_baseline > 0) {
      first_baseline_idx <- df_baseline$year[1]
      pre_indices <- (first_baseline_idx - n_pre_baseline):(first_baseline_idx - 1)
      pre_df <- tibble(year = pre_indices, rate = rates[1:n_pre_baseline]) |>
        as_tsibble(index = year)
      fc_pre <- mdl |> forecast(new_data = pre_df)
      as_tibble(fc_pre) |> pull(.mean)
    } else {
      numeric(0)
    }

    # Generate predictions for post-baseline period with PI
    post_baseline_result <- if (n_post_baseline > 0) {
      fc_post <- mdl |> forecast(h = n_post_baseline)
      fc_post_hilo <- fabletools::hilo(fc_post, 95) |>
        unpack_hilo(cols = `95%`) |>
        as_tibble() |>
        select(.mean, "95%_lower", "95%_upper") |>
        setNames(c("rate", "lower", "upper"))
      fc_post_hilo
    } else {
      tibble(rate = numeric(0), lower = numeric(0), upper = numeric(0))
    }

    # Generate forecast beyond observed data (if h > 0)
    fc_beyond_result <- if (h > 0) {
      fc_beyond <- mdl |> forecast(h = n_post_baseline + h)
      fc_beyond_hilo <- fabletools::hilo(fc_beyond, 95) |>
        unpack_hilo(cols = `95%`) |>
        as_tibble() |>
        select(.mean, "95%_lower", "95%_upper") |>
        setNames(c("rate", "lower", "upper")) |>
        tail(h)
      fc_beyond_hilo
    } else {
      tibble(rate = numeric(0), lower = numeric(0), upper = numeric(0))
    }

    # Combine all predicted rates for this age group
    all_predicted_rates <- c(
      rep(NA, leading_NA),
      pre_baseline_rates,
      bl$.mean,
      post_baseline_result$rate,
      fc_beyond_result$rate
    )

    # Build rate bounds
    rate_lower <- c(
      rep(NA, leading_NA + n_pre_baseline + nrow(bl)),
      post_baseline_result$lower,
      fc_beyond_result$lower
    )
    rate_upper <- c(
      rep(NA, leading_NA + n_pre_baseline + nrow(bl)),
      post_baseline_result$upper,
      fc_beyond_result$upper
    )

    # Extend population for forecast horizon
    population_extended <- if (h > 0) {
      c(population, rep(population[length(population)], h))
    } else {
      population
    }

    # Calculate expected deaths for this age group
    expected_deaths <- all_predicted_rates * population_extended
    expected_lower <- rate_lower * population_extended
    expected_upper <- rate_upper * population_extended

    # Extend actual deaths for output
    deaths_extended <- if (h > 0) {
      c(deaths, rep(NA, h))
    } else {
      deaths
    }

    # Accumulate totals (handle NAs properly)
    for (j in seq_len(data_length + h)) {
      if (!is.na(deaths_extended[j])) {
        total_deaths[j] <- total_deaths[j] + deaths_extended[j]
      }
      if (!is.na(expected_deaths[j])) {
        total_expected[j] <- total_expected[j] + expected_deaths[j]
      }
      if (!is.na(expected_lower[j])) {
        total_lower[j] <- total_lower[j] + expected_lower[j]
      }
      if (!is.na(expected_upper[j])) {
        total_upper[j] <- total_upper[j] + expected_upper[j]
      }
    }
  }

  # Set PI to NA where we don't have bounds (pre-baseline and baseline)
  total_lower[!has_pi] <- NA
  total_upper[!has_pi] <- NA

  # Set forecast deaths to NA
  if (h > 0) {
    total_deaths[(data_length + 1):(data_length + h)] <- NA
  }

  # Calculate z-scores on totals
  # Use baseline period residuals for SD calculation
  baseline_deaths <- total_deaths[bs:be]
  baseline_expected <- total_expected[bs:be]
  baseline_residuals <- baseline_deaths - baseline_expected
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)

  zscores <- (total_deaths - total_expected) / residual_sd

  list(
    asd = round(total_deaths, 2),
    asd_bl = round(total_expected, 2),
    lower = round(total_lower, 2),
    upper = round(total_upper, 2),
    zscore = round(zscores, 3)
  )
}
