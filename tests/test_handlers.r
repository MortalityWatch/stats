# Unit tests for handler functions
library(testthat)
library(tibble)
library(fable)
library(tidyverse)
library(tsibble)

# Source required files
source("../src/utils.r")
source("../src/handlers.r")

# Helper function to check result structure
check_forecast_result <- function(result, expected_length) {
  expect_true("y" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))
  expect_equal(length(result$y), expected_length)
  expect_equal(length(result$lower), expected_length)
  expect_equal(length(result$upper), expected_length)
  expect_equal(length(result$zscore), expected_length)
}

# ============================================================================
# handleForecast() tests
# ============================================================================

test_that("handleForecast with mean method works", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1 # Annual
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # Mean forecast should be constant
  forecast_values <- tail(result$y, h)
  expect_true(all(forecast_values == forecast_values[1]))
})

test_that("handleForecast with linear regression works", {
  y <- c(100, 105, 110, 115, 120, 125, 130)
  h <- 3
  m <- "lin_reg"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # With trend, forecast should generally increase
  forecast_values <- tail(result$y, h)
  expect_true(all(!is.na(forecast_values)))
})

test_that("handleForecast with naive method works", {
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "naive"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with exponential smoothing works", {
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 3
  m <- "exp"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with median method (non-seasonal) works", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "median"
  s <- 1 # Non-seasonal
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # Median forecast should be constant (the median value)
  forecast_values <- tail(result$y, h)
  expect_true(all(forecast_values == forecast_values[1]))
  expect_equal(forecast_values[1], median(y))
})

test_that("handleForecast with median method (seasonal) works", {
  # 12 months of data (s=3 means monthly)
  y <- c(100, 110, 105, 102, 112, 107, 104, 114, 109, 106, 116, 111)
  h <- 3
  m <- "median"
  s <- 3 # Monthly seasonality
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast handles leading NAs correctly", {
  y <- c(NA, NA, 100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # First two values should be NA
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$zscore[1]))
  expect_true(is.na(result$zscore[2]))
})

test_that("handleForecast with different seasonality types works", {
  y <- rep(100:107, 2) # 16 values
  h <- 2
  m <- "mean"
  t <- FALSE

  # Test quarterly (s=2)
  result_q <- handleForecast(y, h, m, s = 2, t)
  check_forecast_result(result_q, length(y) + h)

  # Test monthly (s=3)
  result_m <- handleForecast(y, h, m, s = 3, t)
  check_forecast_result(result_m, length(y) + h)

  # Test weekly (s=4)
  result_w <- handleForecast(y, h, m, s = 4, t)
  check_forecast_result(result_w, length(y) + h)
})

# ============================================================================
# Z-score calculation tests
# ============================================================================

test_that("Z-scores are calculated correctly for mean method", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Z-scores for observed data should be calculated
  observed_zscores <- result$zscore[1:length(y)]
  expect_true(all(!is.na(observed_zscores)))

  # Z-scores for forecast period should be NA (no observed data)
  forecast_zscores <- tail(result$zscore, h)
  expect_true(all(is.na(forecast_zscores)))
})

test_that("Z-scores have mean ~0 and sd ~1 for observed data", {
  y <- c(100, 105, 110, 108, 112, 115, 120, 118, 125, 122)
  h <- 2
  m <- "lin_reg"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  observed_zscores <- result$zscore[1:length(y)]

  # Z-scores should have mean close to 0 and sd close to 1
  expect_true(abs(mean(observed_zscores)) < 0.1)
  expect_true(abs(sd(observed_zscores) - 1) < 0.1)
})

test_that("Periodic series use STL-residual z-scores", {
  n <- 170
  i <- seq_len(n)
  seasonal <- 8 * sin(2 * pi * i / 52)
  trend <- 0.2 * i
  shock <- rep(0, n)
  shock[130] <- 30
  y <- 100 + trend + seasonal + shock

  result <- handleForecast(
    y = y,
    h = 2,
    m = "lin_reg",
    s = 4, # weekly
    t = TRUE,
    bs = 20,
    be = 130
  )

  observed_z <- result$zscore[1:n]
  baseline_z <- observed_z[20:130]

  # baseline-period z-scores should be roughly centered around zero
  expect_true(abs(mean(baseline_z, na.rm = TRUE)) < 0.4)

  # annual seasonality should be largely removed in z-scores
  season_ref <- sin(2 * pi * (20:130) / 52)
  seasonal_corr <- suppressWarnings(cor(baseline_z, season_ref, use = "complete.obs"))
  expect_true(abs(seasonal_corr) < 0.25)

  # shock should remain a clear anomaly
  expect_true(abs(observed_z[130]) > 2)
})

# ============================================================================
# Baseline parameters tests (bs/be for PR, b for legacy backwards compat)
# ============================================================================

test_that("bs/be parameters split data correctly", {
  # 10 years of data, use indices 1-5 for baseline (same as legacy b=5)
  y <- c(100, 105, 110, 108, 112, 115, 120, 125, 130, 135)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)
})

test_that("bs/be calculates z-scores for all data using baseline stats", {
  # Create data where post-baseline has higher values
  y_baseline <- c(100, 105, 110, 108, 112) # Mean ~107
  y_post <- c(200, 205, 210) # Much higher
  y <- c(y_baseline, y_post)

  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)

  # Post-baseline z-scores should be very high (positive)
  # since post-baseline values are much higher than baseline mean
  post_baseline_zscores <- result$zscore[6:8]
  expect_true(all(post_baseline_zscores > 2))
})

test_that("bs/be NULL uses all data (backwards compatible)", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result_with_null <- handleForecast(y, h, m, s, t, bs = NULL, be = NULL)
  result_without <- handleForecast(y, h, m, s, t)

  # Results should be identical
  expect_equal(result_with_null$y, result_without$y)
  expect_equal(result_with_null$zscore, result_without$zscore)
})

test_that("bs/be works with different methods", {
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 2
  s <- 1
  t <- TRUE

  # Test with linear regression
  result_lr <- handleForecast(y, h, "lin_reg", s, t, bs = 1, be = 5)
  check_forecast_result(result_lr, length(y) + h)

  # Test with exponential smoothing
  result_exp <- handleForecast(y, h, "exp", s, t, bs = 1, be = 5)
  check_forecast_result(result_exp, length(y) + h)

  # Test with median
  result_med <- handleForecast(y, h, "median", s, FALSE, bs = 1, be = 5)
  check_forecast_result(result_med, length(y) + h)
})

test_that("bs/be handles interspersed NAs in post-baseline period", {
  # Create data with baseline period and post-baseline with interspersed NAs
  y <- c(100, 105, 110, 108, 112, 115, NA, 120, NA, 125)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)

  # Z-scores should be NA at positions 7 and 9 (where data is NA)
  expect_true(is.na(result$zscore[7]))
  expect_true(is.na(result$zscore[9]))

  # Z-scores should be calculated for non-NA positions 6, 8, 10
  expect_true(!is.na(result$zscore[6]))  # Value 115
  expect_true(!is.na(result$zscore[8]))  # Value 120
  expect_true(!is.na(result$zscore[10])) # Value 125

  # Forecast z-scores should be NA (no observed data)
  expect_true(is.na(result$zscore[11]))
  expect_true(is.na(result$zscore[12]))
})

test_that("bs/be handles interspersed NAs within baseline period", {
  # 24 months of data (2 full cycles needed so seasonal model isn't perfectly determined)
  # NA at position 4 (inside baseline) tests interpolation preserves tsibble regularity
  y <- c(100, 110, 105, NA, 112, 107, 104, 114, 109, 106, 116, 111,
         102, 112, 107, 104, 114, 109, 106, 116, 111, 108, 118, 113)
  h <- 3
  s <- 3 # Monthly seasonality
  t <- FALSE

  # Should work with mean (TSLM + season()) — previously failed with filter(!is.na())
  result_mean <- handleForecast(y, h, "mean", s, t, bs = 1, be = 24)
  check_forecast_result(result_mean, length(y) + h)

  # Baseline z-score at NA position should be NA (no observed value to compare)
  expect_true(is.na(result_mean$zscore[4]))
  # Non-NA baseline positions should have z-scores
  expect_true(!is.na(result_mean$zscore[1]))
  expect_true(!is.na(result_mean$zscore[5]))

  # Should also work with other model types
  result_med <- handleForecast(y, h, "median", s, t, bs = 1, be = 24)
  check_forecast_result(result_med, length(y) + h)

  result_exp <- handleForecast(y, h, "exp", s, t, bs = 1, be = 24)
  check_forecast_result(result_exp, length(y) + h)

  # Non-seasonal (s=1) with interspersed NA in baseline should also work
  y2 <- c(100, 105, NA, 108, 112, 115, 120)
  result_ns <- handleForecast(y2, 2, "mean", 1, FALSE, bs = 1, be = 7)
  check_forecast_result(result_ns, length(y2) + 2)
  expect_true(is.na(result_ns$zscore[3]))
  expect_true(!is.na(result_ns$zscore[2]))
  expect_true(!is.na(result_ns$zscore[4]))
})

# ============================================================================
# Pre-baseline z-score tests (new bs/be feature)
# ============================================================================

test_that("bs > 1 calculates pre-baseline z-scores", {
  # 10 years of data, baseline is years 3-5
  y <- c(100, 105, 110, 108, 112, 200, 205, 210, 215, 220)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  check_forecast_result(result, length(y) + h)

  # Pre-baseline z-scores (indices 1-2) should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Baseline z-scores (indices 3-5) should be calculated
  expect_true(!is.na(result$zscore[3]))
  expect_true(!is.na(result$zscore[4]))
  expect_true(!is.na(result$zscore[5]))

  # Post-baseline z-scores (indices 6-10) should be calculated
  expect_true(!is.na(result$zscore[6]))
  expect_true(!is.na(result$zscore[10]))

  # Forecast z-scores should be NA
  expect_true(is.na(result$zscore[11]))
  expect_true(is.na(result$zscore[12]))
})

test_that("Pre-baseline z-scores use model prediction (not baseline mean)", {
  # Data where pre-baseline is similar to baseline (mean model)
  # With mean model, pre-baseline prediction = baseline mean
  y <- c(108, 112, 100, 105, 110, 200, 205, 210)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  # Baseline mean is (100+105+110)/3 = 105
  # Pre-baseline values are 108 and 112
  # Pre-baseline z-scores should be positive (above mean)
  expect_true(result$zscore[1] > 0)  # 108 > 105
  expect_true(result$zscore[2] > 0)  # 112 > 105
})

test_that("Full example from spec works correctly", {
  # From spec: 10 years of data, baseline 3-5, forecast 3
  y <- c(100, 105, 110, 115, 120, 150, 180, 160, 140, 130)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  # Result should have 13 values (10 observed + 3 forecast)
  expect_equal(length(result$y), 13)
  expect_equal(length(result$zscore), 13)

  # Z-scores for all observed data should be calculated
  expect_true(all(!is.na(result$zscore[1:10])))

  # Z-scores for forecast should be NA
  expect_true(all(is.na(result$zscore[11:13])))

  # Confidence intervals only for post-baseline and forecast periods
  # Pre-baseline (1-2) and baseline (3-5) should have NA for lower/upper
  expect_true(all(is.na(result$lower[1:5])))
  # Post-baseline (6-10) and forecast (11-13) should have PI
  expect_true(all(!is.na(result$lower[6:13])))
})

# ============================================================================
# handleCumulativeForecast() tests
# ============================================================================

test_that("handleCumulativeForecast works with trend", {
  y <- c(1000, 2100, 3300, 4600)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t)

  expect_true("y" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))

  # Should return cumulative values
  expect_true(all(!is.na(result$y[1:length(y)])))
})

test_that("handleCumulativeForecast works without trend", {
  y <- c(1000, 2000, 3000, 4000)
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t)

  expect_true("y" %in% names(result))
  expect_equal(length(result$zscore), length(y) + h)
})

test_that("handleCumulativeForecast returns cumulative baseline values", {
  # Issue #14: Baseline should accumulate, not be flat
  # With a constant mean baseline of 466.175, output should be:
  # Period 1: 466.175, Period 2: 932.35, Period 3: 1398.53, etc.
  y <- c(470.7, 471.5, 464.1, 458.4, 520.7, 571.2, 494.7, 456.3, 434.1)
  h <- 1
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Baseline values (first 4) should be cumulative (strictly increasing)
  baseline_y <- result$y[1:4]
  expect_true(all(diff(baseline_y) > 0), info = "Baseline values should be strictly increasing (cumulative)")

  # Each subsequent baseline value should be roughly the first + increments
  # (not flat/repeated values)
  expect_true(baseline_y[2] > baseline_y[1] * 1.5, info = "Second baseline should be > 1.5x first (cumulative)")
  expect_true(baseline_y[4] > baseline_y[1] * 3, info = "Fourth baseline should be > 3x first (cumulative)")
})

test_that("handleCumulativeForecast returns cumulative post-baseline values", {
  # Post-baseline values should continue cumulating from baseline total
  y <- c(100, 100, 100, 100, 100, 100)  # Constant values
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # With constant baseline of 100:
  # Baseline cumulative: 100, 200, 300, 400
  # Post-baseline (periods 5,6): should be ~500, ~600
  # Forecast: should be ~700, ~800
  expect_equal(result$y[1], 100, tolerance = 1)
  expect_equal(result$y[2], 200, tolerance = 1)
  expect_equal(result$y[3], 300, tolerance = 1)
  expect_equal(result$y[4], 400, tolerance = 1)
  expect_equal(result$y[5], 500, tolerance = 1)  # First post-baseline
  expect_equal(result$y[6], 600, tolerance = 1)  # Second post-baseline
  expect_equal(result$y[7], 700, tolerance = 1)  # First forecast
  expect_equal(result$y[8], 800, tolerance = 1)  # Second forecast
})

test_that("handleCumulativeForecast prediction intervals widen over time", {
  # Issue #14: Prediction intervals should widen as uncertainty accumulates
  y <- c(100, 100, 100, 100, 120, 110, 130, 140)
  h <- 3
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Post-baseline lower/upper bounds should exist
  post_lower <- result$lower[5:length(result$lower)]
  post_upper <- result$upper[5:length(result$upper)]

  expect_true(all(!is.na(post_lower)), info = "Post-baseline lower bounds should not be NA")
  expect_true(all(!is.na(post_upper)), info = "Post-baseline upper bounds should not be NA")

  # Prediction interval width should increase over time
  interval_widths <- post_upper - post_lower
  expect_true(all(diff(interval_widths) >= 0), info = "PI widths should increase or stay same over time")
})

test_that("handleCumulativeForecast y values are within prediction interval bounds", {
  # Ensure y is always within [lower, upper] where bounds exist
  y <- c(470.7, 471.5, 464.1, 458.4, 520.7, 571.2, 494.7, 456.3, 434.1)
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Check that y is within bounds where they exist
  has_bounds <- !is.na(result$lower) & !is.na(result$upper)
  y_with_bounds <- result$y[has_bounds]
  lower_bounds <- result$lower[has_bounds]
  upper_bounds <- result$upper[has_bounds]

  expect_true(all(y_with_bounds >= lower_bounds),
    info = "y values must be >= lower bound")
  expect_true(all(y_with_bounds <= upper_bounds),
    info = "y values must be <= upper bound")
})

test_that("handleCumulativeForecast with bs/be", {
  y <- c(1000, 2100, 3300, 4600, 6000, 7500)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  expect_equal(length(result$zscore), length(y) + h)

  # Post-baseline z-scores should be calculated
  post_baseline_zscores <- result$zscore[5:6]
  expect_true(all(!is.na(post_baseline_zscores)))

  # Forecast z-scores should be NA
  expect_true(all(is.na(result$zscore[7:8])))
})

test_that("handleCumulativeForecast with pre-baseline (bs > 1)", {
  # 8 years of data, baseline is years 3-5 (need at least 3 points)
  y <- c(1000, 2100, 3300, 4600, 6000, 7500, 9100, 10800)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t, bs = 3, be = 5)

  expect_equal(length(result$zscore), length(y) + h)

  # Pre-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[3]))
  expect_true(!is.na(result$zscore[4]))
  expect_true(!is.na(result$zscore[5]))

  # Post-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[6]))
  expect_true(!is.na(result$zscore[7]))
  expect_true(!is.na(result$zscore[8]))

  # Forecast z-scores should be NA
  expect_true(all(is.na(result$zscore[9:10])))
})

test_that("handleCumulativeForecast preserves leading NAs in output", {
  # Issue: /cum endpoint was stripping leading nulls causing index mismatch
  # Input: 10 leading NAs, then 9 actual values
  # bs=11, be=14 refer to positions in the FULL 19-element array
  y <- c(rep(NA, 10), 402.3, 413.9, 390.5, 400.7, 394.7, 446.1, 459.2, 413.7, 404.3)
  h <- 1  # h must be >= 1
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 11, be = 14)

  # Output should have length = input + h (19 + 1 = 20 elements)
  expect_equal(length(result$y), length(y) + h)
  expect_equal(length(result$lower), length(y) + h)
  expect_equal(length(result$upper), length(y) + h)
  expect_equal(length(result$zscore), length(y) + h)

  # Leading positions (1-10) should be NA in y
  expect_true(all(is.na(result$y[1:10])), info = "Leading NA positions should remain NA in output")

  # Baseline positions (11-14) should have cumulative values
  expect_true(all(!is.na(result$y[11:14])), info = "Baseline positions should have values")
  expect_true(result$y[11] > 0)
  expect_true(result$y[14] > result$y[11])  # Cumulative should increase

  # Post-baseline positions (15-19) should have values
  expect_true(all(!is.na(result$y[15:19])), info = "Post-baseline positions should have values")

  # Z-scores for leading NA positions should be NA
  expect_true(all(is.na(result$zscore[1:10])), info = "Z-scores for leading NAs should be NA")

  # Z-scores for baseline and post-baseline should be calculated
  expect_true(all(!is.na(result$zscore[11:19])), info = "Z-scores should be calculated for non-NA data")

  # Forecast z-score should be NA
  expect_true(is.na(result$zscore[20]), info = "Forecast z-score should be NA")
})

test_that("pre-baseline predictions don't inflate cumulative offset", {
  # Regression test for issue #310: pre-baseline was incorrectly added to

  # cumulative offset, causing excess mortality to show ~-55% instead of
  # correct values
  y <- c(100, 100, 100, 100, 100, 100)  # Constant values
  h <- 0
  t <- FALSE

  # With bs=3, pre-baseline is years 1-2, baseline is 3-4
  result <- handleCumulativeForecast(y, h, t, bs = 3, be = 4)

  # Post-baseline (periods 5,6) should continue from baseline_total only
  # Baseline total for 2 periods of ~100 = ~200
  # So period 5 should be ~300, period 6 should be ~400
  expect_equal(result$y[5], 300, tolerance = 10)
  expect_equal(result$y[6], 400, tolerance = 10)

  # NOT ~500 and ~600 (which would happen if pre_total was included)
})

# ============================================================================
# Edge cases and error handling
# ============================================================================

test_that("handleForecast rejects unknown method", {
  y <- c(100, 105, 110, 115)
  h <- 2
  m <- "unknown_method"
  s <- 1
  t <- FALSE

  expect_error(
    handleForecast(y, h, m, s, t),
    "Unknown method"
  )
})

test_that("Z-score precision is 3 decimals", {
  y <- c(100.1234, 105.5678, 110.9012, 108.3456, 112.7890)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Check that z-scores are rounded to 3 decimals
  observed_zscores <- result$zscore[1:length(y)]
  for (z in observed_zscores) {
    if (!is.na(z)) {
      # Count decimal places
      z_str <- as.character(z)
      if (grepl("\\.", z_str)) {
        decimals <- nchar(strsplit(z_str, "\\.")[[1]][2])
        expect_true(decimals <= 3)
      }
    }
  }
})

test_that("Forecast values are rounded to 2 decimals", {
  y <- c(100.123, 105.456, 110.789)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Check forecast values are rounded to 2 decimals
  for (val in result$y) {
    if (!is.na(val)) {
      val_str <- as.character(val)
      if (grepl("\\.", val_str)) {
        decimals <- nchar(strsplit(val_str, "\\.")[[1]][2])
        expect_true(decimals <= 2)
      }
    }
  }
})

# ============================================================================
# xs (start time index) parameter tests
# ============================================================================

test_that("parse_xs correctly parses weekly format", {
  # Test various weekly formats
  result <- parse_xs("2020W10", 4)
  expect_true(inherits(result, "yearweek"))
  expect_equal(format(result), "2020 W10")

  # With hyphen
  result2 <- parse_xs("2020-W10", 4)
  expect_equal(format(result2), "2020 W10")

  # Week 1
  result3 <- parse_xs("2020W01", 4)
  expect_equal(format(result3), "2020 W01")

  # Week 53
  result4 <- parse_xs("2020W53", 4)
  expect_equal(format(result4), "2020 W53")
})

test_that("parse_xs correctly parses monthly format", {
  result <- parse_xs("2020-01", 3)
  expect_true(inherits(result, "yearmonth"))
  expect_equal(format(result), "2020 Jan")

  # Without hyphen
  result2 <- parse_xs("202012", 3)
  expect_equal(format(result2), "2020 Dec")
})

test_that("parse_xs correctly parses quarterly format", {
  result <- parse_xs("2020Q1", 2)
  expect_true(inherits(result, "yearquarter"))
  expect_equal(format(result), "2020 Q1")

  # With hyphen
  result2 <- parse_xs("2020-Q4", 2)
  expect_equal(format(result2), "2020 Q4")
})

test_that("parse_xs correctly parses yearly format", {
  result <- parse_xs("2020", 1)
  expect_equal(result, 2020L)
})

test_that("parse_xs returns NULL for NULL input", {
  expect_null(parse_xs(NULL, 1))
  expect_null(parse_xs(NULL, 4))
})

test_that("handleForecast with xs uses correct weekly time indices", {
  # Create 8 weeks of data starting from week 50 of 2020
  # This crosses year boundary and includes potential week 53
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 4
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020W50")

  check_forecast_result(result, length(y) + h)

  # Should have forecasts
  expect_true(all(!is.na(result$y)))
})

test_that("handleForecast with xs uses correct monthly time indices", {
  # 12 months of data starting from October 2020
  y <- c(100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155)
  h <- 3
  m <- "mean"
  s <- 3  # Monthly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020-10")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs uses correct quarterly time indices", {
  # 8 quarters of data starting from Q3 2020
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 2
  m <- "mean"
  s <- 2  # Quarterly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020Q3")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs uses correct yearly time indices", {
  # 5 years of data starting from 2018
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 1  # Yearly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2018")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast without xs falls back to synthetic indices", {
  # This should work the same as before (backwards compatible)
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs and bs/be works correctly", {
  # 10 weeks of data, baseline is weeks 3-6, starting from week 10
  y <- c(100, 105, 110, 108, 112, 115, 200, 205, 210, 215)
  h <- 2
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 6, xs = "2020W10")

  check_forecast_result(result, length(y) + h)

  # Pre-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Post-baseline z-scores (higher values) should be positive
  expect_true(result$zscore[7] > 0)
  expect_true(result$zscore[10] > 0)
})

test_that("handleForecast with xs handles week 53 correctly", {
  # Data spanning a year with week 53 (2020 had 53 weeks)
  # Start at week 51 of 2020, which should go: W51, W52, W53, then 2021 W01, W02, etc.
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)  # 8 weeks
  h <- 4
  m <- "mean"
  s <- 4
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020W51")

  check_forecast_result(result, length(y) + h)

  # Model should handle week 53 without error
  expect_true(all(!is.na(result$y)))
})

test_that("handleForecast seasonal patterns align with actual calendar weeks", {
  # Create synthetic weekly data with clear seasonal pattern
  # Weekly pattern: higher values at end of each "year" (week 52)
  # 104 weeks = 2 years of weekly data starting from week 1
  set.seed(42)
  weekly_pattern <- rep(c(rep(100, 51), 150), 2)  # Week 52 is higher
  y <- weekly_pattern + rnorm(104, 0, 5)
  h <- 52
  m <- "mean"
  s <- 4
  t <- FALSE

  # With xs starting at week 1, the seasonal pattern should be learned correctly
  result <- handleForecast(y, h, m, s, t, xs = "2020W01")

  check_forecast_result(result, length(y) + h)

  # The forecast for week 52 positions should be higher than other weeks
  # Forecast starts at position 105, so week 52 of year 3 would be at position 156
  forecast_values <- tail(result$y, h)
  # Week 52 values (positions 52 in forecast) should be elevated
  expect_true(forecast_values[52] > mean(forecast_values[1:51]))
})

# ============================================================================
# handleASD() tests - Age-Standardized Deaths
# ============================================================================

# Helper function to check ASD result structure
check_asd_result <- function(result, expected_length) {
  expect_true("asd" %in% names(result))
  expect_true("asd_bl" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))
  expect_equal(length(result$asd), expected_length)
  expect_equal(length(result$asd_bl), expected_length)
  expect_equal(length(result$lower), expected_length)
  expect_equal(length(result$upper), expected_length)
  expect_equal(length(result$zscore), expected_length)
}

# Helper to create age_groups structure
make_age_groups <- function(...) {
  groups <- list(...)
  lapply(groups, function(g) list(deaths = g$deaths, population = g$population))
}

test_that("handleASD with single age group works", {
  age_groups <- make_age_groups(
    list(deaths = c(1000, 1050, 1100, 1080, 1120), population = c(100000, 100000, 100000, 100000, 100000))
  )
  h <- 0
  m <- "mean"
  t <- FALSE

  result <- handleASD(age_groups, h, m, t)

  check_asd_result(result, 5)
  expect_true(all(!is.na(result$asd_bl)))
})

test_that("handleASD with multiple age groups sums correctly", {
  # Two age groups with different rates
  # Group 1: rate = 0.01 (1%)
  # Group 2: rate = 0.02 (2%)
  age_groups <- make_age_groups(
    list(deaths = c(100, 100, 100, 100, 100), population = c(10000, 10000, 10000, 10000, 10000)),
    list(deaths = c(200, 200, 200, 200, 200), population = c(10000, 10000, 10000, 10000, 10000))
  )
  h <- 0
  m <- "mean"
  t <- FALSE

  result <- handleASD(age_groups, h, m, t)

  check_asd_result(result, 5)

  # Total deaths should be 300 per period
  expect_equal(result$asd[1], 300)

  # Total expected should also be ~300 (rates applied to populations, then summed)
  expect_equal(result$asd_bl[1], 300, tolerance = 1)
})

test_that("handleASD detects age structure change", {
  # KEY TEST: Age structure changes but rates stay constant
  # Baseline: Group A (young) has 50% of pop, Group B (old) has 50%
  # Post-baseline: Group B grows to 75% of pop
  #
  # Group A rate = 0.005 (0.5%)
  # Group B rate = 0.02 (2%)

  age_groups <- make_age_groups(
    # Group A (young): rate stays at 0.5%, but population shrinks
    list(
      deaths = c(50, 50, 50, 25, 25),      # Baseline: 50, post: 25 (matches new pop)
      population = c(10000, 10000, 10000, 5000, 5000)  # Shrinks to 5000
    ),
    # Group B (old): rate stays at 2%, but population grows
    list(
      deaths = c(200, 200, 200, 300, 300),  # Baseline: 200, post: 300 (matches new pop)
      population = c(10000, 10000, 10000, 15000, 15000)  # Grows to 15000
    )
  )
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(age_groups, h, m, t, bs, be)

  # Total deaths: baseline = 250, post = 325
  expect_equal(result$asd[1], 250)
  expect_equal(result$asd[4], 325)

  # Expected deaths post-baseline:
  # Group A: 0.005 * 5000 = 25
  # Group B: 0.02 * 15000 = 300
  # Total expected = 325
  expect_equal(result$asd_bl[4], 325, tolerance = 1)

  # Z-scores should be close to 0 (rates didn't change, just age structure)
  expect_true(abs(result$zscore[4]) < 0.5)
})

test_that("handleASD detects true excess with multiple age groups", {
  # Baseline: normal rates
  # Post-baseline: rates increase in both groups (true excess)

  age_groups <- make_age_groups(
    # Group A: rate increases from 1% to 1.5%
    list(
      deaths = c(100, 100, 100, 150, 150),
      population = c(10000, 10000, 10000, 10000, 10000)
    ),
    # Group B: rate increases from 2% to 3%
    list(
      deaths = c(200, 200, 200, 300, 300),
      population = c(10000, 10000, 10000, 10000, 10000)
    )
  )
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(age_groups, h, m, t, bs, be)

  # Total deaths: baseline = 300, post = 450
  expect_equal(result$asd[4], 450)

  # Expected deaths post-baseline (using baseline rates):
  # Group A: 0.01 * 10000 = 100
  # Group B: 0.02 * 10000 = 200
  # Total expected = 300
  expect_equal(result$asd_bl[4], 300, tolerance = 1)

  # Z-scores should be strongly positive (true excess)
  expect_true(result$zscore[4] > 2)
})

test_that("handleASD with lin_reg and multiple age groups works", {
  age_groups <- make_age_groups(
    list(deaths = c(100, 110, 120, 130, 140), population = c(10000, 10000, 10000, 10000, 10000)),
    list(deaths = c(200, 210, 220, 230, 240), population = c(10000, 10000, 10000, 10000, 10000))
  )
  h <- 0
  m <- "lin_reg"
  t <- TRUE
  bs <- 1
  be <- 3

  result <- handleASD(age_groups, h, m, t, bs, be)

  check_asd_result(result, 5)

  # With trend, expected should follow the linear pattern
  expect_true(result$asd_bl[4] > result$asd_bl[3])
  expect_true(result$asd_bl[5] > result$asd_bl[4])
})

test_that("handleASD prediction intervals sum across age groups", {
  # Use baseline data with variance to generate proper PIs
  age_groups <- make_age_groups(
    list(deaths = c(95, 100, 105, 150, 150), population = c(10000, 10000, 10000, 10000, 10000)),
    list(deaths = c(190, 200, 210, 250, 250), population = c(10000, 10000, 10000, 10000, 10000))
  )
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(age_groups, h, m, t, bs, be)

  # PI should be NA for baseline period
  expect_true(all(is.na(result$lower[1:3])))
  expect_true(all(is.na(result$upper[1:3])))

  # PI should exist for post-baseline
  expect_true(all(!is.na(result$lower[4:5])))
  expect_true(all(!is.na(result$upper[4:5])))

  # Lower should be less than upper (now we have variance so PI has width)
  expect_true(result$lower[4] < result$upper[4])
})

test_that("handleASD with forecast horizon works", {
  age_groups <- make_age_groups(
    list(deaths = c(100, 100, 100), population = c(10000, 10000, 10000))
  )
  h <- 2
  m <- "mean"
  t <- FALSE

  result <- handleASD(age_groups, h, m, t)

  check_asd_result(result, 5)  # 3 data + 2 forecast

  # Forecast deaths should be NA
  expect_true(is.na(result$asd[4]))
  expect_true(is.na(result$asd[5]))

  # Forecast expected should exist
  expect_true(!is.na(result$asd_bl[4]))
  expect_true(!is.na(result$asd_bl[5]))
})

test_that("handleASD errors on empty age_groups", {
  expect_error(
    handleASD(list(), 0, "mean", FALSE),
    "at least one age group"
  )
})

message("\nHandler tests completed!")
