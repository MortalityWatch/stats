# Unit tests for request validation and server functions
library(testthat)

# Source the serve file to get validation functions
# Note: This will load but not start the server
source("../src/serve.r", local = TRUE)

test_that("validate_request accepts valid standard forecast request", {
  query <- list(
    y = "10,20,30,40,50",
    h = "5",
    s = "1",
    m = "mean",
    t = "0",
    zscore_method = "standard"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts variance-stabilized z-score params", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    zscore_method = "variance_stabilized",
    lambda_mode = "manual",
    lambda = "0.25"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request rejects invalid z-score method", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    zscore_method = "weird"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_match(result$message, "zscore_method")
})

test_that("validate_request rejects manual lambda without variance-stabilized z-scores", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    zscore_method = "standard",
    lambda_mode = "manual",
    lambda = "0"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_match(result$message, "variance_stabilized")
})

test_that("validate_request rejects missing manual lambda value", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    zscore_method = "variance_stabilized",
    lambda_mode = "manual"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_match(result$message, "lambda")
})

test_that("validate_request accepts valid cumulative forecast request", {
  query <- list(
    y = "100,200,300,400",
    h = "3",
    t = "1"
  )

  result <- validate_request(query, "/cum")

  expect_true(result$valid)
})

test_that("validate_request rejects missing y parameter", {
  query <- list(h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "Missing required parameter 'y'")
})

test_that("validate_request rejects non-numeric y parameter", {
  query <- list(y = "abc,def,ghi", h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "at least 3 valid data points")
})

test_that("validate_request rejects insufficient data points", {
  query <- list(y = "10,20", h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "at least 3 valid data points")
})

test_that("validate_request rejects oversized array", {
  large_array <- paste(rep("10", 10001), collapse = ",")
  query <- list(y = large_array, h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "exceeds maximum length")
})

test_that("validate_request rejects invalid horizon", {
  query <- list(y = "10,20,30,40", h = "0", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "positive integer")
})

test_that("validate_request rejects horizon over 1000", {
  query <- list(y = "10,20,30,40", h = "1001", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
})

test_that("validate_request rejects invalid seasonality", {
  query <- list(y = "10,20,30,40", h = "5", s = "5", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "must be 1 \\(year\\), 2 \\(quarter\\), 3 \\(month\\), or 4 \\(week\\)")
})

test_that("validate_request rejects invalid method", {
  query <- list(y = "10,20,30,40", h = "5", s = "1", m = "invalid")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "naive, mean, median, lin_reg, exp")
})

# ============================================================================
# bs/be parameter validation tests
# ============================================================================

test_that("validate_request accepts valid bs/be parameters", {
  query <- list(
    y = "10,20,30,40,50,60,70,80",
    h = "2",
    s = "1",
    m = "mean",
    bs = "3",
    be = "5"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request rejects bs without be", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    bs = "1"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "must both be provided together")
})

test_that("validate_request rejects be without bs", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    be = "3"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "must both be provided together")
})

test_that("validate_request rejects bs < 1", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    bs = "0",
    be = "3"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "bs.*>= 1")
})

test_that("validate_request rejects be <= bs", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    bs = "3",
    be = "3"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "be.*> 'bs'")
})

test_that("validate_request rejects be > length(y)", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    bs = "1",
    be = "10"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "be.*<= data length")
})

test_that("validate_request rejects bs/be with insufficient non-NA values", {
  query <- list(
    y = "10,NA,NA,40,50",
    h = "2",
    s = "1",
    m = "mean",
    bs = "1",
    be = "3"
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "At least 3 non-NA values")
})

test_that("generate_cache_key creates consistent keys", {
  query1 <- list(y = "10,20,30", h = "5", s = "1", m = "mean")
  query2 <- list(m = "mean", s = "1", h = "5", y = "10,20,30") # Different order

  key1 <- generate_cache_key("/", query1)
  key2 <- generate_cache_key("/", query2)

  expect_equal(key1, key2)
})

test_that("generate_cache_key creates different keys for different params", {
  query1 <- list(y = "10,20,30", h = "5", s = "1", m = "mean")
  query2 <- list(y = "10,20,30", h = "10", s = "1", m = "mean")

  key1 <- generate_cache_key("/", query1)
  key2 <- generate_cache_key("/", query2)

  expect_false(key1 == key2)
})

test_that("cache stores and retrieves responses", {
  # Clear cache
  rm(list = ls(envir = cache_store), envir = cache_store)

  cache_key <- "test_key"
  response <- list(y = c(1, 2, 3), lower = c(0, 1, 2), upper = c(2, 3, 4))

  # Store
  set_cached_response(cache_key, response)

  # Retrieve
  cached <- get_cached_response(cache_key)

  expect_equal(cached, response)
})

test_that("cache returns NULL for non-existent keys", {
  cached <- get_cached_response("nonexistent_key_12345")
  expect_null(cached)
})

test_that("rate limiting allows requests under limit", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.100"

  # First request should be allowed
  result <- check_rate_limit(ip)
  expect_true(result)

  # Second request should be allowed
  result <- check_rate_limit(ip)
  expect_true(result)
})

test_that("rate limiting blocks requests over limit", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.101"

  # Manually set to max
  rate_limit_store[[ip]] <- list(count = RATE_LIMIT_MAX_REQUESTS, window_start = as.numeric(Sys.time()))

  # Next request should be blocked
  result <- check_rate_limit(ip)
  expect_false(result)
})

test_that("rate limiting resets after window expires", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.102"

  # Set to max but with expired window
  rate_limit_store[[ip]] <- list(
    count = RATE_LIMIT_MAX_REQUESTS,
    window_start = as.numeric(Sys.time()) - RATE_LIMIT_WINDOW - 1
  )

  # Should be allowed (window reset)
  result <- check_rate_limit(ip)
  expect_true(result)

  # Verify count was reset
  expect_equal(rate_limit_store[[ip]]$count, 1)
})

# ============================================================================
# xs (start time index) parameter validation tests
# ============================================================================

test_that("validate_request accepts valid xs for weekly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "4",
    m = "mean",
    xs = "2020W10"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts xs with hyphen for weekly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "4",
    m = "mean",
    xs = "2020-W10"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts valid xs for monthly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "3",
    m = "mean",
    xs = "2020-01"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts xs without hyphen for monthly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "3",
    m = "mean",
    xs = "202001"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts valid xs for quarterly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "2",
    m = "mean",
    xs = "2020Q1"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts xs with hyphen for quarterly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "2",
    m = "mean",
    xs = "2020-Q3"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts valid xs for yearly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    xs = "2020"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request rejects invalid xs format for weekly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "4",
    m = "mean",
    xs = "2020-01"  # Monthly format for weekly data
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "YYYYWNN")
})

test_that("validate_request rejects invalid week number", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "4",
    m = "mean",
    xs = "2020W54"  # Week 54 doesn't exist
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "1-53")
})

test_that("validate_request rejects invalid xs format for monthly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "3",
    m = "mean",
    xs = "2020W10"  # Weekly format for monthly data
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "YYYY-MM")
})

test_that("validate_request rejects invalid month number", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "3",
    m = "mean",
    xs = "2020-13"  # Month 13 doesn't exist
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "01-12")
})

test_that("validate_request rejects invalid xs format for quarterly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "2",
    m = "mean",
    xs = "2020Q5"  # Quarter 5 doesn't exist
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "YYYYQN")
})

test_that("validate_request rejects invalid xs format for yearly data", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "1",
    m = "mean",
    xs = "20"  # Too short
  )

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "YYYY")
})

test_that("validate_request accepts week 53 in xs", {
  query <- list(
    y = "10,20,30,40,50",
    h = "2",
    s = "4",
    m = "mean",
    xs = "2020W53"  # 2020 had 53 weeks
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

# ============================================================================
# POST body with NULL values tests (preserving array positions)
# ============================================================================

test_that("validate_request accepts y as list with NULL values (preserves length)", {
  # When y is sent as JSON array with nulls, it becomes a list in R
  # NULL values should become NA, preserving array positions
  query <- list(
    y = list(NULL, NULL, NULL, 10, 20, 30, 40, 50),  # 3 nulls + 5 values = 8 elements
    h = "2",
    bs = "4",
    be = "6"
  )

  result <- validate_request(query, "/cum")

  expect_true(result$valid)
})

test_that("validate_request preserves length when y has leading NULLs", {
  # Issue: unlist() was stripping NULLs, causing index mismatch with bs/be
  # Example from bug report: 10 leading nulls, then 9 values = 19 total
  query <- list(
    y = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
             402.3, 413.9, 390.5, 400.7, 394.7, 446.1, 459.2, 413.7, 404.3),
    h = "1",
    bs = "11",
    be = "14",
    t = "0"
  )

  result <- validate_request(query, "/cum")

  # Should pass validation - be (14) <= length (19)
  expect_true(result$valid, info = "be=14 should be valid for 19-element array with leading NULLs")
})

test_that("validate_request rejects be > length even with NULL-preserved length", {
  query <- list(
    y = list(NULL, NULL, 10, 20, 30),  # 5 elements total
    h = "2",
    bs = "3",
    be = "6"  # > 5, should fail
  )

  result <- validate_request(query, "/cum")

  expect_false(result$valid)
  expect_match(result$message, "be.*<= data length")
})

# ============================================================================
# ASD endpoint validation tests (multi-age-group structure)
# ============================================================================

# Helper to create age groups for ASD tests
make_test_age_groups <- function(...) {
  groups <- list(...)
  lapply(groups, function(g) list(deaths = g$deaths, population = g$population))
}

test_that("validate_request accepts valid ASD request with single age group", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    h = "2",
    m = "mean"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request accepts valid ASD request with multiple age groups", {
  query <- list(
    age_groups = list(
      list(deaths = c(100, 110, 120, 130, 140), population = c(10000, 10000, 10000, 10000, 10000)),
      list(deaths = c(500, 520, 540, 560, 580), population = c(50000, 50000, 50000, 50000, 50000)),
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(40000, 40000, 40000, 40000, 40000))
    ),
    h = "2",
    m = "mean"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request accepts ASD request with lin_reg method", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "lin_reg",
    h = "2",
    t = "1"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request rejects ASD request missing age_groups", {
  query <- list(
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "age_groups")
})

test_that("validate_request rejects ASD request with empty age_groups", {
  query <- list(
    age_groups = list(),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "non-empty")
})

test_that("validate_request rejects ASD group missing deaths", {
  query <- list(
    age_groups = list(
      list(population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "deaths")
})

test_that("validate_request rejects ASD group missing population", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "population")
})

test_that("validate_request rejects ASD group with mismatched array lengths", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "same length")
})

test_that("validate_request rejects ASD groups with different lengths", {
  query <- list(
    age_groups = list(
      list(deaths = c(100, 110, 120, 130, 140), population = c(10000, 10000, 10000, 10000, 10000)),
      list(deaths = c(500, 520, 540), population = c(50000, 50000, 50000))  # Different length
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "same.*length", ignore.case = TRUE)
})

test_that("validate_request accepts ASD request with median method", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "median",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request accepts ASD request with naive method", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "naive",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request accepts ASD request with exp method", {
  query <- list(
    age_groups = list(
      list(
        deaths = c(1000, 1050, 1100, 1150, 1200, 1180, 1220, 1250),
        population = c(100000, 100000, 100000, 100000, 100000, 100000, 100000, 100000)
      )
    ),
    m = "exp",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request rejects ASD request with invalid method", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "invalid_method",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "naive, mean, median, lin_reg, exp")
})

test_that("validate_request rejects ASD group with zero population", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 0, 100000, 100000, 100000))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "positive")
})

test_that("validate_request rejects ASD group with negative population", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, -100000, 100000, 100000, 100000))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "positive")
})

test_that("validate_request accepts ASD request with baseline params", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "mean",
    h = "2",
    bs = "1",
    be = "3"
  )

  result <- validate_request(query, "/asd")

  expect_true(result$valid)
})

test_that("validate_request rejects ASD request with invalid baseline params", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050, 1100, 1150, 1200), population = c(100000, 100000, 100000, 100000, 100000))
    ),
    m = "mean",
    h = "2",
    bs = "1",
    be = "10"  # > length
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "be.*<= data length")
})

test_that("validate_request rejects ASD request with insufficient data points", {
  query <- list(
    age_groups = list(
      list(deaths = c(1000, 1050), population = c(100000, 100000))
    ),
    m = "mean",
    h = "2"
  )

  result <- validate_request(query, "/asd")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "at least 3")
})
