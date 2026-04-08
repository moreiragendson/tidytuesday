library(testthat)
library(dplyr)
library(rlang)
source("rescale_number.R")

make_df <- function(x) tibble::tibble(value = x)

# --- Scale thresholds --------------------------------------------------------

test_that("values >= 1e12 are scaled to T", {
  result <- rescale_number(make_df(3e12), value)
  expect_equal(result$value, 3)
  expect_equal(result$value_suffix, "T")
})

test_that("values >= 1e9 and < 1e12 are scaled to B", {
  result <- rescale_number(make_df(2.5e9), value)
  expect_equal(result$value, 2.5)
  expect_equal(result$value_suffix, "B")
})

test_that("values >= 1e6 and < 1e9 are scaled to M", {
  result <- rescale_number(make_df(1.2e6), value)
  expect_equal(result$value, 1.2)
  expect_equal(result$value_suffix, "M")
})

test_that("values >= 1e3 and < 1e6 are scaled to K", {
  result <- rescale_number(make_df(500e3), value)
  expect_equal(result$value, 500)
  expect_equal(result$value_suffix, "K")
})

test_that("values < 1e3 are left unchanged with empty suffix", {
  result <- rescale_number(make_df(42), value)
  expect_equal(result$value, 42)
  expect_equal(result$value_suffix, "")
})

# --- Exact boundaries --------------------------------------------------------

test_that("exactly 1e12 uses T", {
  expect_equal(rescale_number(make_df(1e12), value)$value_suffix, "T")
})

test_that("exactly 1e9 uses B", {
  expect_equal(rescale_number(make_df(1e9), value)$value_suffix, "B")
})

test_that("exactly 1e6 uses M", {
  expect_equal(rescale_number(make_df(1e6), value)$value_suffix, "M")
})

test_that("exactly 1e3 uses K", {
  expect_equal(rescale_number(make_df(1e3), value)$value_suffix, "K")
})

# --- Output column names -----------------------------------------------------

test_that("without rename, output columns use the source column name", {
  result <- rescale_number(make_df(5e6), value)
  expect_true("value"        %in% names(result))
  expect_true("value_suffix" %in% names(result))
})

test_that("rename syntax creates new named columns and preserves original", {
  result <- rescale_number(make_df(5e6), rescaled = value)
  expect_true("rescaled"        %in% names(result))
  expect_true("rescaled_suffix" %in% names(result))
  expect_true("value"           %in% names(result))  # original preserved by mutate()
})

# --- Multiple rows -----------------------------------------------------------

test_that("all rows in a mixed dataframe are scaled correctly", {
  df <- tibble::tibble(amount = c(500, 1.5e3, 2e6, 3e9, 4e12))
  result <- rescale_number(df, amount)
  expect_equal(result$amount,        c(500, 1.5, 2, 3, 4))
  expect_equal(result$amount_suffix, c("", "K", "M", "B", "T"))
})

# --- Error handling ----------------------------------------------------------

test_that("passing zero columns raises an error", {
  expect_error(
    rescale_number(make_df(1)),
    "`rescale_number\\(\\)` requires exactly one column\\."
  )
})

test_that("passing two columns raises an error", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(
    rescale_number(df, a, b),
    "`rescale_number\\(\\)` requires exactly one column\\."
  )
})
