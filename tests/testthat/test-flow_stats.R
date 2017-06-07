#' @export
#' @importFrom dplyr group_by mutate
#' 
context("dataframe input")
test_that("flow.ts is a dataframe", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  expect_is(flow.ts, "data.frame")
})
#' 
context("flow.ts streamflow column is numeric and called 'flow'")
test_that("flow.ts$flow is numeric", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  expect_true(is.numeric(flow.ts$flow))
 })
#' 
context("flow.ts year column is 4 digit numeric and called 'year'")
test_that("flow.ts$year is numeric and 4 digits", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  expect_true(is.numeric(flow.ts$year))
  expect_equal(nchar(as.character(flow.ts$year)), rep(4,6))
})
#' 
context("create a new column called 'flow_pk'")
test_that("new column", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  flow.ts <- add_annual_peak_flow(flow.ts)
  expect_true(is.numeric(flow.ts$flow.pk))
})
#' 
context("create a new column called 'flow_med'")
test_that("new column", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  flow.ts <- add_annual_median_flow(flow.ts)
  expect_true(is.numeric(flow.ts$flow.med))
})
#' 
context("create a new column called 'flow_low'")
test_that("new column", {
  flow.ts <- data.frame(flow=c(1:6), year=2010)
  flow.ts <- add_annual_low_flow(flow.ts)
  expect_true(is.numeric(flow.ts$flow.low))
})

