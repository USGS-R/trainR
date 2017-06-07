context("calc_water_year class and structure correct")
test_that("non-vector returns error", {
  expect_error(calc_water_year(2))
  expect_error(calc_water_year("foo"))
})
test_that("non-date vectors return error", {
  expect_error(calc_water_year(1:3))
  expect_error(calc_water_year(c('a','b','c')))
})
test_that("function returns numeric", {
  expect_equal(class(calc_water_year(seq(as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"), by="day"))),"numeric")
})
