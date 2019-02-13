context("tests for censored data functions")

test_that("sep_censored returns a list of two vectors", {
  myvals <- c(10:15, "<6", "<8")
  cen_list <- sep_censored(myvals)
  expect_is(cen_list, "list")
  expect_length(cen_list, 2)
  expect_named(cen_list, c("numeric.vals", "censored"))
})

test_that("which_censored indices", {
  myvals <- c(10:15, "<6", "<8")
  cen_i <- which_censored(myvals)
  expect_is(cen_i, "integer")
  expect_length(cen_i, 2)
  expect_equal(cen_i, c(7,8))
})
