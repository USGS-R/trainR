context("tests for rmse functions")

test_that("edge case when predictions = observations", {
  obs <- 1:5
  pred <- 1:5
  expect_equal(rmse(obs,pred),0)
})

test_that("math works", {
  obs <- 1:5
  pred <- 11:15
  expect_equal(rmse(obs,pred), 10)
})