context("Missing Values")
test_that("is pct_missing return correct value TRUE",{
  expect_equal(pct_missing(c(1,1,1,NA)) , 25)
})
test_that("is pct_censored return correct value TRUE",{
  expect_equal(pct_censored(c("<1",1,1,1)), 25)
})
test_that("is pct_missing return all NA TRUE",{
  expect_equal(pct_missing(c(NA)),100)
})
test_that("is pct_censored return all censored TRUE",{
  expect_equal(pct_censored(c("<1")),100)
})
test_that("is pct_missing return all data good TRUE",{
  expect_equal(pct_missing(c(1,1,1,1)),0)
})
test_that("is pct_censored return all data good TRUE",{
  expect_equal(pct_censored(c(1,1,1)),0)
})
test_that("is pct_missing return bad input TRUE",{
  expect_warning(pct_missing(NULL)) 
})
