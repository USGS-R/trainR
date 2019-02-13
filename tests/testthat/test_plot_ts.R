test_that("plot_ts works", {
  i<-dev.cur()
   plot_ts(as.Date("2010-08-15"),10,color="red")
   expect_true(dev.cur() == i+1)
})
test_that("plot_ts fails", {
  expect_error(plot_ts("2010-08-15",10,color="red"))
})
