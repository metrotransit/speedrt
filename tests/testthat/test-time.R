context("Transit Time")

test_that("Times past midnight parse", {
  expect_equal(structure(c(as.ITime('1:00'), as.ITime('1:00') + 86400L), class = 'ITime'), as.TransitTime(c('1:00', '25:00')))
})
