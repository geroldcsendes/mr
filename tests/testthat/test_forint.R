context("forint code padding to sums")
library(mr)

test_that("forint glues ' HUF' to the end of an amount", {
  expect_equal(forint(42), "42 HUF")
})
