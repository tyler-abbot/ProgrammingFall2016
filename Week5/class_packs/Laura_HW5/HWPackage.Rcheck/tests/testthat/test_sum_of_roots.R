context("Sum_of_roots")

test_that("Outputs the correct thing.",{
  expect_that(sum_of_roots(9, 16), equals(7))
})