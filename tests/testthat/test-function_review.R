test_that("function_review works as expected", {

  fr <- function_review(system.file("testpkg", package = "docreview"))
  expect_equal(fr$failures, 1)
  expect_equal(fr$warnings, 0)
  expect_equal(fr$details$exports_examples, c(add = TRUE, deduct = FALSE))

})
