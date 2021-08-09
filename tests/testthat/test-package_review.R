pkg_path <- system.file("testpkg", package = "docreview")

test_that("package_review works as expected", {
  pr <- suppressMessages(package_review(pkg_path))
  expect_named(pr, c("functions", "vignettes"))
})

test_that("package_review can exclude sections from review", {
  pr1 <- suppressMessages(package_review(pkg_path, doc_types = "functions"))
  expect_named(pr1, "functions")

  pr2 <- suppressMessages(package_review(pkg_path, doc_types = "vignettes"))
  expect_named(pr2, "vignettes")
})

test_that("package_review can exclude sections from review", {
  expect_error(
    suppressMessages(package_review(pkg_path, error_on_failure = TRUE)),
    regexp = "Failures found by docreview: 1"
  )

  expect_error(
    suppressMessages(package_review(pkg_path, error_on_warning = TRUE)),
    regexp = "Warnings found by docreview: 2"
  )
})

test_that("package_review can set custom thresholds", {
  custom_thresholds <- set_thresholds(
    exports_without_examples = "warn",
    fk_fail = 40, fk_warn = 50,
    length_fail = 200, length_warn = 100
  )

  res <- suppressMessages(package_review(pkg_path, thresholds = custom_thresholds))

  expect_equal(res$functions$failures, 0)
  expect_equal(res$functions$warnings, 1)
  expect_equal(res$vignettes$failures, 3)
  expect_equal(res$vignettes$warnings, 1)
})

test_that("set_thresholds invalid threshold value", {
  expect_error(
    set_thresholds(exports_without_examples = "irrelevant"),
    "'arg' should be one of “fail”, “warn”, “none”"
    )


})
