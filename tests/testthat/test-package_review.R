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
  custom_thresholds <- list(
    functions = list(exports_without_examples = "warn"),
    vignettes = list(
      fk = list(fail = 40, warn = 50),
      length = list(
        fail = 200,
        warn = 100
      )
    )
  )

  res <- suppressMessages(package_review(pkg_path, thresholds = custom_thresholds))

  expect_equal(res$functions$failures, 0)
  expect_equal(res$functions$warnings, 1)
  expect_equal(res$vignettes$failures, 3)
  expect_equal(res$vignettes$warnings, 1)
})
