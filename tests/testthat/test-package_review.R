pkg_path <- system.file("testpkg", package = "docreview", mustWork = TRUE)

test_that("package_review can exclude sections from review", {
  just_func <- get_config(system.file(package = "docreview", "configs", "just_functions.yml", mustWork = TRUE))
  pr1 <- suppressMessages(package_review(pkg_path, just_func))
  expect_named(pr1, "functions")

  just_vignettes <- get_config(system.file(package = "docreview", "configs", "just_vignettes.yml", mustWork = TRUE))
  pr2 <- suppressMessages(package_review(pkg_path, just_vignettes))
  expect_named(pr2, "vignettes")
})

test_that("package_review can raise errors", {
  error <- get_config(system.file(package = "docreview", "configs", "error.yml", mustWork = TRUE))
  warn <- get_config(system.file(package = "docreview", "configs", "warn.yml", mustWork = TRUE))

  expect_error(
    suppressMessages(package_review(pkg_path, error)),
    regexp = "Failures found by docreview: 1"
  )

  expect_error(
    suppressMessages(package_review(pkg_path, warn)),
    regexp = "Warnings found by docreview: 2"
  )
})

test_that("package_review can set custom thresholds", {
  thresholds <- get_config(system.file(package = "docreview", "configs", "thresholds.yml", mustWork = TRUE))

  res <- suppressMessages(package_review(pkg_path, thresholds))
  expect_equal(res$functions$failures, 0)
  expect_equal(res$functions$warnings, 1)
  expect_equal(res$vignettes$failures, 2)
  expect_equal(res$vignettes$warnings, 2)
})

test_that("package_review default thresholds", {
  res <- suppressMessages(package_review(pkg_path))
  expect_equal(res$functions$failures, 1)
  expect_equal(res$functions$warnings, 0)
  expect_equal(res$vignettes$failures, 3)
  expect_equal(res$vignettes$warnings, 2)
})
