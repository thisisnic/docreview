test_that("vignette_review works as expected", {
  vr <- vignette_review(system.file("testpkg", package = "docreview", mustWork = TRUE), checks = get_config()$vignettes)
  expect_equal(vr$failures, 3)
  expect_equal(vr$warnings, 2)

  expect_named(vr$details, c("testpkg.Rmd", "testpkg2.Rmd"))

  expect_equal(vr$details$testpkg.Rmd$flesch_kincaid, 42, tolerance = 0.1)
  expect_equal(vr$details$testpkg.Rmd$length, 382)

  expect_equal(vr$details$testpkg2.Rmd$flesch_kincaid, 38, tolerance = 0.1)
  expect_equal(vr$details$testpkg2.Rmd$length, 182)
})
