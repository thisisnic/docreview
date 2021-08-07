test_that("get_exports_without_examples", {
  expect_identical(get_exports_without_examples(path = "./testpkg"), "deduct")
})
