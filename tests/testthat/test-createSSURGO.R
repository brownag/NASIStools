test_that("createSSURGO works", {
  expect_error(createSSURGO(tempdir()), "No TXT files found")
})
