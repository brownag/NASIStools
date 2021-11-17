test_that("create_ESD_ecosites_import works", {

  expect_warning(create_ESD_ecosites_import("test_esd.csv", c(1,1,1), c("A","A","C")))
  expect_silent(create_ESD_ecosites_import("test_esd.csv", c(1,2,3), c("A","B","C")))

  expect_warning(create_ESD_ecosites_import("test_esd.xlsx", c(1,1,1), c("A","A","C")))
  expect_silent(create_ESD_ecosites_import("test_esd.xlsx", c(1,2,3), c("A","B","C")))

  unlink(c("test_esd.csv", "test_esd.xlsx"))
})
