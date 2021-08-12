test_that("get_SSURGO_component_keys works", {
  expect_error(get_SSURGO_component_keys(".", ''))
})

test_that("get_SSURGO_cointerp works", {
  expect_error(get_SSURGO_cointerp(".", ''))
})

test_that("get_SSURGO_interp_reasons_by_mrulename works", {
  expect_error(get_SSURGO_interp_reasons_by_mrulename(".", ''))
})
