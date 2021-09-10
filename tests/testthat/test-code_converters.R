test_that("musym_to_all works", {
  expect_true(is.vector(musym_to_nmusym("asdf")))
  expect_true(is.vector(musym_to_muiid("asdf")))
  expect_true(is.vector(musym_to_dmuiid("asdf")))
  expect_true(is.vector(musym_to_muname("asdf")))
  expect_true(inherits(musym_to_all("asdf"), 'data.frame'))
})
