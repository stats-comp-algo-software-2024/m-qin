test_that("two absolutely and relatively close vectors returns TRUE", {
  expect_equal(are_all_close(1:10, 1:10), TRUE)
  expect_equal(are_all_close(1:10, 1:10 + 10^(-10)), TRUE)
  expect_equal(are_all_close(-1, -1 + 10^(-10)), TRUE)
})

test_that("two absolutely but not relatively close vectors returns FALSE", {
  expect_equal(are_all_close(-1e7, -2e7), FALSE)
  expect_equal(are_all_close(c(0, -1e7), c(0, -2e7)), FALSE)
})

test_that("two relatively but not absolutely close vectors returns FALSE", {
  expect_equal(are_all_close(1e7, 1e7 + 1), FALSE)
  expect_equal(are_all_close(c(0, -1e7), c(0, -1e7 + 1)), FALSE)
})
