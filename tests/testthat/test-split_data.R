test_that("multiplication works", {
  w <- split_data(iris, m = 5)
  r <- length(w)
  expect_equal(5, r)
})
