test_that("Benchmark works", {
  expect_equal(as.integer(nrow(blblm_benchmarks(mpg ~ wt * hp, data = mtcars, m = 3, B = 100))),
               as.integer(future::availableCores()))
})
