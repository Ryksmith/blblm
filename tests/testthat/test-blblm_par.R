test_that("blblm_par works", {
  fit <- blblm_par(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, 2)
  expect_s3_class(fit, "blblm")
  co <- coef(fit)
  expect_equal(length(co), 4)
})
