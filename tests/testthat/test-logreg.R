test_that("Logistic regression works", {
   data <- iris
   labels <- rep(0:1,75)
   data$Species <- labels
   fit <- blb_logreg(Species ~ Petal.Length * Sepal.Length, data = data, m = 2, B = 100)
   expect_equal(length(coef(fit)), 4)
})
