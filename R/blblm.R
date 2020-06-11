#' @import purrr parallel furrr stats bench future readr
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' Logistic Regression
#'
#' Computes the coefficients for the logistic regression.
#' @param formula The formula to be used.
#' @param data The dataframe to be used.
#' @param m Number of splits to make in the dataframe,
#' @param B Number of bootstraps to be performed.
#' @export
#' @examples
#' data <- iris
#' labels <- rep(0:1,75)
#' data$Species <- labels
#' fit <- blb_logreg(Species ~ Petal.Length * Sepal.Length, data = data, m = 2, B = 100)
#' coef(fit)
blb_logreg <- function(formula, data, m = 3, B = 100) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ logreg_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Replicate the input data B times.
#'
#' @param formula Regression formula.
#' @param data The data.
#' @param n Number of total data points to be replicated.
#' @param B  Number of bootstraps.
logreg_each_subsample <- function(formula, data, n, B) {
  replicate(B, logreg1(formula,data), simplify = FALSE)
}



#' Fit Logistic regresion model
#'
#' @param formula Logistic regression model
#' @param data Dataframes to have their coefficients estimated.
#' @export
logreg1 <- function(formula, data) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, family = binomial(), maxit = 100)#weights = freqs
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}





#' Benchmark parallelization
#'
#' Run blblm with the number of cores varied from 1 to max available and compare their benchmarks.
#'
#' An example you can run is:
#' blblm_benchmarks(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' note: This cannot be ran like a normal example due to an issue with the future package.
#'
#' @param formula Formula for the regression
#'
#' @param data The input data frame.
#' @param m The number of splits to make on the data. m > 1.
#' @param B The number of bootstraps to execute. B > 1.
#'
#' @return Returns a dataframe with results for both memory and time usage.
#'
#' @export
blblm_benchmarks <- function(formula, data, m, B){
  max_cores <- availableCores()   ##Number of cores available to the program
  single <- mark(       ##Benchmarking single-core, blblm
    blblm(formula = formula, data = data, m = 10, B = 100), filter_gc = F
  )
  time_results <- as.vector(single[1,3][[1]])      ###Saving run time
  memory_results <- as.vector(single[1,5][[1]])    ##Saving the memory usage
  for (i in 2:max_cores){    ###Running blblm_par with n_core = 2:max_cores
    you <- mark(
      blblm_par(formula = formula, data = data, m = 10, B = 100, ncores = i), filter_gc = F
    )
    time_results <- append(time_results, as.vector(you[1,3][[1]]))
    memory_results <- append(memory_results, as.vector(you[1,5][[1]]))
  }
  results <- data.frame(n_cores = c(1:max_cores), time_results,memory_results)
  ###Making a data frame with n_cores, runtime, and memory usage for 1:n cores.
  return (results)
}


#' Calculate Regression With Parallelization
#'
#' Calculates the linear regression coefficients of a dataframe and formula using a specified number
#' of cores, using B bootstraps and with the data split into m parts.
#'
#' An example is below:
#' fit <- blblm_par(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, ncores = 2)
#' coef(fit)
#' Note: This cannot be ran during check() due to an error with the future package.
#'
#' @param formula The regression formula
#' @param data The input data frame.
#' @param m Number of splits to split the data into. m > 1.
#' @param B Number of bootstraps to be performed in estimating the coefficients. B > 1.
#' @param ncores  Number of cores to use. Must be between 1 and the maximum number of cores you have.
#'
#' @export
blblm_par <- function(formula,data,m,B,ncores){  ###Added ncores, allowing user to pick number of cores to use.
  suppressWarnings(plan(multiprocess, workers = ncores))
  data_list <- split_data(data, m)
  estimates <- data_list %>% future_map(function(x){   ###Future map, parallel map.
    lm_each_subsample(formula = formula, data = x, n = nrow(data), B = B)
  })
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

####REturns the data as a data_list
input_files <- function(path){
  file_names <- file.path(path, list.files(path))
  single_core <- map(file_names, function(i){
    read_csv(i)
  })
  data_list <- single_core
  return (data_list)
}


#' Parallel regression, User specified data
#'
#' Allows the usewr to computer coefficients for the linear regression using a specified
#' number of cores, and using their own input csv files.
#'
#'An example:
#' dir.create("files", showWarnings = FALSE)
#' set.seed(141)
#' 1:100 %>% walk(function(i) {
#'   dt <- tibble(x = rnorm(5000), y = rnorm(5000))
#'   write_csv(dt, file.path("files", sprintf("file%02d.csv", i)))
#' })
#' fit <- blblm_par_user(formula = y ~ 2 * x, path = 'files',ncores = 2, B = 100)
#' coef(fit)
#'
#' Note: This cannot be run in check() due to an error with the future package.
#'
#' @param formula The linear regression formula.
#'
#' @param path The path to the folder containing the csv files.
#' @param ncores The desires number of cores to use. ncores > 0.
#' @param B The number of bootstraps to be performed. B > 1.
#'
#' @export
blblm_par_user <- function(formula,path, ncores, B){  ###Added ncores, allowing user to pick number of cores to use.
  suppressWarnings(plan(multiprocess, workers = ncores))
  data_list <- input_files(path)
  l <- map(data_list, function(i){
    length = nrow(i)
  })
  nfile <- length(l)
  nrows <- 0
  for (i in 1:nfile){
    nrows = nrows + l[i][[1]]
  }
  estimates <- data_list %>% future_map(function(q){   ###Future map, parallel map.
    lm_each_subsample(formula = formula, data = q, n = nrows, B = B)
  })
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Linear Regression Coefficients
#'
#' Calculates the linear regression coefficients of an input formula, data, number of splits m,
#' and number of bootstraps, B. Single-core program.
#'
#' @param formula Formula for the linear regression.
#'
#' @param data The dataframe to be used.
#' @param m The number of splits for the dataframe. m > 1
#' @param B The number of bootstraps to use. B > 1
#'
#' @export
#'
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' coef(fit)
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' Splits the input dataframe into m approximately equal sizes randomly.
#'
#' @param data Dataframe to be split.
#' @param m Number of splits to be made.
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)   ##Samples m nums from 1:nrow w/replacement
  data %>% split(idx)   ####Divides data by groups defined by f.
}



#' compute the estimates
#'
#' Calls the function lm_each_boot B times, with B corresponding to the number of bootstraps.
#'
#' @param formula Regression formula
#' @param data Input data frames.
#' @param n Total number of data points
#' @param B  Number of bootstraps to be performed.
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula The regression formula.
#' @param data The input dataframe.
#' @param n The number of datapoints in total.
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' Estimate Coefficients
#'
#' Estimate the regression estimates based on given the number of repetitions and with weights from
#' lm_each_boot. Uses the lm function for linear regression estimation. Returns the estimated coefficients
#' along with the estimated sigma(Standard deviation).
#'
#' @param formula The input Formula.
#' @param data The input dataframe,
#' @param freqs The frequencies to use as weights in the lm.
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' Grabs coefficients
#'
#' Grabs the estimated regression coefficients from the fitted linear regression model.
#'
#' @param fit The fitted linear regression model from lm().
blbcoef <- function(fit) {
  coef(fit)
}


#' Compute sigma from fit
#'
#' Computed the standard deviation estimation from the fitted linear regression model.
#'
#' @param fit The linear regression model from lm().
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' Prints Estimated model
#'
#' Prints the estimated model from the average of the B bootstraps.
#'
#' @param x The estimated coefficients, sigma, etc.
#'
#' @param ... Other arguments
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' Caculates Sigma
#'
#' Calculates the estimated standard deviation by taking the mean of each of the standard devations
#' estimated during each boostrap.
#'
#' @param object The lm models, of which to estimate the standard devation.
#'
#' @param confidence Unknown,
#' @param level The confidence level for which to calculate the standard deviation.
#' @param ... Other arguments
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Estimated Coefficients
#'
#' Calculates the final estimated regression coefficients by taking the mean from each of the m groups
#' calculated during each of the B boostraps.
#'
#'
#' @param object The input models.
#'
#' @param ... Other arguments
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' Confidence Interval
#'
#' Gives a 95% confidence interval for the coefficients used in the formula specified in the linear regression.
#'
#'
#' @param object The object from which the coefficients are estimated.
#'
#' @param parm The paramters for which to find a confidence interval.
#' @param level The % confidence level of the estimate.
#' @param ... Other arguments
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' LM Prediction
#'
#' Predicts the value of new data based on the coefficients estimated by blblm.
#'
#' @param object The final estimated fitted linear regression model.
#'
#' @param new_data New data to predict on.
#' @param confidence Unknown.
#' @param level The % confidence interval for this prediction,
#' @param ... Other arguments.
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
