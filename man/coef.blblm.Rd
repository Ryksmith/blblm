% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{coef.blblm}
\alias{coef.blblm}
\title{Estimated Coefficients}
\usage{
\method{coef}{blblm}(object, ...)
}
\arguments{
\item{object}{The input models.}

\item{...}{Other arguments}
}
\description{
Calculates the final estimated regression coefficients by taking the mean from each of the m groups
calculated during each of the B boostraps.
}
