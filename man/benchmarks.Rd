% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SA22231050.R
\name{benchmarks}
\alias{benchmarks}
\title{Benchmark R and Rcpp functions.}
\description{
Use R package \code{microbenchmark} to compare the performance of C++ functions between bisection

Use R package \code{microbenchmark} to compare the performance of C++ functions between newton
}
\examples{
\dontrun{
x1 <- seq(1:50)
tm1 <- microbenchmark::microbenchmark(
  rnR = bisection(x <- seq(1:10), target <- 8),
  rnC = binarySearchC(NumericVector x = x1, double target = 8)
)
print(summary(tm1)[,c(1,3,5,6)])
}
\dontrun{
tm1 <- microbenchmark::microbenchmark(
  rnR = newton(f = function(x) x^2 - 4, f1 = function(x) 2*x, x0 = -5, tol = 1e-6)
  rnC = newtonMethodC(x0=-5,tol=1e-6,max_iter=1000)
)
print(summary(tm1)[,c(1,3,5,6)])
}
}
