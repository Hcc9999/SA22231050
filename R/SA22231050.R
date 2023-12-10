#' @title bisection
#' @description Find the target value using the dichotomy method
#' @param x target sequence
#' @param target the target number
#' @return mid target value subscript
#' @examples
#' \dontrun{
#' x <- seq(1:100)
#' bisection(x, 15)
#' }
#' @export
bisection <- function(x,  target) {
  l = 1
  r = length(x)
  iter_n = 0
  
  ITER_UPPER = 500
  iter_n = 0
  
  while (TRUE) {
    iter_n = iter_n + 1
    mid = (l + r) / 2
    mid = floor(mid)
    if (iter_n > ITER_UPPER) {
      stop('not found solution')
    }
    mid_val = x[mid]
    if (mid_val < target) {
      l = mid+1
    } else if (mid_val > target) {
      r = mid-1
    } else {
      return (mid)
    }
  }
  return(-1)
}


#' @title newton
#' @description Newton's method to find roots
#' @param f equation
#' @param f1 Derivatives of Eq.
#' @param x0 the init number
#' @param tol the tolerant number
#' @param max_iter the max number of loop
#' @return x solution 
#' @examples 
#' \dontrun{
#' ans <- newton(f = function(x) x^2 - 4, f1 = function(x) 2*x, x0 = -5, tol = 1e-6)
#' }
#' @export
newton <- function(f, f1, x0, tol, max_iter = 1000) {
  x = x0
  f = f(x)
  f1 = f1(x)
  iter = 0
  while (TRUE) {
    if (f1 == 0) {
      stop("The derivative is 0")
    }
    x = x - f / f1
    f = f(x)
    f1 = f1(x)

    iter = iter + 1
    if ((abs(x - x0) < tol)) {
      return(c(x, iter))
    }
    x0 = x 
    if (iter > max_iter) {
      stop("cannot find solution")
    }
  }
}

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C++ functions between bisection
#' @examples
#' \dontrun{
#' x1 <- seq(1:50)
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = bisection(x <- seq(1:10), target <- 8),
#'   rnC = binarySearchC(NumericVector x = x1, double target = 8)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @useDynLib SA22231050
NULL

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C++ functions between newton
#' @examples
#' \dontrun{
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = newton(f = function(x) x^2 - 4, f1 = function(x) 2*x, x0 = -5, tol = 1e-6)
#'   rnC = newtonMethodC(x0=-5,tol=1e-6,max_iter=1000)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @useDynLib SA22231050
NULL



