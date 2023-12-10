## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  #R实现
#  bisection <- function(x,  target) {
#    l = 1
#    r = length(x)
#    iter_n = 0
#  
#    ITER_UPPER = 500
#    iter_n = 0
#  
#    while (TRUE) {
#      iter_n = iter_n + 1
#      mid = (l + r) / 2
#      mid = floor(mid)
#      if (iter_n > ITER_UPPER) {
#        stop('not found solution')
#      }
#      mid_val = x[mid]
#      if (mid_val < target) {
#        l = mid+1
#      } else if (mid_val > target) {
#        r = mid-1
#      } else {
#        return (mid)
#      }
#    }
#    return(-1)
#  }
#  x <- seq(1:100)
#  bisection(x, 15)
#  
#  #c++
#  library(Rcpp)
#  cppFunction(code='
#  int binarySearchC(NumericVector x, double target) {
#    int low = 0;
#    int high = x.size() - 1;
#  
#    while (low <= high) {
#      int mid = (low + high) / 2;
#      double midVal = x[mid];
#  
#      if (midVal < target) {
#        low = mid + 1;
#      } else if (midVal > target) {
#        high = mid - 1;
#      } else {
#        return mid+1;
#      }
#    }
#  ')
#  
#  x <- seq(1:100)
#  result <- binarySearchC(x, target = 15)
#  print(result)
#  
#  #速度比较
#  library(microbenchmark)
#  tm1 <- microbenchmark::microbenchmark(
#     rnR = bisection(x = seq(1:100), 15),
#     rnC = binarySearchC(NumericVector x = seq(1:100), target = 15)
#  )
#  print(summary(tm1)[,c(1,3,5,6)])
#  

## ----eval=FALSE---------------------------------------------------------------
#  #R
#  newton <- function(f, f1, x0, tol, max_iter = 1000) {
#    x = x0
#    f = f(x)
#    f1 = f1(x)
#    iter = 0
#    while (TRUE) {
#      if (f1 == 0) {
#        stop("The derivative is 0")
#      }
#      x = x - f / f1
#      f = f(x)
#      f1 = f1(x)
#  
#      iter = iter + 1
#      if ((abs(x - x0) < tol)) {
#        return(c(x, iter))
#      }
#      x0 = x
#      if (iter > max_iter) {
#        stop("cannot find solution")
#      }
#    }
#  }
#  ans <- newton(f = function(x) x^2 - 4, f1 = function(x) 2*x, x0 = -5, tol = 1e-6)
#  
#  #c++
#  library(Rcpp)
#  
#  cppFunction(code='
#  double f(double x) {
#     return x*x-4;
#  }
#  ')
#  
#  cppFunction(code='
#  double f_prime(double x) {
#     return 2*x;
#  }
#  ')
#  cppFunction(code='
#  double newtonMethodC(double x0, double tol, int max_iter) {
#    double x = x0;
#    double fx = f(x);
#    double fpx = f_prime(x);
#    int iter = 0;
#  
#    while (fabs(fx) > tol && iter < max_iter) {
#      x = x - fx / fpx;
#      fx = f(x);
#      fpx = f_prime(x);
#      iter++;
#    }
#  
#    return x;
#  }
#  ')
#  x0 <- -9
#  tol <- 1e-6
#  max_iter <- 100
#  result <- newtonMethodC(x0, tol, max_iter)
#  print(result)
#  
#  

