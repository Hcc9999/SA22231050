#include <Rcpp.h>
using namespace Rcpp;


//' @title bisection
//' @description Find the target value using the dichotomy method
//' @param x target sequence
//' @param target the target number
//' @return mid target value subscript
//' @examples
//' \dontrun{
//'   x <- seq(1:50)
//'   target <- 8
//'   result <- binarySearchC(x, target)
//'   print(result)
//' }
//' @export
// [[Rcpp::export]]
int binarySearchC(NumericVector x, double target) {
  int low = 0;                  
  int high = x.size() - 1;      
  
  while (low <= high) {
    int mid = (low + high) / 2; 
    double midVal = x[mid];     
    
    if (midVal < target) {
      low = mid + 1;            
    } else if (midVal > target) {
      high = mid - 1;           
    } else {
      return mid+1;               
    }
  }
  
  return -1;                    
}

double f(double x) {
   return x*x-4;
}

double f_prime(double x) {
   return 2*x;
}
//' @title newton
//' @description Newton's method to find roots
//' @param x0 the init number
//' @param tol the tolerant number
//' @param max_iter the max number of loop
//' @return mid target value subscript
//' @examples
//' \dontrun{
//' x0 <- -9
//' tol <- 1e-6
//' max_iter <- 100
//' result <- newtonMethodC(x0, tol, max_iter)
//' print(result)
//' }
//' @export
// [[Rcpp::export]]
double newtonMethodC(double x0, double tol, int max_iter) {
  double x = x0;                 
  double fx = f(x);  
  double fpx = f_prime(x);  
  int iter = 0;               
  
  while (fabs(fx) > tol && iter < max_iter) {
    x = x - fx / fpx;         
    fx = f(x);      
    fpx = f_prime(x);  
    iter++;                    
  }
  
  return x;                    
}


