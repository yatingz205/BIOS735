#'Two-sample t-tests (equal variance) between two groups
#'
#' This function calculates the two-sample t-test statistic (equal variance)
#' for each row in a matrix based on a given two-level factor.
#'
#' @param x a matrix with m rows and n columns
#' @param f a factor with 2 levels with a total length of n indicating
#' the group assignments for the n elements in a row
#'
#' @return `m` number of t-test statistics for `m` columns of the given matrix
#'
#' @examples
#'
#' m <- 5
#' n <- 50
#' little.n <- n/2
#' set.seed(1)
#'
#' x <- matrix(rnorm(m*n), nrow=m, ncol=n)
#' f <- gl(2, little.n)
#' getT(x, f)
#'
#' @export
getT <- function(x, f) {

  uni.f <- unique(f)

  if(!is.factor(f) | length(uni.f) != 2){
    stop("input f is not a two-level factor")
  }
  if(length(f) != ncol(x)){
    stop("dimension of x and f not matching")
  }
  if (length(f[which(f==uni.f[1])]) != length(f[which(f==uni.f[2])])){
    stop("two groups do not have equal size")
  }


  n1 <- sum(f == 1)
  n2 <- sum(f == 2)
  s1.squareds <- rowSums((x[,f == 1] - rowMeans(x[,f == 1]))^2)
  s2.squareds <- rowSums((x[,f == 2] - rowMeans(x[,f == 2]))^2)

  diffs <- rowMeans(x[, f == 1]) - rowMeans(x[, f == 2])
  s.squareds <- (s1.squareds + s2.squareds)/(n1 + n2 - 2)

  # add a temporary bug
  # s.squareds <- s.squareds*(-1)
  # browser()

  ts <- diffs / sqrt(2*s.squareds/n1)
  ts
}
