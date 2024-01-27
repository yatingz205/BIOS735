test_that("test input f", {

  m <- 5
  n <- 4
  x <- matrix(rnorm(m * n), nrow = m, ncol = n)

  expect_error(
    getT(x,c(1,2,3,3)), "not a two-level factor")
  expect_error(
    getT(x,as.factor(c(1,2,2,2))), "two groups do not have equal size")
})


test_that("test dimension of x and f", {

  m <- 40
  n <- 50
  x <- matrix(rnorm(m * n), nrow = m, ncol = n)
  f <- gl(2, (n-1)/2)

  expect_error(
    getT(x,f), "dimension not matching")
})


test_that("output compared with standard function", {

  m <- 5
  n <- 50
  set.seed(1)
  x <- matrix(rnorm(m*n), nrow=m, ncol=n)
  f <- gl(2, n/2)

  ts <- sapply(
    seq_len(m),
    function(i) t.test(x[i,] ~ f, var.equal=TRUE)$statistic)
  ts <- unname(ts)

  expect_equal(getT(x,f), ts)

})

