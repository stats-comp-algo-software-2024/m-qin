#' Calculate the MLE for beta using pseudoinverse of design matrix
#'
#' @details This function calculates the MLE for beta in a least squares regression
#' using the pseudoinverse of the design matrix.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possible including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#'
#' @return The estimated MLE for beta, i.e., (X^T X)^{-1} X^T y, as a vector.
#'
pseudoinverse_mle <- function(design, outcome){
  beta_hat <- solve(t(design) %*% design, t(design) %*% outcome)
  beta_hat <- as.vector(beta_hat) # for conformity with other functions
  return(beta_hat)
}

