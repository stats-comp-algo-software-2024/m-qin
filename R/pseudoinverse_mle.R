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
  # we want beta_hat = (X^T X)^{-1} X^T y
  # i.e., X^T X beta_hat = X^T y
  # so beta_hat is the solution to solve(X^T X, X^T y)
  beta_hat <- solve(t(design) %*% design, t(design) %*% outcome) # to do: think about whether t(design) is computationally intensive; should I save it somewhere?

  # return as vector, not p x 1 matrix, for conformity with other functions
  beta_hat <- as.vector(beta_hat)
  return(beta_hat)
}

