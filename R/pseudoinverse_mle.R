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
  n_pred <- ncol(design) # p
  pseudoinv <- solve(t(design) %*% design, diag(n_pred)) # (X^T X)^{-1}
  beta_hat <- pseudoinv %*% t(design) %*% outcome
  beta_hat <- as.vector(beta_hat) # return as vector, not p x 1 matrix, for conformity with other functions
  return(beta_hat)
}

