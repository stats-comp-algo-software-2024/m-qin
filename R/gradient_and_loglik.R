#' Calculate log likelihood of data in linear regression model at a point
#'
#' @details This function computes the log likelihood of data in a linear regression model at a given value of beta.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param beta a p-length numeric vector at which to evaluate the log likelihood, possibly including an intercept.
#' @param Sigma_inv the covariance matrix for the errors; default is nxn identity matrix; doesn't influence MLE estimate of beta (though would influence estimate of variance of beta).
#'
#' @return The log likelihood of the data at beta.
#'
loglik <- function(design, outcome, beta, Sigma_inv = diag(length(outcome))){
  residual <- outcome - design %*% beta
  logp <- -(1/2) * t(residual) %*% Sigma_inv %*% residual
  return(logp)
}

#' @details This function computes, analytically, the gradient of the log likelihood in linear regression.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param beta a p-length numeric vector at which to evaluate the log likelihood, possibly including an intercept.
#' @param Sigma_inv the covariance matrix for the errors; default is nxn identity matrix; doesn't influence MLE estimate of beta (though would influence estimate of variance of beta).
#'
#' @return The gradient of the log likelihood of the data at beta.
#'
gradient_of_loglik <- function(design, outcome, beta, Sigma_inv = diag(length(outcome))) {
  residual <- outcome - design %*% beta
  grad <- t(design) %*% Sigma_inv %*% residual # chain rule on log likelihood
  grad <- as.vector(grad) # return as vector, not p x 1 matrix, for conformity with other functions
  return(grad)
}
