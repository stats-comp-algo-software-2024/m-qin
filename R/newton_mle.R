#' Calculate the MLE for beta using Newton's method
#'
#' @details This function calculates the MLE for beta in a least squares regression
#' using Newton's method (using the gradient and Hessian of the least squares function).
#'
#' @param design a n x p design matrix of numeric or factor predictors, possible including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#'
#' @return The estimated MLE for beta, i.e., value of beta that maximizes the (log) likelihood, as a vector.
#'
newton_mle <- function(design, outcome, model = "logistic", n_iter = 10){
  n_pred <- ncol(design)
  beta <- matrix(0, nrow = n_pred, ncol = 1) # initial value

  for (i in 1:n_iter){
    grad <- gradient_of_loglik(design, outcome, beta, model)
    hess <- hessian_of_loglik(design, outcome, beta, model)
    # beta <- beta - solve(hess) %*% grad # don't use this because inverts a matrix
    beta_diff <- solve(- hess, grad)
    beta <- beta + beta_diff
  }

  return(beta)
}
