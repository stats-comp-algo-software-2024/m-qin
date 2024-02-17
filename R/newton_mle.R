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
newton_mle <- function(design, outcome, model = "linear"){

  # to do: change this to be Newton's method not BFGS
  bfgs <- stats::optim(par = rep(0, ncol(design)),
                      fn = function(beta) loglik(design, outcome, beta, model),
                      gr = function(beta) gradient_of_loglik(design, outcome, beta, model),
                      method = "BFGS",
                      control = list(fnscale = -1)) # maximize
  mle <- bfgs$par
  return(mle)
}
