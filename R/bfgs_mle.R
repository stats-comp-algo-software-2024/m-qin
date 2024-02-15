#' Calculate the MLE for beta using BFGS
#'
#' @details This function calculates the MLE for beta in a least squares regression
#' using BFGS (a quasi-Newton method using the gradient of the least squares function).
#'
#' @param design a n x p design matrix of numeric or factor predictors, possible including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#'
#' @return The estimated MLE for beta, i.e., value of beta that maximizes the (log) likelihood, as a vector.
#'
bfgs_mle <- function(design, outcome){
  bfgs <- stats::optim(par = rep(0, ncol(design)),
                      fn = function(beta) loglik(design, outcome, beta),
                      gr = function(beta) gradient_of_loglik(design, outcome, beta),
                      method = "BFGS",
                      control = list(fnscale = -1)) # maximize
  mle <- bfgs$par
  return(mle)
}
