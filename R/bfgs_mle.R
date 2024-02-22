#' Calculate the MLE for beta using BFGS
#'
#' @details This function calculates the MLE for beta in a least squares regression
#' using BFGS (a quasi-Newton method using the gradient of the least squares function).
#'
#' @param design a n x p design matrix of numeric or factor predictors, possible including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#'
#' @return A p-length vector, the estimated MLE for beta, i.e., value of beta that maximizes the (log) likelihood, as a vector.
#'
bfgs_mle <- function(design, outcome, model = "linear"){
  bfgs <- stats::optim(par = rep(0, ncol(design)),
                      fn = function(beta) log_lik(design, outcome, beta, model),
                      gr = function(beta) gradient_of_log_lik(design, outcome, beta, model),
                      method = "BFGS",
                      control = list(fnscale = -1)) # maximize
  mle <- bfgs$par
  return(mle)
}
