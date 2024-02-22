#' Calculate the MLE for beta using Newton's method
#'
#' @details This function calculates the MLE for beta in a least squares regression
#' using Newton's method (using the gradient and Hessian of the least squares function).
#'
#' @param design a n x p design matrix of numeric or factor predictors, possible including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#' @param abs_conv_thres a double indicating the absolute convergence threshold for change in log likelihood; note that likelihood ratio under null beta tells us we can get a 0.5 increase in log likelihood by chance
#' @param rel_conv_thres a double indicating the relative convergence threshold for change in log likelihood
#' @param max_iter an integer indicating the maximum number of iterations to run before giving up on convergence (if convergence threshold hasn't been met)
#'
#' @return The estimated MLE for beta, i.e., value of beta that maximizes the (log) likelihood, as a vector.
#'
newton_mle <- function(design, outcome, model = "logistic", abs_conv_thres = 1e-6, rel_conv_thres = 1e-6, max_iter = 200){
  n_pred <- ncol(design)
  beta <- matrix(0, nrow = n_pred, ncol = 1) # initial value
  iter <- 0
  loglik <- log_lik(design, outcome, beta, model)

  while (iter < max_iter){
    grad <- gradient_of_log_lik(design, outcome, beta, model)
    hess <- hessian_of_log_lik(design, outcome, beta, model)
    beta_diff <- solve(- hess, grad)
    beta <- beta + beta_diff

    loglik_diff <- log_lik(design, outcome, beta, model) - loglik
    if (abs(loglik_diff) < abs_conv_thres) return(beta)
    if (abs(loglik_diff / loglik) < rel_conv_thres) return(beta)
    loglik <- loglik + loglik_diff
    iter <- iter + 1
  }

  return(beta)
}
