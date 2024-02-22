#' Calculate log likelihood of data in linear regression model at a point
#'
#' @details This function computes the log likelihood of data in a linear regression model at a given value of beta.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#' @param beta a p-length numeric vector at which to evaluate the log likelihood, possibly including an intercept.
#' @param Sigma_inv the covariance matrix for the errors; default is nxn identity matrix; doesn't influence MLE estimate of beta (though would influence estimate of variance of beta).
#'
#' @return A scalar, the log likelihood of the data at beta.
#'
loglik <- function(design, outcome, beta, model = "linear", Sigma_inv = diag(length(outcome))){
  if (model == "linear"){
    residual <- outcome - design %*% beta
    logp <- -(1/2) * t(residual) %*% Sigma_inv %*% residual
  } else if (model == "logistic"){
    linear_pred <- design %*% beta
    logp <- sum(outcome * linear_pred - log(1 + exp(linear_pred))) # equiv: t(outcome) %*% design %*% beta - sum(log(1 + exp(design %*% beta)))
  }
  return(logp)
}

#' @details This function computes, analytically, the gradient of the log likelihood in linear regression.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#' @param beta a p-length numeric vector at which to evaluate the log likelihood, possibly including an intercept.
#' @param Sigma_inv the covariance matrix for the errors; default is nxn identity matrix; doesn't influence MLE estimate of beta (though would influence estimate of variance of beta).
#'
#' @return a p x 1 matrix, the gradient of the log likelihood of the data at beta.
#'
gradient_of_loglik <- function(design, outcome, beta, model = "linear", Sigma_inv = diag(length(outcome))) {
  if (model == "linear"){
    residual <- outcome - design %*% beta
    grad <- t(design) %*% Sigma_inv %*% residual
    grad <- as.vector(grad) # for conformity with other functions
  } else if (model == "logistic"){
    linear_pred <- design %*% beta
    grad <- t(design) %*% outcome - t(design) %*% (exp(linear_pred) / (1 + exp(linear_pred)))
  }
  return(grad)
}

#' @details This function computes, analytically, the Hessian matrix of the log likelihood in linear regression.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#' @param beta a p-length numeric vector at which to evaluate the log likelihood, possibly including an intercept.
#' @param Sigma_inv the covariance matrix for the errors; default is nxn identity matrix; doesn't influence MLE estimate of beta (though would influence estimate of variance of beta).
#'
#' @return a p x 1 matrix, the Hessian of the log likelihood of the data at beta.
#'
hessian_of_loglik <- function(design, outcome, beta, model = "logistic", Sigma_inv = diag(length(outcome))) {
  if (model == "logistic"){
    linear_pred <- design %*% beta
    hess <- - t(design) %*% diag(as.vector(exp(linear_pred) / (1 + exp(linear_pred))^2)) %*% design
  } else{
    hess <- "yet to be implemented"
  }
  return(hess)
}
