are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

simulate_data <- function(
    n_obs, n_pred, model = "linear", intercept = NULL,
    coef_true = NULL, design = NULL, seed = NULL, option = list()
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if ((model != "linear")  && !is.null(option$signal_to_noise)) {
    warning(paste(
      "The `signal_to_noise` option is currently unsupported for",
      "non-linear models and will be ignored."
    ))
  }
  if (is.null(coef_true)) {
    coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
  }
  if (is.null(design)) {
    design <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  }
  if (!is.null(intercept)) {
    if (!is.numeric(intercept)) {
      stop("The intercept argument must be numeric.")
    }
    coef_true <- c(intercept, coef_true)
    design <- cbind(rep(1, n_obs), design)
  }
  expected_mean <- as.vector(design %*% coef_true)
  if (model == 'linear') {
    signal_to_noise <- option$signal_to_noise
    if (is.null(signal_to_noise)) {
      signal_to_noise <- 0.1
    }
    noise_magnitude <- sqrt(var(expected_mean) / signal_to_noise^2)
    noise <- noise_magnitude * rnorm(n_obs)
    outcome <- expected_mean + noise
  } else {
    n_trial <- option$n_trial
    prob <- 1 / (1 + exp(-expected_mean))
    # Object type of `outcome` returned by this function is variable. One should
    # in general be careful about introducing this type of inconsistency, but
    # sometimes one might find it the most natural and/or reasonable thing to do.
    if (is.null(n_trial)) {
      outcome <- rbinom(n_obs, 1, prob)
    } else {
      n_success <- rbinom(n_obs, n_trial, prob)
      outcome <- list(n_success = n_success, n_trial = n_trial)
    }
  }
  return(list(design = design, outcome = outcome, coef_true = coef_true))
}

#' @details This function computes, using centered difference, a numeric approximation of the gradient of any given function.
#'
#' @param function a function that takes in one vector input (x)
#' @param x a vector, the value at which to evaluate the gradient
#' @param dx a vector, the step size to use for the centered difference calculation
#'
#' @return a p x 1 matrix, the estimated gradient at x.
#'
approx_grad_of_func <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  d <- length(x)
  numerical_grad <- matrix(0, nrow = d, ncol = 1)

  for (i in 1:d){
    unit_vec <- rep(0, d)
    unit_vec[i] <- 1
    numerical_grad[i, 1] <- (func(x + dx * unit_vec) - func(x - dx * unit_vec)) / 2 / dx
  }
  return(numerical_grad)
}

#' @details This function computes, using centered difference, a numeric approximation of the hessian of any given function.
#'
#' @param function a function that takes in one vector input (x)
#' @param x a vector, the value at which to evaluate the gradient
#' @param v a vector, the direction in which to evaluate the gradient
#' @param dx a vector, the step size to use for x
#' @param t a scalar, the step size to use for scaling v
#'
#' @return a p x 1 matrix, the estimated hessian at x.
#'
approx_hess_of_func_at_vec <- function(func, x, v, dx = .Machine$double.eps^(1/3), t = .Machine$double.eps^(1/3)) {
  grad_as_func <- function(i) approx_grad_of_func(func, i, dx)
  deriv_of_grad <- (grad_as_func(x + t*v) - grad_as_func(x - t*v)) / 2 / t
  hess_as_matrix <- as.matrix(deriv_of_grad, ncol = 1)
  return(hess_as_matrix)
}
