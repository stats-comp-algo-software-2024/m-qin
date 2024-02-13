test_that("gradient calculation matches numerical gradient calculation (n=80, p=3, beta=0)", {
  # get simulated data
  n_obs <- 80
  n_pred <- 3
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  # compute gradient at specific value of beta
  beta <- rep(0, n_pred + 1)
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta)

  # compare
  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("gradient calculation matches numerical gradient calculation (n=200, p=6, beta=c(-1, 1, -1, 1, -1, 1, -1))", {
  # get simulated data
  n_obs <- 200
  n_pred <- 6
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  # compute gradient at specific value of beta
  beta <- c(-1, 1, -1, 1, -1, 1, -1)
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta)

  # compare
  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("gradient calculation matches numerical gradient calculation (n=100, p=2, beta=1:(n_pred+1)/2)", {
  # get simulated data
  n_obs <- 100
  n_pred <- 2
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  # compute gradient at specific value of beta
  beta <- 1:(n_pred + 1) / 2
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta)

  # compare
  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})
