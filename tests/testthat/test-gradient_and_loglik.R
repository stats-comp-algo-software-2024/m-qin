test_that("gradient calculation matches numerical gradient calculation", {
  # get simulated data
  n_obs <- 80
  n_pred <- 3
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  # compute gradient at specific value of beta
  beta0 <- rep(0, n_pred + 1)
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta0)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta0)

  # compare
  expect_equal(are_all_close(analytic_gradient, numeric_gradient), TRUE)
})
