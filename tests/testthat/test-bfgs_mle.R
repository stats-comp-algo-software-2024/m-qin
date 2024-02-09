test_that("BFGS and pseudo-inverse LS estimates of linear model (n=100, p=2) are close", {
  # get simulated data
  n_obs <- 100
  n_pred <- 2
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  # get MLE estimated by pseudoinverse and MLE estimated by BFGS
  mle_pseudoinv <- pseudoinverse_mle(data$design, data$outcome)
  mle_bfgs <- bfgs_mle(data$design, data$outcome)

  # compare
  expect_equal(are_all_close(mle_bfgs, mle_pseudoinv), TRUE)
})
