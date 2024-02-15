test_that("BFGS and pseudo-inverse LS estimates of linear model (n=100, p=2) are close (abs_tol=1e-6, rel_tol=1e-4)", {
  data <- simulate_data(n_obs = 100, n_pred = 2, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  mle_pseudoinv <- pseudoinverse_mle(data$design, data$outcome)
  mle_bfgs <- bfgs_mle(data$design, data$outcome)

  expect_true(are_all_close(mle_bfgs, mle_pseudoinv, abs_tol = 1e-6, rel_tol = 1e-4))
})

test_that("BFGS and pseudo-inverse LS estimates of linear model (n=500, p=5) are close (abs_tol=1e-6, rel_tol=1e-4)", {
  data <- simulate_data(n_obs = 500, n_pred = 5, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778, signal_to_noise = 0.1)

  mle_pseudoinv <- pseudoinverse_mle(data$design, data$outcome)
  mle_bfgs <- bfgs_mle(data$design, data$outcome)

  expect_true(are_all_close(mle_bfgs, mle_pseudoinv, abs_tol = 1e-6, rel_tol = 1e-4))
})
