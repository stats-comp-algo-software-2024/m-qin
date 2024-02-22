test_that("BFGS and pseudo-inverse LS estimates of linear model (n=100, p=2) are close (abs_tol=1e-6, rel_tol=1e-4)", {
  data <- simulate_data(n_obs = 100, n_pred = 2, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  mle_pseudoinv <- pseudoinverse_mle(data$design, data$outcome)
  mle_bfgs <- bfgs_mle(data$design, data$outcome)

  expect_true(are_all_close(mle_bfgs, mle_pseudoinv, abs_tol = 1e-6, rel_tol = 1e-4))
})

test_that("BFGS and pseudo-inverse LS estimates of linear model (n=500, p=5) are close (abs_tol=1e-6, rel_tol=1e-4)", {
  data <- simulate_data(n_obs = 500, n_pred = 5, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  mle_pseudoinv <- pseudoinverse_mle(data$design, data$outcome)
  mle_bfgs <- bfgs_mle(data$design, data$outcome)

  expect_true(are_all_close(mle_bfgs, mle_pseudoinv, abs_tol = 1e-6, rel_tol = 1e-4))
})

test_that("Newton and BFGS outputs coincide on logit model (n=32, p=4)", {
  data <- simulate_data(n_obs = 32, n_pred = 4, model = 'logit', seed = 1918)
  design <- data$design
  outcome <- data$outcome

  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(design, outcome, model = 'logit', option = list(mle_solver = 'BFGS'))

  expect_true(are_all_close(coef(via_newton_out),
                            coef(via_bfgs_out),
                            abs_tol = 1e-2, rel_tol = 1e-2))
})

test_that("Newton and BFGS outputs coincide on logit model (n=200, p=5)", {
  data <- simulate_data(n_obs = 200, n_pred = 5, model = 'logit', seed = 1918)
  design <- data$design
  outcome <- data$outcome

  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(design, outcome, model = 'logit', option = list(mle_solver = 'BFGS'))

  expect_true(are_all_close(coef(via_newton_out),
                            coef(via_bfgs_out),
                            abs_tol = 1e-2, rel_tol = 1e-2))
})
