test_that("gradient calculation matches numerical gradient calculation (model='linear', n=80, p=3, beta=0)", {
  n_obs <- 80
  n_pred <- 3
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  beta <- rep(0, n_pred + 1)
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta)

  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("gradient calculation matches numerical gradient calculation (model='linear', n=200, p=6, beta=c(-1, 1, -1, 1, -1, 1, -1))", {
  n_obs <- 200
  n_pred <- 6
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  beta <- c(-1, 1, -1, 1, -1, 1, -1)
  analytic_gradient <- gradient_of_loglik(data$design, data$outcome, beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(data$design, data$outcome, beta), beta)

  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("gradient calculation matches numerical gradient calculation (model='linear', n=100, p=2, beta=1:(n_pred+1)/2)", {
  n_obs <- 100
  n_pred <- 2
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "linear", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  beta <- 1:(n_pred + 1) / 2
  analytic_gradient <- gradient_of_loglik(design = data$design, outcome = data$outcome, beta = beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(design = data$design, outcome = data$outcome, beta = beta), beta)

  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("gradient calculation matches numerical gradient calculation (model='logistic', n=100, p=2, beta=1:(n_pred+1)/2)", {
  n_obs <- 100
  n_pred <- 2
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "logistic", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  beta <- 1:(n_pred + 1) / 2
  analytic_gradient <- gradient_of_loglik(design = data$design, outcome = data$outcome, model = "logistic", beta = beta)
  numeric_gradient <- approx_grad_of_func(function(beta) loglik(design = data$design, outcome = data$outcome, model = "logistic", beta = beta), beta)

  expect_true(are_all_close(analytic_gradient, numeric_gradient))
})

test_that("Hessian calculation matches numerical gradient calculation (model='logistic', n=100, p=2, beta=1:(n_pred+1)/2, evaluate at 2 unit vectors)", {
  n_obs <- 100
  n_pred <- 2
  data <- simulate_data(n_obs = n_obs, n_pred = n_pred, model = "logistic", intercept = 1,
                        coef_true = NULL, design = NULL, seed = 140778)

  beta <- 1:(n_pred + 1) / 2
  vector1 <- c(1, rep(0, n_pred))
  vector2 <- c(rep(0, n_pred), 1)

  analytic_hessian <- hessian_of_loglik(design = data$design, outcome = data$outcome, model = "logistic", beta = beta)
  analytic_hessian1 <- analytic_hessian %*% vector1
  analytic_hessian2 <- analytic_hessian %*% vector2

  numeric_hessian1 <- approx_hess_of_func_at_vec(func = function(beta) loglik(design = data$design, outcome = data$outcome, model = "logistic", beta = beta),
                                                 x = beta, v = vector1)
  numeric_hessian2 <- approx_hess_of_func_at_vec(func = function(beta) loglik(design = data$design, outcome = data$outcome, model = "logistic", beta = beta),
                                                 x = beta, v = vector2)

  expect_true(are_all_close(analytic_hessian1, numeric_hessian1, abs_tol = 1e-4, rel_tol = 1e-3))
  expect_true(are_all_close(analytic_hessian2, numeric_hessian2, abs_tol = 1e-4, rel_tol = 1e-3))
})
