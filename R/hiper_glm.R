#' Constructor for objects of the class hiperglm
#'
#' @details This function makes a hiperglm object from a design matrix and an outcome vector.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#' @param model a character indicating the link function between the linear predictors and the mean of the outcome.
#'
#' @return a fitted hiperglm object.
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = "linear", option = list(mle_solver = "newton")){
  # to do: check that design and outcome are correctly formatted

  if (option$mle_solver == "BFGS"){
    results <- newton_mle(design, outcome, model)
  } else if (option$mle_solver == "BFGS"){
    results <- bfgs_mle(design, outcome, model)
  }

  # equivalent: hiperglm <- list(); class(hiperglm) <- "hiperglm"
  hiperglm <- structure(list(design = design,
                             outcome = outcome,
                             results = results),
                        class = "hiperglm")
  return(hiperglm)
}
