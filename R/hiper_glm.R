#' Constructor for objects of the class hiperglm
#'
#' @details This function makes a hiperglm object from a design matrix and an outcome vector.
#'
#' @param design a n x p design matrix of numeric or factor predictors, possibly including an intercept.
#' @param outcome a n-length numeric vector of outcomes.
#'
#' @return a fitted hiperglm object.
#'
#' @export
#'
hiper_glm <- function(design, outcome){
  # to do: check that design and outcome are correctly formatted

  # equivalent: hiperglm <- list(); class(hiperglm) <- "hiperglm"
  hiperglm <- structure(list(design = design,
                             outcome = outcome,
                             results = "Yet to be implemented"),
                        class = "hiperglm")
  return(hiperglm)
}
