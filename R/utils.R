#' Printed summary of hiperglm object
#'
#' @details This function returns prints summary characteristics of a fitted hiperglm object.
#'
#' @param x a fitted hiperglm object.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'

print.hiperglm <- function(x, ...){
  sapply(x, print)
}


#' Estimated coefficients of hiperglm object
#'
#' @details This function returns the estimated regression coefficients of a fitted hiperglm object.
#'
#' @param hiperglm a fitted hiperglm object.
#'
#' @export
#'

coef.hiperglm <- function(hiperglm){
  return(hiperglm$results)
}


#' Estimated variance-covariance matrix of hiperglm object
#'
#' @details This function returns the estimated variance-covariance matrix of a fitted hiperglm object.
#'
#' @param hiperglm a fitted hiperglm object.
#'
#' @export
#'

vcov.hiperglm <- function(hiperglm){
  warning("Yet to be implemented")
}
