# USER:
# Method "regarima" for the function coef
#' @export
coef.regarima <- function (object, ...){
  c(object$arima.coefficients[,1],object$regression.coefficients[,1])
}
# USER:
# Method "regarima" for the function logLik
# attributes: df = number of parameters, nobs = number of effective observations
#' @export
logLik.regarima <- function (object, ...) {
  res <- if (is.null(object$loglik[1]))
    NA
  else structure(object$loglik[1], df = object$loglik[2],
                 nobs = object$loglik[3])
  class(res) <- "logLik"
  res
}
