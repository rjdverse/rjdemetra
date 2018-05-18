# USER:
# Method JD_RegArima for the function coef
#' @export
coef.JD_RegArima=function (object, ...){
  c(object$arima.coefficients[,1],object$regression.coefficients[,1])
}
# USER:
# Method JD_RegArima for the function logLik
# attributes: df = number of parameters, nobs = number of effective observations
#' @export
logLik.JD_RegArima=function (object, ...) {
  res <- if (is.null(object$loglik[1]))
    NA
  else structure(object$loglik[1], df = object$loglik[2],
                 nobs = object$loglik[3])
  class(res) <- "logLik"
  res
}
