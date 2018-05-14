# USER:
# Method JD_RegArima for the function coef
coef.JD_RegArima=function (object, ...){
  c(object$arima.coefficients[,1],object$regression.coefficients[,1])
}
# USER:
# Method JD_RegArima for the function logLik
# attributes: df = number of parameters, nobs = number of effective observations
logLik.JD_RegArima=function (object, ...) {
  res <- if (is.null(object$loglik$loglikelihood))
    NA
  else structure(object$loglik$loglikelihood, df = object$loglik$n.parameters,
                 nobs = object$loglik$eff.obs)
  class(res) <- "logLik"
  res
}
