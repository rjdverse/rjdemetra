# USER:
# Method "regarima" for the function coef
#' @export
coef.regarima <- function(object, component = c("regression", "arima", "both"), ...){
  component <- match.arg(component)
  if (component == "regression") {
    object$regression.coefficients[, "Estimate"]
  } else if (component == "arima") {
    object$arima.coefficients[, "Estimate"]
  } else{
    c(object$arima.coefficients[, "Estimate"],
      object$regression.coefficients[, "Estimate"])
  }
}
#' @export
coef.SA <- function(object, component = c("regression", "arima", "both"), ...){
  coef.regarima(object$regarima, component, ...)
}
# USER:
# Method "regarima" for the function logLik
# attributes: df = number of parameters, nobs = number of effective observations
#' @export
logLik.regarima <- function(object, ...) {
  res <- if (is.null(object$loglik["logvalue", ]))
    NA
  else structure(object$loglik["logvalue", ],
                 df = object$loglik["np",],
                 nobs = object$loglik["neffectiveobs", ])
  class(res) <- "logLik"
  res
}
#' @export
logLik.SA <- function(object, ...) {
  logLik.regarima(object$regarima, ...)
}
#' @export
vcov.regarima <- function(object, ...){
  jmod <- jregarima(object)
  result <- get_indicators(jmod, "model.covar")[[1]]
  rownames(result) <- colnames(result) <-
    result <- get_indicators(jmod, "model.description")[[1]]
  result
}
#' @export
vcov.SA <- function(object, ...){
  if ("preprocessing.model.covar" %in% names(object$user_defined)) {
    result <- object$user_defined$preprocessing.model.covar
    rownames(result) <- colnames(result) <- rownames(object$regarima$regression.coefficients)
  }else{
    result <- vcov.regarima(object$regarima, ...)
  }
  result
}
#' @export
df.residual.regarima <- function(object, ...){
  object$loglik["neffectiveobs",] - object$loglik["np",]
}
#' @export
df.residual.SA <- function(object, ...){
  df.residual.regarima(object$regarima, ...)
}
#' @export
nobs.regarima <- function(object, ...){
 object$loglik["neffectiveobs",]
}
#' @export
nobs.SA <- function(object, ...){
  nobs.regarima(object$regarima, ...)
}
#' @export
residuals.regarima <- function(object, ...){
  object$residuals
}
#' @export
residuals.SA <- function(object, ...){
  residuals.regarima(object$regarima, ...)
}
