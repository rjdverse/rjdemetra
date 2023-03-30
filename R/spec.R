#' Access a model specification, a SA or a pre-adjustment model in X13 and TRAMO-SEATS
#' @description
#' The following functions enable the access to different parts of the final model specification,
#' as included in the \code{"SA", "regarima", "SA_spec"} and \code{"regarima_spec"} S3 class objects.
#' @param object an object of one of the following classes: \code{c("SA","X13")},
#' \code{c("SA","TRAMO_SEATS")}, \code{c("SA_spec","X13")}, \code{c("SA_spec","TRAMO_SEATS")},
#' \code{c("regarima","X13")}, \code{c("regarima","TRAMO_SEATS")}, \code{c("regarima_spec","X13")},
#' \code{c("regarima_spec","TRAMO_SEATS")}.
#' @name specification
NULL
#' @return
#' \itemize{
#' \item \code{s_estimate} returns a data.frame with the \emph{estimate} variables
#' \item \code{s_transform} returns a data.frame with the \emph{transform} variables
#' \item \code{s_usrdef} returns a data.frame with the \emph{user-defined regressors} (outliers and variables) model specification, indicating if those variables are included in the model and if coefficients are pre-specified
#' \item \code{s_preOut} returns a data.frame with the \emph{pre-specified outliers}
#' \item \code{s_preVar} returns a list with information on the user-defined variables, including: \code{series} - the time series and \code{description} - data.frame with the variable type and coefficients
#' \item \code{s_td} returns a data.frame with the \emph{trading.days} variables
#' \item \code{s_easter} returns a data.frame with the \emph{easter} variable
#' \item \code{s_out} returns a data.frame with the \emph{outliers} detection variables
#' \item \code{s_arima} returns a data.frame with the \emph{arima} variables
#' \item \code{s_arimaCoef} returns a data.frame with the user-specified ARMA coefficients
#' \item \code{s_fcst} returns a data.frame with the forecast horizon
#' \item \code{s_span} returns a data.frame with the \emph{span} variables
#' \item \code{s_x11} returns a data.frame with the \emph{x11} variables
#' \item \code{s_seats} returns a data.frame with the \emph{seats} variables
#' }
#'
#' @references
#' More information and examples related to 'JDemetra+' features in the online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#'
#' @rdname specification
#' @name specification
#' @examples \donttest{
#' myseries <- ipi_c_eu[, "FR"]
#' myreg1 <- regarima_x13(myseries, spec = "RG5c")
#' myspec1 <- regarima_spec_x13(myreg1,
#'              estimate.from = "2005-10-01",
#'              outlier.from = "2010-03-01")
#'
#' s_estimate(myreg1)
#' s_estimate(myspec1)
#'
#' s_transform(myreg1)
#' s_transform(myspec1)
#'
#' s_usrdef(myreg1)
#' s_usrdef(myspec1)
#'
#' myspec2 <- regarima_spec_x13(myreg1, usrdef.outliersEnabled = TRUE,
#'              usrdef.outliersType = c("LS", "AO"),
#'              usrdef.outliersDate = c("2009-10-01", "2005-02-01"))
#' myreg2 <- regarima(myseries, myspec2)
#'
#' s_preOut(myreg2)
#' s_preOut(myspec2)
#'
#' var1 <- ts(rnorm(length(myseries))*10, start = start(myseries), frequency = 12)
#' var2 <- ts(rnorm(length(myseries))*100, start = start(myseries), frequency = 12)
#' var3 <- ts.union(var1, var2)
#' myspec3 <- regarima_spec_x13(spec = "RG5c",
#'                              usrdef.varEnabled = TRUE,
#'                              usrdef.var = var3)
#' myreg3 <- regarima(myseries, myspec3)
#'
#' s_preVar(myspec3)
#' s_preVar(myreg3)
#'
#' s_td(myreg1)
#' s_td(myspec1)
#'
#' s_easter(myreg1)
#' s_easter(myspec1)
#'
#' s_out(myreg1)
#' s_out(myspec1)
#'
#' s_arima(myreg1)
#' s_arima(myspec1)
#'
#' myspec4 <- regarima_spec_x13(myreg1, automdl.enabled = FALSE,
#'              arima.coefEnabled = TRUE,
#'              arima.p = 1,arima.q = 1, arima.bp = 1, arima.bq = 1,
#'              arima.coef = rep(0.2, 4),
#'              arima.coefType = rep("Initial", 4))
#' myreg4 <- regarima(myseries, myspec4)
#'
#' s_arimaCoef(myreg4)
#' s_arimaCoef(myspec4)
#'
#' s_fcst(myreg1)
#' s_fcst(myspec1)
#'
#' s_span(myreg1)
#' s_span(myspec1)
#'
#' myspec5 <- x13_spec(spec = "RSA5c", x11.seasonalComp = FALSE)
#' mysa5 <- x13(myseries, myspec5)
#'
#' s_x11(mysa5)
#' s_x11(myspec5)
#'
#' myspec6 <- tramoseats_spec(spec = "RSAfull", seats.approx = "Noisy")
#' mysa6 <- tramoseats(myseries, myspec6)
#'
#' s_seats(mysa6)
#' s_seats(mysa6)
#' }
#' @export
s_estimate <- function(object = NA){
  if (!inherits(object, c("regarima","regarima_spec","SA","SA_spec")))
    stop("use only with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$estimate)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$estimate[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$estimate)
  } else {
    obj <- object$regarima$estimate[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_transform <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$transform)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$transform[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$transform)
  } else {
    obj <- object$regarima$transform[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_usrdef <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$regression$userdef$specification)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$regression$userdef$specification[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$regression$userdef$specification)
  } else {
    obj <- object$regarima$regression$userdef$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_preOut <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$regression$userdef$outliers)
  } else if (inherits(object, "regarima_spec")) {
    return(object$regression$userdef$outliers$Final)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$regression$userdef$outliers)
  } else {
    return(object$regarima$regression$userdef$outliers$Final)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_preVar <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$regression$userdef$variables)
  } else if (inherits(object, "regarima_spec")){
    return(object$regression$userdef$variables$Final)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$regression$userdef$variables)
  } else {
    return(object$regarima$regression$userdef$variables$Final)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_td <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$regression$trading.days)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$regression$trading.days[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$regression$trading.days)
  } else {
    obj <- object$regarima$regression$trading.days[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_easter <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$regression$easter)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$regression$easter[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$regression$easter)
  } else {
    obj <- object$regarima$regression$easter[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_out <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$outliers)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$outliers[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$outliers)
  } else {
    obj <- object$regarima$outliers[3,]
    rownames(obj) <- ""
    return(obj)
  }
}
#' @rdname specification
#' @name specification
#' @export
s_arima <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$arima$specification)
  } else if (inherits(object, "regarima_spec")){
    obj <- object$arima$specification[3,]
    rownames(obj) <- ""
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$arima$specification)
  } else {
    obj <- object$regarima$arima$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_arimaCoef <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$arima$coefficients)
  } else if (inherits(object, "regarima_spec")){
    return(object$arima$coefficients$Final)
  } else if (inherits(object, "SA")) {
    return(object$regarima$specification$arima$coefficients)
  } else {
    return(object$regarima$arima$coefficients$Final)
  }
}
#' @rdname specification
#' @name specification
#' @export
s_fcst <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$forecast)
  }else if (inherits(object, "regarima_spec")){
    obj <- data.frame(horizon = object$forecast[3,],row.names = c(""))
    return(obj)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$forecast)
  } else {
    obj <- data.frame(horizon = object$regarima$forecast[3,],row.names = c(""))
    return(obj)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_span <- function(object = NA){
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "X13") &&
      (
        (inherits(object, "regarima_spec") && is.null(object$estimate)) ||
        (inherits(object, "regarima") && is.null(object$specification$estimate)) ||
        (inherits(object, "SA_spec") && is.null(object$regarima$estimate)) ||
        (inherits(object, "SA") && is.null(object$regarima$specification$estimate))
      ))
    return(NULL)

  if (inherits(object, "regarima")){
    return(object$specification$span)
  } else if (inherits(object, "regarima_spec")){
    return(object$span)
  } else if (inherits(object, "SA")){
    return(object$regarima$specification$span)
  } else {
    return(object$regarima$span)
  }
}

#' @rdname specification
#' @name specification
#' @export
s_x11 <- function(object = NA){
  if (inherits(object, c("SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "SA")){
    return(object$decomposition$specification)
  } else {
    obj <- object$x11[3,]
    rownames(obj) <- ""
    return(obj)
  }
}
#' @rdname specification
#' @name specification
#' @export
s_seats <- function(object = NA){
  if (inherits(object, c("SA","SA_spec"))==FALSE)
    stop("This function must only be used with \"SA\" and \"SA_spec\" objects", call. = FALSE)

  if (inherits(object, "SA")){
    return(object$decomposition$specification)
  } else {
    obj <- object$seats[3,]
    rownames(obj) <- ""
    return(obj)
  }
}



