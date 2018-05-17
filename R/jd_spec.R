#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_estimate} access the \code{estimate} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{estimate} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_estimate(myreg1)
#'   s_estimate(myspec1)
#'
#' @export
#Functions to extract different elements of the specification file
s_estimate <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$estimate)
  }else{
    obj <- object$estimate[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_transform} access the \code{transform} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{transform} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_transform(myreg1)
#'   s_transform(myspec1)
#' @export
s_transform <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$transform)
  }else{
    obj <- object$transform[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#'   \code{s_usrdef} access the \emph{user-defined regressors} (outliers and variables) specification of the model.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{user-defined regressors} (outliers and variables) specification of the model. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_usrdef(myreg1)
#'   s_usrdef(myspec1)
#'
#' @export
s_usrdef<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$specification)
  }else{
    obj <- object$regression$userdef$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_preOut} access the \emph{pre-specified outliers} of the model.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{pre-specified outliers}. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg <- jd_regarima_defX13(myseries,spec="RG5c")
#'   myspec1<-jd_regarima_specX13(myreg,usrdef.outliersEnabled = TRUE,
#'                                usrdef.outliersType = c("LS","AO"),
#'                                usrdef.outliersDate=c("2009-10-01","2005-02-01"))
#'   myreg1 <- jd_regarima(myseries,myspec1)
#'
#'   s_preOut(myreg1)
#'   s_preOut(myspec1)
#'
#' @export
s_preOut<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$outliers)
  }else{
    return(object$regression$userdef$outliers$Final)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_preVar} access the \emph{user-defined variables} of the model.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A list with the information on the user-defined variables, including: \code{series} - the time series and \code{description} - data.frame with the variable type and coefficients. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   var1 <- ts(rnorm(length(myseries))*10,start = c(2001, 12), frequency = 12)
#'   var2 <- ts(rnorm(length(myseries))*100,start = c(2001, 12), frequency = 12)
#'   var3 <-ts(matrix(c(var1,var2), ncol=2),start = c(2001, 12), frequency = 12)
#'   myspec1 <- jd_regarima_specDefX13(spec="RG5c", usrdef.varEnabled = TRUE, usrdef.var = var3)
#'   myreg1 <- jd_regarima(myseries,myspec1)
#'
#'   s_preVar(myspec1)
#'   s_preVar(myreg1)
#'
#' @export
s_preVar<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$variables)
  }else{
    return(object$regression$userdef$variables$Final)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_td} access the \emph{trading.days} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{trading.days} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_td(myreg1)
#'   s_td(myspec1)
#'
#' @export
s_td<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$trading.days)
  }else{
    obj <- object$regression$trading.days[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_easter} access the \emph{easter} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{easter} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_easter(myreg1)
#'   s_easter(myspec1)
#'
#' @export
s_easter<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$easter)
  }else{
    obj <- object$regression$easter[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_out} access the \code{outliers} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{outliers} detection variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_out(myreg1)
#'   s_out(myspec1)
#'
#' @export
s_out<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$outliers)
  }else{
    obj <- object$outliers[3,]
    rownames(obj) <- ""
    return(obj)
  }
}
#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_arima} access the \code{arima} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{arima} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_arima(myreg1)
#'   s_arima(myspec1)
#'
#' @export
s_arima<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$arima$specification)
  }else{
    obj <- object$arima$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_arimaCoef} access the \emph{arima} part of the model specification with the user-specified ARIMA coefficients.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the user-specified ARIMA coefficients. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg <- jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg, automdl.enabled =FALSE,
#'            arima.coefEnabled = TRUE, arima.p=1,arima.q=1, arima.bp=1, arima.bq=1,
#'            arima.coef = rep(0.2,4), arima.coefType = rep("Initial",4))
#'   myreg1 <- jd_regarima(myseries,myspec1)
#'
#'   s_arimaCoef(myreg1)
#'   s_arimaCoef(myspec1)
#'
#' @export
s_arimaCoef<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$arima$coefficients)
  }else{
    return(object$arima$coefficients$Final)
  }
}
#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_fcst} access the \code{forecast} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the forecast horizon. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1,  fcst.horizon = 36)
#'
#'   s_fcst(myreg1)
#'   s_fcst(myspec1)
#'
#' @export
s_fcst <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$forecast)
  }else{
    obj <- data.frame(horizon = object$forecast[3,],row.names = c(""))
    return(obj)
  }
}

#' Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{s_span} access the \code{span} part of the model specification.
#'
#' @param object object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.
#'
#' @return
#' A data.frame with the \emph{span} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'
#'   s_span(myreg1)
#'   s_span(myspec1)
#'
#' @export
s_span<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$span)
  }else{
    return(object$span)
  }
}
