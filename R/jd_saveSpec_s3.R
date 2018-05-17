#' Saving a RegARIMA model specification, pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{jd_saveSpec} saves a RegARIMA model specification.
#'
#' @param object object of one of the classes: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} and \code{c("JD_RegArima","TRAMO_SEATS")}.
#' @param file (path and) name of the file where the model specification will be saved.
#'
#' @details
#' \code{jd_saveSpec} saves the model specification of a \code{"JD_RegArima_Spec"} or \code{"JD_RegArima"} class object. In case of the \code{"JD_RegArima_Spec"} class object only the final values are saved (for details see the values of the function \code{\link{jd_regarima_specX13}} and \code{\link{jd_regarima_specTS}}).
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec2 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'   myreg2 <-jd_regarima(myseries,myspec2)
#'
#'   myreg3 <-jd_regarima_defTS(myseries, spec=c("TRfull"))
#'   myspec4<-jd_regarima_specTS(myreg3,tradingdays.mauto ="Unused",
#'                               tradingdays.option = "WorkingDays",
#'                               easter.type = "Standard", automdl.enabled = FALSE, arima.mu = TRUE)
#'   myreg4 <-jd_regarima(myseries,myspec4)
#'
#'   # Save the model specification from a c("JD_RegArima_Spec","X13") class object
#'   jd_saveSpec(myspec2,"specx13.RData")
#'   # Save the model specification from a c("JD_RegArima","X13") class object
#'   jd_saveSpec(myreg2,"regx13.RData")
#'   # Save the model specification from a c("JD_RegArima_Spec","TRAMO_SEATS") class object
#'   jd_saveSpec(myspec4,"specTS.RData")
#'   # Save the model specification from a c("JD_RegArima","TRAMO_SEATS") class object
#'   jd_saveSpec(myreg4,"regTS.RData")
#' @export
# Generic function to save model specification (RegARIMA and SA)
jd_saveSpec = function (object, file = "JD_Spec.RData") {
  if (!inherits(object, "X13") & !inherits(object, "TRAMO_SEATS")) {
    stop("use only with \"X13\" and \"TRAMO_SEATS\") class objects", call. = FALSE)
  }else{
  UseMethod("jd_saveSpec", object)
  }
}
#' @export
# Method: JD_RegArima_Spec
jd_saveSpec.JD_RegArima_Spec = function(object, file){
  if (!inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima_Spec\" class object")

  estimate <- s_estimate(object)
  transform <- s_transform(object)
  usrdef <- s_usrdef(object)
  predef.outliers <- s_preOut(object)
  predef.variables <- s_preVar(object)
  trading.days <- s_td(object)
  easter <- s_easter(object)
  outliers <- s_out(object)
  arima.dsc <- s_arima(object)
  predef.coef <- s_arimaCoef(object)
  forecast <- s_fcst(object)
  span <- s_span(object)
  userdef <-list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
  regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = predef.coef)

  spec <- list(estimate=estimate, transform=transform, regression=regression,
                        outliers=outliers, arima=arima, forecast = forecast ,span=span)

  class(spec) <- if (inherits(object, "X13")) {"regarima_saveSpecX13"} else {"regarima_saveSpecTS"}
  save(spec, file = file)
}
#' @export
# Method: JD_RegArima
jd_saveSpec.JD_RegArima = function(object, file){
  if (!inherits(object, "JD_RegArima"))
    stop("use only with \"JD_RegArima\" class object")

  spec = object$specification
  class(spec) <- if (inherits(object, "X13")) {"regarima_saveSpecX13"} else {"regarima_saveSpecTS"}
  save(spec, file = file)
}

#' Loading a RegARIMA model specification, pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{jd_loadSpec} loads a previously saved RegARIMA model specification.
#'
#' @param file (path and) name of the file where the model specification was saved.
#'
#' @details
#' \code{jd_loadSpec} loads a RegARIMA model specification that was previously saved using the function \code{\link{jd_saveSpec}}. It creates a \code{c("JD_RegArima_Spec","X13")} or \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class object, depending on the sub-class of the previously saved model specification.
#'
#' @return
#' A list of class \code{"JD_RegArima_Spec"}. For details see Value of the function \code{\link{jd_regarima_specX13}} and \code{\link{jd_regarima_specTS}}.
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#'   myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myspec2 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#'   myreg2 <-jd_regarima(myseries,myspec2)
#'
#'   myreg3 <-jd_regarima_defTS(myseries, spec=c("TRfull"))
#'   myspec4 <-jd_regarima_specTS(myreg3,tradingdays.mauto ="Unused",tradingdays.option ="WorkingDays",
#'                                easter.type = "Standard", automdl.enabled = FALSE, arima.mu = TRUE)
#'   myreg4 <-jd_regarima(myseries,myspec4)
#'
#'   # Save the model specification from a c("JD_RegArima_Spec","X13") class object
#'   jd_saveSpec(myspec2,"specx13.RData")
#'   # Save the model specification from a c("JD_RegArima","X13") class object
#'   jd_saveSpec(myreg2,"regx13.RData")
#'   # Save the model specification from a c("JD_RegArima_Spec","TRAMO_SEATS") class object
#'   jd_saveSpec(myspec4,"specTS.RData")
#'   # Save the model specification from a c("JD_RegArima","TRAMO_SEATS") class object
#'   jd_saveSpec(myreg4,"regTS.RData")
#'
#'   # Load the model specification
#'   # c("JD_RegArima_Spec","X13") class object
#'   myspec2a <- jd_loadSpec("specx13.RData")
#'   myspec2b <- jd_loadSpec("regx13.RData")
#'   # c("JD_RegArima_Spec","TRAMO_SEATS") class object
#'   myspec4a <- jd_loadSpec("specTS.RData")
#'   myspec4b <- jd_loadSpec("regTS.RData")
#'
#'   jd_regarima(myseries,myspec2a)
#'   jd_regarima(myseries,myspec4a)
#' @export
# Generic function to load previously saved model specification
jd_loadSpec <- function (file = "JD_Spec.RData") {
  object <- get(load(file = file))
  if (("regarima_saveSpecX13" != class(object)) & ("regarima_saveSpecTS" != class(object)))
    stop("no model specification found in the file!\n")

  s.estimate <- object$estimate
  s.transform <- object$transform
  s.usrdef <- object$regression$userdef$specification
  s.predef.outliers <- object$regression$userdef$outliers
  s.predef.variables <- object$regression$userdef$variables
  s.trading.days <- object$regression$trading.days
  s.easter <- object$regression$easter
  s.outliers <- object$outliers
  s.arima.dsc <- object$arima$specification
  s.predef.coef <- object$arima$coefficients
  s.forecast <- object$forecast
  span <- object$span

  estimate<- rbind(s.estimate,rep(NA,length(s.estimate)),s.estimate)
  transform <- rbind(s.transform,rep(NA,length(s.transform)),s.transform)
  usrdef <- rbind(s.usrdef,rep(NA,length(s.usrdef)),s.usrdef)
  trading.days <- rbind(s.trading.days,rep(NA,length(s.trading.days)),s.trading.days)
  easter <- rbind(s.easter,rep(NA,length(s.easter)),s.easter)
  outliers <- rbind(s.outliers,rep(NA,length(s.outliers)),s.outliers)
  arima.dsc <- rbind(s.arima.dsc,rep(NA,length(s.arima.dsc)),s.arima.dsc)
  forecast <- rbind(s.forecast,rep(NA,length(s.estimate)),s.forecast)

  rownames(estimate)=c("Loaded","User_modif","Final")
  rownames(transform)=c("Loaded","User_modif","Final")
  rownames(usrdef)=c("Loaded","User_modif","Final")
  rownames(trading.days)=c("Loaded","User_modif","Final")
  rownames(easter)=c("Loaded","User_modif","Final")
  rownames(outliers)=c("Loaded","User_modif","Final")
  rownames(arima.dsc)=c("Loaded","User_modif","Final")
  rownames(forecast)=c("Loaded","User_modif","Final")

  userdef <-list(specification = usrdef, outliers = list(Predefined = s.predef.outliers, Final = s.predef.outliers),
                 variables = list(Predefined = s.predef.variables, Final = s.predef.variables))
  regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = list(Predefined = s.predef.coef, Final = s.predef.coef))

  spec <- list(estimate=estimate, transform=transform, regression=regression,
               outliers=outliers, arima=arima, forecast = forecast , span=span)

  class(spec) <- if (class(object)== "regarima_saveSpecX13") {c("JD_RegArima_Spec","X13")} else {c("JD_RegArima_Spec","TRAMO_SEATS")}
  return(spec)
}
