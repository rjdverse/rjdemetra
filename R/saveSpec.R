#' Saving and loading a model specification, SA and pre-adjustment in X13 and TRAMO-SEATS
#' @description
#' \code{save_spec} saves a SA or RegARIMA model specification. \code{load_spec} loads the previously saved model specification.
#'
#' @param object object of one of the classes: \code{c("SA_spec","X13")}, \code{c("SA_spec","TRAMO_SEATS")}, \code{c("SA","X13")}, \code{c("SA","TRAMO_SEATS")}, \code{c("regarima_spec","X13")}, \code{c("regarima_spec","TRAMO_SEATS")}, \code{c("regarima","X13")}, \code{c("regarima","TRAMO_SEATS")}.
#' @param file (path and) name of the file where the model specification will be saved (have been saved).
#'
#' @details
#' \code{save_spec} saves the final model specification of a \code{"SA_spec"}, \code{"SA"}, \code{"regarima_spec"} or \code{"regarima"} class object.
#' \code{load_spec} loads the previously saved model specification. It creates a \code{c("SA_spec","X13")}, \code{c("sA_spec","TRAMO_SEATS")}, \code{c("regarima_spec","X13")} or \code{c("regarima_spec","TRAMO_SEATS")} class object, in line with the class of the previously saved model specification.
#'
#' @return
#' \code{load_spec}  returns an object of class \code{"SA_spec"} or \code{"regarima_spec"}.

#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#' \donttest{
#' myseries <- ipi_c_eu[, "FR"]
#' myreg1 <- regarima_x13(myseries, spec = "RG5c")
#' myspec2 <- regarima_spec_x13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
#' myreg2 <- regarima(myseries, myspec2)
#'
#' myreg3 <- regarima_tramoseats(myseries, spec = "TRfull")
#' myspec4 <-regarima_spec_tramoseats(myreg3, tradingdays.mauto = "Unused",
#'                                   tradingdays.option ="WorkingDays",
#'                                   easter.type = "Standard",
#'                                   automdl.enabled = FALSE, arima.mu = TRUE)
#' myreg4 <-regarima(myseries, myspec4)
#'
#' myspec6 <- x13_spec("RSA5c")
#' mysa6 <- x13(myseries, myspec6)
#'
#' myspec7 <- tramoseats_spec("RSAfull")
#' mysa7 <- tramoseats(myseries, myspec7)
#'
#' dir <- tempdir()
#'
#'  # Save the model specification from a c("regarima_spec","X13") class object
#' save_spec(myspec2, file.path(dir, "specx13.RData"))
#'  # Save the model specification from a c("regarima","X13") class object
#' save_spec(myreg2, file.path(dir,"regx13.RData"))
#'  # Save the model specification from a c("regarima_spec","TRAMO_SEATS") class object
#' save_spec(myspec4, file.path(dir,"specTS.RData"))
#'  # Save the model specification from a c("regarima","TRAMO_SEATS") class object
#' save_spec(myreg4, file.path(dir,"regTS.RData"))
#'  # Save model from a c("SA_spec","X13") class object
#' save_spec(myspec6, file.path(dir,"specFullx13.RData"))
#'  # Save model from a c("SA","X13") class object
#' save_spec(mysa6, file.path(dir,"sax13.RData"))
#'  # Save model from a c("SA_spec","TRAMO_SEATS") class object
#' save_spec(myspec7, file.path(dir,"specFullTS.RData"))
#'  # Save model from a c("SA","TRAMO_SEATS") class object
#' save_spec(mysa7, file.path(dir,"saTS.RData"))
#'
#'  # Load the model specification
#' myspec2a <- load_spec(file.path(dir,"specx13.RData"))
#' myspec2b <- load_spec(file.path(dir,"regx13.RData"))
#' myspec4a <- load_spec(file.path(dir,"specTS.RData"))
#' myspec4b <- load_spec(file.path(dir,"regTS.RData"))
#' myspec6a <- load_spec(file.path(dir,"specFullx13.RData"))
#' myspec6b <- load_spec(file.path(dir,"sax13.RData"))
#' myspec7a <- load_spec(file.path(dir,"specFullTS.RData"))
#' myspec7b <- load_spec(file.path(dir,"saTS.RData"))
#'
#'
#' regarima(myseries, myspec2a)
#' x13(myseries, myspec6a)
#' tramoseats(myseries, myspec7a)
#'
#' regarima(myseries, myspec4a)
#' x13(myseries, myspec6b)
#' tramoseats(myseries, myspec7b)
#' }
#' @export
save_spec = function (object, file = file.path(tempdir(), "spec.RData")) {
  if (inherits(object, c("regarima","regarima_spec","SA","SA_spec"))==FALSE)
    stop("use only with \"regarima\", \"regarima_spec\", \"SA\" and \"SA_spec\" objects", call. = FALSE)

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

  if (inherits(object,c("SA","SA_spec")) & inherits(object,"X13")){
    decomp <- s_x11(object)
    cspec <- "SA_saveX13"
  } else if (inherits(object,c("SA","SA_spec")) & inherits(object,"TRAMO_SEATS")){
    decomp <- s_seats(object)
    cspec <- "SA_saveTS"
  } else if (inherits(object,"X13")) {
    decomp <- NA
    cspec <- "regarima_saveX13"
  } else {
    decomp <- NA
    cspec <- "regarima_saveTS"
  }

  spec <- list(estimate=estimate, transform=transform, usrdef = usrdef,predef.outliers=predef.outliers,
               predef.variables=predef.variables, trading.days=trading.days,easter= easter,
               outliers=outliers, arima.dsc=arima.dsc, predef.coef=predef.coef,
               forecast = forecast,span=span, decomp=decomp)
  class(spec) <- cspec
  save(spec, file = file)
}
# Generic function to load previously saved model specification
#' @rdname save_spec
#' @name save_spec
#' @export
load_spec <- function (file = "spec.RData") {
  object <- get(load(file = file))
  if (inherits(object,c("SA_saveX13","SA_saveTS","regarima_saveX13","regarima_saveTS"))==FALSE)
    stop("no model specification found in the file!\n")

  s.estimate <- object$estimate
  s.transform <- object$transform
  s.usrdef <- object$usrdef
  s.predef.outliers <- object$predef.outliers
  s.predef.variables <- object$predef.variables
  s.trading.days <- object$trading.days
  s.easter <- object$easter
  s.outliers <- object$outliers
  s.arima.dsc <- object$arima.dsc
  s.predef.coef <- object$predef.coef
  s.forecast <- object$forecast
  span <- object$span
  s.decomp <- object$decomp

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

  regarima <- list(estimate=estimate, transform=transform, regression=regression,
               outliers=outliers, arima=arima, forecast = forecast , span=span)

  if (inherits(object,"regarima_saveX13")){
    class(regarima) <- c("regarima_spec","X13")
    return(regarima)
  } else if (inherits(object,"regarima_saveTS")){
    class(regarima) <- c("regarima_spec","TRAMO_SEATS")
    return(regarima)
  } else if (inherits(object,"SA_saveX13")){
    class(regarima) <- c("regarima_spec","X13")
    x11 <- rbind(s.decomp,rep(NA,length(s.decomp )),s.decomp)
    rownames(x11)=c("Loaded","User_modif","Final")
    class(x11) <- c("X11_spec","data.frame")
    z <- list(regarima = regarima, x11 = x11)
    class(z) <- c("SA_spec","X13")
    return(z)
  } else {
    class(regarima) <- c("regarima_spec","TRAMO_SEATS")
    seats <- rbind(s.decomp,rep(NA,length(s.decomp )),s.decomp)
    rownames(seats)=c("Loaded","User_modif","Final")
    class(seats) <- c("seats_spec","data.frame")
    z <- list(regarima = regarima, seats = seats)
    class(z) <- c("SA_spec","TRAMO_SEATS")
    return(z)
  }
}
