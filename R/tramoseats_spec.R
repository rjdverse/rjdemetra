#' Pre-specified TRAMO-SEATS model specification, SA/TRAMO-SEATS
#'
#' @description
#' .
#'
#' @inheritParams regarima_specDefTS
#' @param spec predefined JDemetra+ model specification (see Details). The default is "RSAfull".
#' @param seats.approx character, approximation mode. When the ARIMA model estimated by TRAMO does not accept an admissible decomposition, SEATS: \code{"None"} - performs an approximation; \code{"Legacy"} - replaces the model with a decomposable one; \code{"Noisy"} - estimates a new model by adding a white noise to the non-admissible model estimated by TRAMO.
#' @param seats.trendBoundary numeric, trend boundary. The boundary from which an AR root is integrated in the trend component. If the modulus of the inverse real root is greater than Trend boundary, the AR root is integrated in the trend component. Below this value the root is integrated in the transitory component.
#' @param seats.seasdBoundary numeric, seasonal boundary. Boundary from which a negative AR root is integrated in the seasonal component.
#' @param seats.seasdBoundary1 numeric, seasonal boundary (unique). Boundary from which a negative AR root is integrated in the seasonal component when the root is the unique seasonal root.
#' @param seats.seasTol numeric, seasonal tolerance. The tolerance (measured in degrees) to allocate the AR non-real roots to the seasonal component (if the modulus of the inverse complex AR root is greater than Trend boundary and the frequency of this root differs from one of the seasonal frequencies by less than Seasonal tolerance) or the transitory component (otherwise).
#' @param seats.maBoundary numeric, MA unit root boundary. When the modulus of an estimated MA root falls in the range (xl, 1), it is set to xl.
#' @param seats.method character, estimation method of the unobserved components. The choice can be made from: \code{"Burman"} (default, may result in a significant underestimation of the standard deviations of the components as it may become numerically unstable when some roots of the MA polynomial are near 1); \code{"KalmanSmoother"} (it is not disturbed by the (quasi-) unit roots in MA); \code{"McElroyMatrix"} (has the same stability issues as the Burman's algorithm).
#'
#' @details
#' .
#' @return
#' .
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @examples
#' myspec <- tramoseats_specDef(spec="RSAfull")
#'
#' @export

tramoseats_specDef <-function(spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                                 estimate.from=NA_character_,
                                 estimate.to=NA_character_,
                                 estimate.first=NA_integer_,
                                 estimate.last=NA_integer_,
                                 estimate.exclFirst=NA_integer_,
                                 estimate.exclLast=NA_integer_,
                                 estimate.tol=NA_integer_,
                                 estimate.eml=NA,
                                 estimate.urfinal=NA_integer_,
                                 transform.function=c(NA_character_,"Auto","None","Log"),
                                 transform.fct=NA_integer_,
                                 usrdef.outliersEnabled = NA,
                                 usrdef.outliersType = NA,
                                 usrdef.outliersDate = NA,
                                 usrdef.outliersCoef = NA,
                                 usrdef.varEnabled = NA,
                                 usrdef.var = NA,
                                 usrdef.varType = NA,
                                 usrdef.varCoef = NA,
                                 tradingdays.mauto=c(NA_character_,"Unused","FTest","WaldTest"),
                                 tradingdays.pftd=NA_integer_,
                                 tradingdays.option = c(NA_character_,"TradingDays","WorkingDays","None"),
                                 tradingdays.leapyear = NA,
                                 tradingdays.stocktd = NA_integer_,
                                 tradingdays.test = c(NA_character_,"Separate_T","Joint_F","None"),
                                 easter.type = c(NA_character_,"Unused","Standard","IncludeEaster","IncludeEasterMonday"),
                                 easter.julian = NA,
                                 easter.duration = NA_integer_,
                                 easter.test = NA,
                                 outlier.enabled = NA,
                                 outlier.from=NA_character_,
                                 outlier.to=NA_character_,
                                 outlier.first=NA_integer_,
                                 outlier.last=NA_integer_,
                                 outlier.exclFirst=NA_integer_,
                                 outlier.exclLast=NA_integer_,
                                 outlier.ao = NA,
                                 outlier.tc = NA,
                                 outlier.ls = NA,
                                 outlier.so = NA,
                                 outlier.usedefcv = NA,
                                 outlier.cv = NA_integer_,
                                 outlier.eml = NA,
                                 outlier.tcrate = NA_integer_,
                                 automdl.enabled = NA,
                                 automdl.acceptdefault = NA,
                                 automdl.cancel = NA_integer_,
                                 automdl.ub1 = NA_integer_,
                                 automdl.ub2 = NA_integer_,
                                 automdl.armalimit = NA_integer_,
                                 automdl.reducecv = NA_integer_,
                                 automdl.ljungboxlimit = NA_integer_,
                                 automdl.compare = NA,
                                 arima.mu = NA,
                                 arima.p = NA_integer_,
                                 arima.d = NA_integer_,
                                 arima.q = NA_integer_,
                                 arima.bp = NA_integer_,
                                 arima.bd = NA_integer_,
                                 arima.bq = NA_integer_,
                                 arima.coefEnabled = NA,
                                 arima.coef= NA,
                                 arima.coefType = NA,
                                 fcst.horizon = NA_integer_,
                                 seats.approx = c(NA_character_,"None","Legacy","Noisy"),
                                 seats.trendBoundary = NA_integer_,
                                 seats.seasdBoundary = NA_integer_,
                                 seats.seasdBoundary1 = NA_integer_,
                                 seats.seasTol = NA_integer_,
                                 seats.maBoundary = NA_integer_,
                                 seats.method = c(NA_character_,"Burman","KalmanSmoother","McElroyMatrix"))
{
  spec<-match.arg(spec)
  reg_spec=gsub("RSA","TR",spec)
  regarima <- regarima_specDefTS(reg_spec,estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,estimate.exclLast,
                                    estimate.tol,estimate.eml,estimate.urfinal,transform.function,transform.fct,
                                    usrdef.outliersEnabled,usrdef.outliersType,usrdef.outliersDate,usrdef.outliersCoef,
                                    usrdef.varEnabled,usrdef.var,usrdef.varType,usrdef.varCoef,tradingdays.mauto,tradingdays.pftd,
                                    tradingdays.option,tradingdays.leapyear,tradingdays.stocktd,tradingdays.test,
                                    easter.type,easter.julian,easter.duration,easter.test,outlier.enabled,
                                    outlier.from,outlier.to,outlier.first,outlier.last,outlier.exclFirst,
                                    outlier.exclLast,outlier.ao,outlier.tc,outlier.ls,outlier.so,outlier.usedefcv,
                                    outlier.cv,outlier.eml,outlier.tcrate,automdl.enabled,automdl.acceptdefault,
                                    automdl.cancel,automdl.ub1,automdl.ub2,automdl.armalimit,automdl.reducecv,
                                    automdl.ljungboxlimit,automdl.compare,arima.mu,arima.p,arima.d,arima.q,
                                    arima.bp,arima.bd,arima.bq,arima.coefEnabled,arima.coef,arima.coefType,fcst.horizon)

  seats <- seats_specDef(spec,seats.approx, seats.trendBoundary, seats.seasdBoundary, seats.seasdBoundary1,
                                 seats.seasTol, seats.maBoundary, seats.method)

  z <- list(regarima = regarima, seats = seats)
  class(z) <- c("SA_spec","TRAMO_SEATS")
  return(z)
}

seats_specDef<- function(spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                            seats.approx = c(NA_character_,"None","Legacy","Noisy"),
                            seats.trendBoundary = NA_integer_,
                            seats.seasdBoundary = NA_integer_,
                            seats.seasdBoundary1 = NA_integer_,
                            seats.seasTol = NA_integer_,
                            seats.maBoundary = NA_integer_,
                            seats.method = c(NA_character_,"Burman","KalmanSmoother","McElroyMatrix"))
{
  spec<-match.arg(spec)
  seats.approx <- match.arg(seats.approx)
  seats.method <- match.arg(seats.method)

  list.numeric <- list("seats.trendBoundary", "seats.seasdBoundary", "seats.seasdBoundary1",
                       "seats.seasTol", "seats.maBoundary")

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."), call. = FALSE)}

  # modifed values
  seats <- do.call(data.frame, as.list(match.call()[c(-1,-2)]))
  # create the java object
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  rspec <- specSeats_jd2r(spec = jrspec)
  seats.spec <- do.call(data.frame, rspec)
  names(seats.spec) <- paste0("seats.",names(seats.spec))
  seats.mod <- rbind(seats.spec,seats,rep(NA,length(seats.spec)))
  z <- spec_seats(seats.mod)

  class(z) <- c("seats_spec","data.frame")
  return(z)
}


#' TRAMO-SEATS model specification, SA/TRAMO-SEATS
#'
#' @description
#' .
#'
#' @inheritParams tramoseats_specDef
#' @param object model specification, object of class c("SA_spec","TRAMO_SEATS") or c("SA","TRAMO_SEATS").
#'
#' @details
#' .
#' @return
#' .
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @examples
#' myspec <- tramoseats_specDef(spec="RSAfull")
#' mysa<- tramoseats(myseries,myspec)
#' myspec1 <- tramoseats_spec(myspec, seats.approx = "Noisy")
#' mysa1 <-tramoseats(myseries,myspec1)
#' myspec2 <- tramoseats_spec(mysa, seats.approx = "Noisy")
#' mysa2 <-tramoseats(myseries,myspec2)
#'
#' @export

tramoseats_spec <-function(object,
                                 estimate.from=NA_character_,
                                 estimate.to=NA_character_,
                                 estimate.first=NA_integer_,
                                 estimate.last=NA_integer_,
                                 estimate.exclFirst=NA_integer_,
                                 estimate.exclLast=NA_integer_,
                                 estimate.tol=NA_integer_,
                                 estimate.eml=NA,
                                 estimate.urfinal=NA_integer_,
                                 transform.function=c(NA_character_,"Auto","None","Log"),
                                 transform.fct=NA_integer_,
                                 usrdef.outliersEnabled = NA,
                                 usrdef.outliersType = NA,
                                 usrdef.outliersDate = NA,
                                 usrdef.outliersCoef = NA,
                                 usrdef.varEnabled = NA,
                                 usrdef.var = NA,
                                 usrdef.varType = NA,
                                 usrdef.varCoef = NA,
                                 tradingdays.mauto=c(NA_character_,"Unused","FTest","WaldTest"),
                                 tradingdays.pftd=NA_integer_,
                                 tradingdays.option = c(NA_character_,"TradingDays","WorkingDays","None"),
                                 tradingdays.leapyear = NA,
                                 tradingdays.stocktd = NA_integer_,
                                 tradingdays.test = c(NA_character_,"Separate_T","Joint_F","None"),
                                 easter.type = c(NA_character_,"Unused","Standard","IncludeEaster","IncludeEasterMonday"),
                                 easter.julian = NA,
                                 easter.duration = NA_integer_,
                                 easter.test = NA,
                                 outlier.enabled = NA,
                                 outlier.from=NA_character_,
                                 outlier.to=NA_character_,
                                 outlier.first=NA_integer_,
                                 outlier.last=NA_integer_,
                                 outlier.exclFirst=NA_integer_,
                                 outlier.exclLast=NA_integer_,
                                 outlier.ao = NA,
                                 outlier.tc = NA,
                                 outlier.ls = NA,
                                 outlier.so = NA,
                                 outlier.usedefcv = NA,
                                 outlier.cv = NA_integer_,
                                 outlier.eml = NA,
                                 outlier.tcrate = NA_integer_,
                                 automdl.enabled = NA,
                                 automdl.acceptdefault = NA,
                                 automdl.cancel = NA_integer_,
                                 automdl.ub1 = NA_integer_,
                                 automdl.ub2 = NA_integer_,
                                 automdl.armalimit = NA_integer_,
                                 automdl.reducecv = NA_integer_,
                                 automdl.ljungboxlimit = NA_integer_,
                                 automdl.compare = NA,
                                 arima.mu = NA,
                                 arima.p = NA_integer_,
                                 arima.d = NA_integer_,
                                 arima.q = NA_integer_,
                                 arima.bp = NA_integer_,
                                 arima.bd = NA_integer_,
                                 arima.bq = NA_integer_,
                                 arima.coefEnabled = NA,
                                 arima.coef= NA,
                                 arima.coefType = NA,
                                 fcst.horizon = NA_integer_,
                                 seats.approx = c(NA_character_,"None","Legacy","Noisy"),
                                 seats.trendBoundary = NA_integer_,
                                 seats.seasdBoundary = NA_integer_,
                                 seats.seasdBoundary1 = NA_integer_,
                                 seats.seasTol = NA_integer_,
                                 seats.maBoundary = NA_integer_,
                                 seats.method = c(NA_character_,"Burman","KalmanSmoother","McElroyMatrix"))
{
  if (!inherits(object, "TRAMO_SEATS") & (inherits(object, c("SA","SA_spec"))==FALSE))
    stop("use only with c(\"SA\",\"TRAMO_SEATS\") and c(\"SA_spec\",\"TRAMO_SEATS\") objects", call. = FALSE)

  regarima <- regarima_specTS(object,estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,estimate.exclLast,
                                    estimate.tol,estimate.eml,estimate.urfinal,transform.function,transform.fct,
                                    usrdef.outliersEnabled,usrdef.outliersType,usrdef.outliersDate,usrdef.outliersCoef,
                                    usrdef.varEnabled,usrdef.var,usrdef.varType,usrdef.varCoef,tradingdays.mauto,tradingdays.pftd,
                                    tradingdays.option,tradingdays.leapyear,tradingdays.stocktd,tradingdays.test,
                                    easter.type,easter.julian,easter.duration,easter.test,outlier.enabled,
                                    outlier.from,outlier.to,outlier.first,outlier.last,outlier.exclFirst,
                                    outlier.exclLast,outlier.ao,outlier.tc,outlier.ls,outlier.so,outlier.usedefcv,
                                    outlier.cv,outlier.eml,outlier.tcrate,automdl.enabled,automdl.acceptdefault,
                                    automdl.cancel,automdl.ub1,automdl.ub2,automdl.armalimit,automdl.reducecv,
                                    automdl.ljungboxlimit,automdl.compare,arima.mu,arima.p,arima.d,arima.q,
                                    arima.bp,arima.bd,arima.bq,arima.coefEnabled,arima.coef,arima.coefType,fcst.horizon)

  seats <- seats_spec(object,seats.approx, seats.trendBoundary, seats.seasdBoundary, seats.seasdBoundary1,
                            seats.seasTol, seats.maBoundary, seats.method)

  z <- list(regarima = regarima, seats = seats)
  class(z) <- c("SA_spec","TRAMO_SEATS")
  return(z)
}

seats_spec<- function(object,
                            seats.approx = c(NA_character_,"None","Legacy","Noisy"),
                            seats.trendBoundary = NA_integer_,
                            seats.seasdBoundary = NA_integer_,
                            seats.seasdBoundary1 = NA_integer_,
                            seats.seasTol = NA_integer_,
                            seats.maBoundary = NA_integer_,
                            seats.method = c(NA_character_,"Burman","KalmanSmoother","McElroyMatrix"))
{
  seats.approx <- match.arg(seats.approx)
  seats.method <- match.arg(seats.method)

  list.numeric <- list("seats.trendBoundary", "seats.seasdBoundary", "seats.seasdBoundary1",
                       "seats.seasTol", "seats.maBoundary")

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."), call. = FALSE)}

  # modifed values
  seats <- do.call(data.frame, as.list(match.call()[c(-1,-2)]))
  seats.spec <- s_seats(object)
  seats.mod <- rbind(seats.spec,seats,rep(NA,length(seats.spec)))
  z <- spec_seats(seats.mod)

  class(z) <- c("seats_spec","data.frame")
  return(z)
}

