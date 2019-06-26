#' TRAMO-SEATS model specification, SA/TRAMO-SEATS
#'
#' @description
#'
#' Function to create (and/or modify) a \code{c("SA_spec", "TRAMO_SEATS")} class object with the SA model specification for the TRAMO-SEATS method. It can be done from a pre-defined 'JDemetra+' model specification (a \code{character}), a previous specification (\code{c("SA_spec", "TRAMO_SEATS")} object) or a seasonal adjustment model (\code{c("SA", "TRAMO_SEATS")} object).
#'
#' @param spec model specification X13.  It can be a \code{character} of predefined TRAMO-SEATS 'JDemetra+' model specification (see \emph{Details}), an object of class \code{c("SA_spec","TRAMO_SEATS")} or an object of class \code{c("SA", "TRAMO_SEATS")}. The default is \code{"RSAfull"}.
#'
#' The time span of the series to be used for the estimation of the RegArima model coefficients (default from 1900-01-01 to 2020-12-31) is controlled by the following six variables: \code{estimate.from, estimate.to, estimate.first, estimate.last, estimate.exclFirst} and \code{estimate.exclLast}; where \code{estimate.from} and \code{estimate.to} have priority over remaining span control variables, \code{estimate.last} and \code{estimate.first} have priority over \code{estimate.exclFirst} and \code{estimate.exclLast}, and \code{estimate.last} has priority over \code{estimate.first}.
#'
#' @inheritParams  regarima_spec_tramoseats
#' @param seats.approx character, approximation mode. When the ARIMA model estimated by TRAMO does not accept an admissible decomposition, SEATS: \code{"None"} - performs an approximation; \code{"Legacy"} - replaces the model with a decomposable one; \code{"Noisy"} - estimates a new model by adding a white noise to the non-admissible model estimated by TRAMO.
#' @param seats.trendBoundary numeric, trend boundary. The boundary from which an AR root is integrated in the trend component. If the modulus of the inverse real root is greater than Trend boundary, the AR root is integrated in the trend component. Below this value the root is integrated in the transitory component.
#' @param seats.seasdBoundary numeric, seasonal boundary. Boundary from which a negative AR root is integrated in the seasonal component.
#' @param seats.seasdBoundary1 numeric, seasonal boundary (unique). Boundary from which a negative AR root is integrated in the seasonal component when the root is the unique seasonal root.
#' @param seats.seasTol numeric, seasonal tolerance. The tolerance (measured in degrees) to allocate the AR non-real roots to the seasonal component (if the modulus of the inverse complex AR root is greater than Trend boundary and the frequency of this root differs from one of the seasonal frequencies by less than Seasonal tolerance) or the transitory component (otherwise).
#' @param seats.maBoundary numeric, MA unit root boundary. When the modulus of an estimated MA root falls in the range (xl, 1), it is set to xl.
#' @param seats.method character, estimation method of the unobserved components. The choice can be made from: \code{"Burman"} (default, may result in a significant underestimation of the standard deviations of the components as it may become numerically unstable when some roots of the MA polynomial are near 1); \code{"KalmanSmoother"} (it is not disturbed by the (quasi-) unit roots in MA); \code{"McElroyMatrix"} (has the same stability issues as the Burman's algorithm).
#'
#' @details
#'
#' The available predefined 'JDemetra+' model specifications are described in the table below.
#'
#' \tabular{rrrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RSA0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5 |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
#' RSAfull |\tab automatic |\tab AO/LS/TC |\tab automatic |\tab automatic
#' }
#' @return
#'
#' A two-elements list of class \code{c("SA_spec", "TRAMO_SEATS")}: (1) object of class \code{c("regarima_spec", "TRAMO_SEATS")} with the RegARIMA model specification, (2) object of class \code{c("seats_spec", "data.frame")} with the SEATS algorithm specification.
#' Each component refers to different part of the SA model specification, mirroring the arguments of the function (for details see arguments description).
#' Each of the lowest-level component (except span, pre-specified outliers, user-defined variables and pre-specified ARMA coefficients) is structured within a data frame with columns denoting different variables of the model specification and rows referring to: first row - base specification, as provided within the argument \code{spec}; second row - user modifications as specified by the remaining arguments of the function (e.g.: \code{arima.d}); and third row - final model specification.
#' The final specification (third row) shall include user modifications (row two) unless they were wrongly specified. The pre-specified outliers, user-defined variables and pre-specified ARMA coefficients consist of a list with the \code{Predefined} (base model specification) and \code{Final} values.
#'
#' \item{regarima}{object of class \code{c("regarima_spec", "TRAMO_SEATS")}. See \emph{Value} of the function \code{\link{regarima_spec_tramoseats}}}
#'
#' \item{seats}{data.frame of class \code{c("seats_spec", "data.frame")}, containing the \emph{seats} variables in line with the names of the arguments variables. The final values can be also accessed with the function \code{\link{s_seats}}.}
#'
#' @references
#' Info on 'JDemetra+', usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @seealso \code{\link{tramoseats}}
#'
#' @examples\donttest{
#' myseries <- ipi_c_eu[, "FR"]
#' myspec1 <- tramoseats_spec(spec = c("RSAfull"))
#' mysa1 <- tramoseats(myseries, spec = myspec1)
#'
#' # Modify a pre-specified model specification
#' myspec2 <- tramoseats_spec(spec = "RSAfull", tradingdays.mauto = "Unused",
#'                            tradingdays.option = "WorkingDays",
#'                            easter.type = "Standard",
#'                            automdl.enabled = FALSE, arima.mu = TRUE)
#' mysa2 <- tramoseats(myseries, spec = myspec2)
#'
#' # Modify the model specification from a "SA" object
#' myspec3 <- tramoseats_spec(mysa1, tradingdays.mauto = "Unused",
#'                            tradingdays.option = "WorkingDays",
#'                            easter.type = "Standard", automdl.enabled = FALSE, arima.mu = TRUE)
#' mysa3 <- tramoseats(myseries, myspec3)
#'
#' # Modify the model specification from a "SA_spec" object
#' myspec4 <- tramoseats_spec(myspec1, tradingdays.mauto = "Unused",
#'                            tradingdays.option = "WorkingDays",
#'                            easter.type = "Standard", automdl.enabled = FALSE, arima.mu = TRUE)
#' mysa4 <- tramoseats(myseries, myspec4)
#'
#' # Pre-specified outliers
#' myspec5 <- tramoseats_spec(spec = "RSAfull",
#'                            usrdef.outliersEnabled = TRUE,
#'                            usrdef.outliersType = c("LS", "LS"),
#'                            usrdef.outliersDate = c("2008-10-01", "2003-01-01"),
#'                            usrdef.outliersCoef = c(10,-8), transform.function = "None")
#' s_preOut(myspec5)
#' mysa5 <- tramoseats(myseries, myspec5)
#' mysa5
#' s_preOut(mysa5)
#'
#' # User-defined calendar regressors
#' var1 <- ts(rnorm(length(myseries))*10, start = start(myseries), frequency = 12)
#' var2 <- ts(rnorm(length(myseries))*100, start = start(myseries), frequency = 12)
#' var<- ts.union(var1, var2)
#'
#' myspec6 <- tramoseats_spec(spec = "RSAfull", tradingdays.option = "UserDefined",
#'                            usrdef.varEnabled = TRUE, usrdef.var = var,
#'                            usrdef.varType = c("Calendar", "Calendar"))
#' s_preVar(myspec6)
#' mysa6 <- tramoseats(myseries, myspec6)
#'
#' myspec7 <- tramoseats_spec(spec = "RSAfull", usrdef.varEnabled = TRUE,
#'                            usrdef.var = var, usrdef.varCoef = c(17,-1),
#'                            transform.function = "None")
#' mysa7 <- tramoseats(myseries, myspec7)
#'
#' # Pre-specified ARMA coefficients
#' myspec8 <- tramoseats_spec(spec = "RSAfull",
#'                            arima.coefEnabled = TRUE, automdl.enabled = FALSE,
#'                            arima.p = 2, arima.q = 0,
#'                            arima.bp = 1, arima.bq = 1,
#'                            arima.coef = c(-0.12, -0.12, -0.3, -0.99),
#'                            arima.coefType = rep("Fixed", 4))
#' mysa8 <- tramoseats(myseries, myspec8)
#' mysa8
#' s_arimaCoef(myspec8)
#' s_arimaCoef(mysa8)
#' }
#' @export
tramoseats_spec <- function(spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                            preliminary.check = NA,
                     estimate.from = NA_character_,
                     estimate.to = NA_character_,
                     estimate.first = NA_integer_,
                     estimate.last = NA_integer_,
                     estimate.exclFirst = NA_integer_,
                     estimate.exclLast = NA_integer_,
                     estimate.tol = NA_integer_,
                     estimate.eml = NA,
                     estimate.urfinal = NA_integer_,
                     transform.function = c(NA, "Auto", "None", "Log"),
                     transform.fct = NA_integer_,
                     usrdef.outliersEnabled = NA,
                     usrdef.outliersType = NA,
                     usrdef.outliersDate = NA,
                     usrdef.outliersCoef = NA,
                     usrdef.varEnabled = NA,
                     usrdef.var = NA,
                     usrdef.varType = NA,
                     usrdef.varCoef = NA,
                     tradingdays.mauto = c(NA, "Unused", "FTest", "WaldTest"),
                     tradingdays.pftd = NA_integer_,
                     tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                     tradingdays.leapyear = NA,
                     tradingdays.stocktd = NA_integer_,
                     tradingdays.test = c(NA, "Separate_T", "Joint_F", "None"),
                     easter.type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
                     easter.julian = NA,
                     easter.duration = NA_integer_,
                     easter.test = NA,
                     outlier.enabled = NA,
                     outlier.from = NA_character_,
                     outlier.to = NA_character_,
                     outlier.first = NA_integer_,
                     outlier.last = NA_integer_,
                     outlier.exclFirst = NA_integer_,
                     outlier.exclLast = NA_integer_,
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
                     seats.approx = c(NA, "None", "Legacy", "Noisy"),
                     seats.trendBoundary = NA_integer_,
                     seats.seasdBoundary = NA_integer_,
                     seats.seasdBoundary1 = NA_integer_,
                     seats.seasTol = NA_integer_,
                     seats.maBoundary = NA_integer_,
                     seats.method = c(NA, "Burman", "KalmanSmoother", "McElroyMatrix")
                     ){
  UseMethod("tramoseats_spec", spec)
}
#' @export
tramoseats_spec.character <- function(spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                                      preliminary.check = NA,
                                estimate.from = NA_character_,
                                estimate.to = NA_character_,
                                estimate.first = NA_integer_,
                                estimate.last = NA_integer_,
                                estimate.exclFirst = NA_integer_,
                                estimate.exclLast = NA_integer_,
                                estimate.tol = NA_integer_,
                                estimate.eml = NA,
                                estimate.urfinal = NA_integer_,
                                transform.function = c(NA, "Auto", "None", "Log"),
                                transform.fct = NA_integer_,
                                usrdef.outliersEnabled = NA,
                                usrdef.outliersType = NA,
                                usrdef.outliersDate = NA,
                                usrdef.outliersCoef = NA,
                                usrdef.varEnabled = NA,
                                usrdef.var = NA,
                                usrdef.varType = NA,
                                usrdef.varCoef = NA,
                                tradingdays.mauto = c(NA, "Unused", "FTest", "WaldTest"),
                                tradingdays.pftd = NA_integer_,
                                tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                                tradingdays.leapyear = NA,
                                tradingdays.stocktd = NA_integer_,
                                tradingdays.test = c(NA, "Separate_T", "Joint_F", "None"),
                                easter.type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
                                easter.julian = NA,
                                easter.duration = NA_integer_,
                                easter.test = NA,
                                outlier.enabled = NA,
                                outlier.from = NA_character_,
                                outlier.to = NA_character_,
                                outlier.first = NA_integer_,
                                outlier.last = NA_integer_,
                                outlier.exclFirst = NA_integer_,
                                outlier.exclLast = NA_integer_,
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
                                seats.approx = c(NA, "None", "Legacy", "Noisy"),
                                seats.trendBoundary = NA_integer_,
                                seats.seasdBoundary = NA_integer_,
                                seats.seasdBoundary1 = NA_integer_,
                                seats.seasTol = NA_integer_,
                                seats.maBoundary = NA_integer_,
                                seats.method = c(NA, "Burman", "KalmanSmoother", "McElroyMatrix"))
{
  spec <- match.arg(spec)
  reg_spec <- gsub("RSA", "TR", spec)
  regarima <-  regarima_spec_tramoseats(reg_spec,preliminary.check, estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,estimate.exclLast,
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

  seats <- seats_spec_def(spec,seats.approx, seats.trendBoundary, seats.seasdBoundary, seats.seasdBoundary1,
                          seats.seasTol, seats.maBoundary, seats.method)

  z <- list(regarima = regarima, seats = seats)
  class(z) <- c("SA_spec","TRAMO_SEATS")
  return(z)
}

seats_spec_def <- function(spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                          seats.approx = c(NA_character_,"None","Legacy","Noisy"),
                          seats.trendBoundary = NA_integer_,
                          seats.seasdBoundary = NA_integer_,
                          seats.seasdBoundary1 = NA_integer_,
                          seats.seasTol = NA_integer_,
                          seats.maBoundary = NA_integer_,
                          seats.method = c(NA_character_,"Burman","KalmanSmoother","McElroyMatrix"))
{
  spec <- match.arg(spec)
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
#' @export
tramoseats_spec.TRAMO_SEATS <- function(spec,
                            preliminary.check = NA,
                            estimate.from = NA_character_,
                            estimate.to = NA_character_,
                            estimate.first = NA_integer_,
                            estimate.last = NA_integer_,
                            estimate.exclFirst = NA_integer_,
                            estimate.exclLast = NA_integer_,
                            estimate.tol = NA_integer_,
                            estimate.eml = NA,
                            estimate.urfinal = NA_integer_,
                            transform.function = c(NA, "Auto", "None", "Log"),
                            transform.fct = NA_integer_,
                            usrdef.outliersEnabled = NA,
                            usrdef.outliersType = NA,
                            usrdef.outliersDate = NA,
                            usrdef.outliersCoef = NA,
                            usrdef.varEnabled = NA,
                            usrdef.var = NA,
                            usrdef.varType = NA,
                            usrdef.varCoef = NA,
                            tradingdays.mauto = c(NA, "Unused", "FTest", "WaldTest"),
                            tradingdays.pftd = NA_integer_,
                            tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                            tradingdays.leapyear = NA,
                            tradingdays.stocktd = NA_integer_,
                            tradingdays.test = c(NA, "Separate_T", "Joint_F", "None"),
                            easter.type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
                            easter.julian = NA,
                            easter.duration = NA_integer_,
                            easter.test = NA,
                            outlier.enabled = NA,
                            outlier.from = NA_character_,
                            outlier.to = NA_character_,
                            outlier.first = NA_integer_,
                            outlier.last = NA_integer_,
                            outlier.exclFirst = NA_integer_,
                            outlier.exclLast = NA_integer_,
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
                            seats.approx = c(NA, "None", "Legacy", "Noisy"),
                            seats.trendBoundary = NA_integer_,
                            seats.seasdBoundary = NA_integer_,
                            seats.seasdBoundary1 = NA_integer_,
                            seats.seasTol = NA_integer_,
                            seats.maBoundary = NA_integer_,
                            seats.method = c(NA, "Burman", "KalmanSmoother", "McElroyMatrix"))
{
  if ( !inherits(spec, c("SA","SA_spec")))
    stop("use only with c(\"SA\",\"TRAMO_SEATS\") and c(\"SA_spec\",\"TRAMO_SEATS\") objects", call. = FALSE)

  regarima <- regarima_spec_tramoseats(spec,preliminary.check, estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,estimate.exclLast,
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

  seats <- seats_spec(spec,seats.approx, seats.trendBoundary, seats.seasdBoundary, seats.seasdBoundary1,
                            seats.seasTol, seats.maBoundary, seats.method)

  z <- list(regarima = regarima, seats = seats)
  class(z) <- c("SA_spec","TRAMO_SEATS")
  return(z)
}

seats_spec<- function(spec,
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
  seats.spec <- s_seats(spec)
  seats.mod <- rbind(seats.spec,seats,rep(NA,length(seats.spec)))
  z <- spec_seats(seats.mod)

  class(z) <- c("seats_spec","data.frame")
  return(z)
}

