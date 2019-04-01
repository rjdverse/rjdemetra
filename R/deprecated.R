#' Deprecated functions
#'
#' @description
#' Use \code{\link{x13_spec}} instead of \code{x13_spec_def}.
#'
#' @inheritParams x13_spec
#' @inheritParams tramoseats_spec
#' @inheritParams x13
#' @param  spec model specification
#' @export
#' @name deprecated-RJDemetra
x13_spec_def <- function(spec = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c","X11"),
                         estimate.from = NA_character_,
                         estimate.to = NA_character_,
                         estimate.first = NA_integer_,
                         estimate.last = NA_integer_,
                         estimate.exclFirst = NA_integer_,
                         estimate.exclLast = NA_integer_,
                         estimate.tol = NA_integer_,
                         transform.function = c(NA, "Auto", "None", "Log"),
                         transform.adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                         transform.aicdiff = NA_integer_,
                         usrdef.outliersEnabled = NA,
                         usrdef.outliersType = NA,
                         usrdef.outliersDate = NA,
                         usrdef.outliersCoef = NA,
                         usrdef.varEnabled = NA,
                         usrdef.var = NA,
                         usrdef.varType = NA,
                         usrdef.varCoef = NA,
                         tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                         tradingdays.autoadjust = NA,
                         tradingdays.leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                         tradingdays.stocktd = NA_integer_,
                         tradingdays.test = c(NA, "Remove", "Add", "None"),
                         easter.enabled = NA,
                         easter.julian = NA,
                         easter.duration = NA_integer_,
                         easter.test = c(NA, "Add", "Remove", "None"),
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
                         outlier.method = c(NA, "AddOne", "AddAll"),
                         outlier.tcrate = NA_integer_,
                         automdl.enabled = NA,
                         automdl.acceptdefault = NA,
                         automdl.cancel = NA_integer_,
                         automdl.ub1 = NA_integer_,
                         automdl.ub2 = NA_integer_,
                         automdl.mixed = NA,
                         automdl.balanced = NA,
                         automdl.armalimit = NA_integer_,
                         automdl.reducecv = NA_integer_,
                         automdl.ljungboxlimit = NA_integer_,
                         automdl.ubfinal = NA_integer_,
                         arima.mu = NA,
                         arima.p = NA_integer_,
                         arima.d = NA_integer_,
                         arima.q = NA_integer_,
                         arima.bp = NA_integer_,
                         arima.bd = NA_integer_,
                         arima.bq = NA_integer_,
                         arima.coefEnabled = NA,
                         arima.coef = NA,
                         arima.coefType = NA,
                         fcst.horizon = NA_integer_,
                         x11.mode = c(NA, "Undefined", "Additive", "Multiplicative", "LogAdditive", "PseudoAdditive"),
                         x11.seasonalComp = NA,
                         x11.lsigma = NA_integer_,
                         x11.usigma = NA_integer_,
                         x11.trendAuto = NA,
                         x11.trendma = NA_integer_,
                         x11.seasonalma = NA_character_,
                         x11.fcasts = NA_integer_,
                         x11.bcasts = NA_integer_,
                         x11.excludeFcasts = NA)
{
  .Deprecated("x13_spec")
  x13_spec(spec , NA,
           estimate.from ,
           estimate.to ,
           estimate.first ,
           estimate.last ,
           estimate.exclFirst ,
           estimate.exclLast ,
           estimate.tol ,
           transform.function ,
           transform.adjust ,
           transform.aicdiff ,
           usrdef.outliersEnabled ,
           usrdef.outliersType ,
           usrdef.outliersDate ,
           usrdef.outliersCoef ,
           usrdef.varEnabled ,
           usrdef.var ,
           usrdef.varType ,
           usrdef.varCoef ,
           tradingdays.option ,
           tradingdays.autoadjust ,
           tradingdays.leapyear ,
           tradingdays.stocktd ,
           tradingdays.test ,
           easter.enabled ,
           easter.julian ,
           easter.duration ,
           easter.test ,
           outlier.enabled ,
           outlier.from ,
           outlier.to ,
           outlier.first ,
           outlier.last ,
           outlier.exclFirst ,
           outlier.exclLast ,
           outlier.ao ,
           outlier.tc ,
           outlier.ls ,
           outlier.so ,
           outlier.usedefcv ,
           outlier.cv ,
           outlier.method ,
           outlier.tcrate ,
           automdl.enabled ,
           automdl.acceptdefault ,
           automdl.cancel ,
           automdl.ub1 ,
           automdl.ub2 ,
           automdl.mixed ,
           automdl.balanced ,
           automdl.armalimit ,
           automdl.reducecv ,
           automdl.ljungboxlimit ,
           automdl.ubfinal ,
           arima.mu ,
           arima.p ,
           arima.d ,
           arima.q ,
           arima.bp ,
           arima.bd ,
           arima.bq ,
           arima.coefEnabled ,
           arima.coef ,
           arima.coefType ,
           fcst.horizon ,
           x11.mode ,
           x11.seasonalComp ,
           x11.lsigma ,
           x11.usigma ,
           x11.trendAuto ,
           x11.trendma ,
           x11.seasonalma ,
           x11.fcasts ,
           x11.bcasts ,
           x11.excludeFcasts )
}
#' @description
#' Use \code{\link{x13}} instead of \code{x13_def}.
#'
#' @export
#' @name deprecated-RJDemetra
x13_def <- function(series, spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c"),
                    userdefined = NULL){
  .Deprecated("x13")
  if (!is.ts(series)) {
    stop("series must be a time series")
  }
  spec <- match.arg(spec)
  # create the java objects
  jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)

  return(x13JavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))
}

#' @description
#' Use \code{\link{tramoseats_spec}} instead of \code{tramoseats_spec_def}.
#'
#' @export
#' @name deprecated-RJDemetra
tramoseats_spec_def <- function(spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
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
  .Deprecated("tramoseats_spec")
  tramoseats_spec(spec ,NA,
                  estimate.from ,
                  estimate.to ,
                  estimate.first ,
                  estimate.last ,
                  estimate.exclFirst ,
                  estimate.exclLast ,
                  estimate.tol ,
                  estimate.eml ,
                  estimate.urfinal ,
                  transform.function ,
                  transform.fct ,
                  usrdef.outliersEnabled ,
                  usrdef.outliersType ,
                  usrdef.outliersDate ,
                  usrdef.outliersCoef ,
                  usrdef.varEnabled ,
                  usrdef.var ,
                  usrdef.varType ,
                  usrdef.varCoef ,
                  tradingdays.mauto ,
                  tradingdays.pftd ,
                  tradingdays.option ,
                  tradingdays.leapyear ,
                  tradingdays.stocktd ,
                  tradingdays.test ,
                  easter.type ,
                  easter.julian ,
                  easter.duration ,
                  easter.test ,
                  outlier.enabled ,
                  outlier.from ,
                  outlier.to ,
                  outlier.first ,
                  outlier.last ,
                  outlier.exclFirst ,
                  outlier.exclLast ,
                  outlier.ao ,
                  outlier.tc ,
                  outlier.ls ,
                  outlier.so ,
                  outlier.usedefcv ,
                  outlier.cv ,
                  outlier.eml ,
                  outlier.tcrate ,
                  automdl.enabled ,
                  automdl.acceptdefault ,
                  automdl.cancel ,
                  automdl.ub1 ,
                  automdl.ub2 ,
                  automdl.armalimit ,
                  automdl.reducecv ,
                  automdl.ljungboxlimit ,
                  automdl.compare ,
                  arima.mu ,
                  arima.p ,
                  arima.d ,
                  arima.q ,
                  arima.bp ,
                  arima.bd ,
                  arima.bq ,
                  arima.coefEnabled ,
                  arima.coef,
                  arima.coefType ,
                  fcst.horizon ,
                  seats.approx ,
                  seats.trendBoundary ,
                  seats.seasdBoundary ,
                  seats.seasdBoundary1 ,
                  seats.seasTol ,
                  seats.maBoundary ,
                  seats.method )
}

#' @description
#' Use \code{\link{tramoseats}} instead of \code{tramoseats_def}.
#'
#' @export
#' @name deprecated-RJDemetra
tramoseats_def <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                           userdefined = NULL){
  .Deprecated("tramoseats")
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec <- match.arg(spec)
  # create the java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )

  return(tramoseatsJavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))
}

#' @description
#' Use \code{\link{regarima_spec_tramoseats}} instead of \code{regarima_spec_def_tramoseats}.
#'
#' @export
#' @name deprecated-RJDemetra
regarima_spec_def_tramoseats <- function(spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5"),
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
                                         tradingdays.mauto = c(NA, "Unused", "FTest" ,"WaldTest"),
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
                                         fcst.horizon = NA_integer_)
{
  .Deprecated("regarima_spec_tramoseats")
  regarima_spec_tramoseats(spec , NA,
                           estimate.from ,
                           estimate.to ,
                           estimate.first ,
                           estimate.last ,
                           estimate.exclFirst ,
                           estimate.exclLast ,
                           estimate.tol ,
                           estimate.eml ,
                           estimate.urfinal ,
                           transform.function ,
                           transform.fct ,
                           usrdef.outliersEnabled ,
                           usrdef.outliersType ,
                           usrdef.outliersDate ,
                           usrdef.outliersCoef ,
                           usrdef.varEnabled ,
                           usrdef.var ,
                           usrdef.varType ,
                           usrdef.varCoef ,
                           tradingdays.mauto ,
                           tradingdays.pftd ,
                           tradingdays.option ,
                           tradingdays.leapyear ,
                           tradingdays.stocktd ,
                           tradingdays.test ,
                           easter.type ,
                           easter.julian ,
                           easter.duration ,
                           easter.test ,
                           outlier.enabled ,
                           outlier.from ,
                           outlier.to ,
                           outlier.first ,
                           outlier.last ,
                           outlier.exclFirst ,
                           outlier.exclLast ,
                           outlier.ao ,
                           outlier.tc ,
                           outlier.ls ,
                           outlier.so ,
                           outlier.usedefcv ,
                           outlier.cv ,
                           outlier.eml ,
                           outlier.tcrate ,
                           automdl.enabled ,
                           automdl.acceptdefault ,
                           automdl.cancel ,
                           automdl.ub1 ,
                           automdl.ub2 ,
                           automdl.armalimit ,
                           automdl.reducecv ,
                           automdl.ljungboxlimit ,
                           automdl.compare ,
                           arima.mu ,
                           arima.p ,
                           arima.d ,
                           arima.q ,
                           arima.bp ,
                           arima.bd ,
                           arima.bq ,
                           arima.coefEnabled ,
                           arima.coef,
                           arima.coefType ,
                           fcst.horizon )
}

#' @description
#' Use \code{\link{regarima_tramoseats}} instead of \code{regarima_def_tramoseats}.
#'
#' @export
#' @name deprecated-RJDemetra
regarima_def_tramoseats <- function(series, spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5")){
  .Deprecated("regarima_tramoseats")
  if (!is.ts(series)) {
    stop("series must be a time series")
  }
  spec <- match.arg(spec)

  # create the java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;", "getCore")
  jdictionary <- .jnull("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "tramo",
                  ts_r2jd(series),
                  jspec, jdictionary)
  jrobct <- new(Class = "TRAMO_java", internal = jrslt)

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_defTS(jrobj = jrobct, spec = jrspec)
    return(z)
  }
}

#' @description
#' Use \code{\link{regarima_x13}} instead of \code{regarima_def_x13}.
#'
#' @export
#' @name deprecated-RJDemetra
regarima_def_x13 <- function(series, spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c")){
  .Deprecated("regarima_x13")
  if (!is.ts(series)) {
    stop("series must be a time series")
  }
  spec <- match.arg(spec)

  # create the java objects
  jrspec <- .jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "x12",
                  ts_r2jd(series), jspec, jdictionary)

  jrobct <- new(Class = "RegArima_java", internal = jrslt)

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_defX13(jrobj = jrobct, spec = jrspec)
    return(z)
  }
}
#' @description
#' Use \code{\link{regarima_spec_x13}} instead of \code{regarima_spec_def_x13}.
#'
#' @export
#' @name deprecated-RJDemetra
regarima_spec_def_x13  <-function(spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c"),
                                  estimate.from = NA_character_,
                                  estimate.to = NA_character_,
                                  estimate.first = NA_integer_,
                                  estimate.last = NA_integer_,
                                  estimate.exclFirst = NA_integer_,
                                  estimate.exclLast = NA_integer_,
                                  estimate.tol = NA_integer_,
                                  transform.function = c(NA, "Auto", "None", "Log"),
                                  transform.adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                                  transform.aicdiff = NA_integer_,
                                  usrdef.outliersEnabled = NA,
                                  usrdef.outliersType = NA,
                                  usrdef.outliersDate = NA,
                                  usrdef.outliersCoef = NA,
                                  usrdef.varEnabled = NA,
                                  usrdef.var = NA,
                                  usrdef.varType = NA,
                                  usrdef.varCoef = NA,
                                  tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                                  tradingdays.autoadjust = NA,
                                  tradingdays.leapyear = c(NA, "LeapYear", "LengthOfPeriod","None"),
                                  tradingdays.stocktd = NA_integer_,
                                  tradingdays.test = c(NA, "Remove", "Add", "None"),
                                  easter.enabled = NA,
                                  easter.julian = NA,
                                  easter.duration = NA_integer_,
                                  easter.test = c(NA, "Add", "Remove", "None"),
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
                                  outlier.method = c(NA, "AddOne", "AddAll"),
                                  outlier.tcrate  = NA_integer_,
                                  automdl.enabled = NA,
                                  automdl.acceptdefault = NA,
                                  automdl.cancel = NA_integer_,
                                  automdl.ub1 = NA_integer_,
                                  automdl.ub2 = NA_integer_,
                                  automdl.mixed = NA,
                                  automdl.balanced = NA,
                                  automdl.armalimit = NA_integer_,
                                  automdl.reducecv = NA_integer_,
                                  automdl.ljungboxlimit = NA_integer_,
                                  automdl.ubfinal= NA_integer_,
                                  arima.mu = NA,
                                  arima.p = NA_integer_,
                                  arima.d = NA_integer_,
                                  arima.q = NA_integer_,
                                  arima.bp = NA_integer_,
                                  arima.bd = NA_integer_,
                                  arima.bq = NA_integer_,
                                  arima.coefEnabled = NA,
                                  arima.coef = NA,
                                  arima.coefType = NA,
                                  fcst.horizon = NA_integer_)
{
  .Deprecated("regarima_spec_x13")
  regarima_spec_x13(spec, NA,
                    estimate.from ,
                    estimate.to ,
                    estimate.first ,
                    estimate.last ,
                    estimate.exclFirst ,
                    estimate.exclLast ,
                    estimate.tol ,
                    transform.function ,
                    transform.adjust ,
                    transform.aicdiff ,
                    usrdef.outliersEnabled ,
                    usrdef.outliersType ,
                    usrdef.outliersDate ,
                    usrdef.outliersCoef ,
                    usrdef.varEnabled ,
                    usrdef.var ,
                    usrdef.varType ,
                    usrdef.varCoef ,
                    tradingdays.option ,
                    tradingdays.autoadjust ,
                    tradingdays.leapyear ,
                    tradingdays.stocktd ,
                    tradingdays.test ,
                    easter.enabled ,
                    easter.julian ,
                    easter.duration ,
                    easter.test ,
                    outlier.enabled ,
                    outlier.from ,
                    outlier.to ,
                    outlier.first ,
                    outlier.last ,
                    outlier.exclFirst ,
                    outlier.exclLast ,
                    outlier.ao ,
                    outlier.tc ,
                    outlier.ls ,
                    outlier.so ,
                    outlier.usedefcv ,
                    outlier.cv ,
                    outlier.method ,
                    outlier.tcrate  ,
                    automdl.enabled ,
                    automdl.acceptdefault ,
                    automdl.cancel ,
                    automdl.ub1 ,
                    automdl.ub2 ,
                    automdl.mixed ,
                    automdl.balanced ,
                    automdl.armalimit ,
                    automdl.reducecv ,
                    automdl.ljungboxlimit ,
                    automdl.ubfinal,
                    arima.mu ,
                    arima.p ,
                    arima.d ,
                    arima.q ,
                    arima.bp ,
                    arima.bd ,
                    arima.bq ,
                    arima.coefEnabled ,
                    arima.coef ,
                    arima.coefType ,
                    fcst.horizon)
}
