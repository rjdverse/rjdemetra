#' Pre-specified X13 model specification, SA/X13
#'
#' @description
#' .
#'
#' @inheritParams jd_regarima_specDefX13
#' @param spec predefined JDemetra+ model specification (see Details). The default is "RSA5c".
#' @param x11.mode .
#' @param x11.seasonalComp .
#' @param x11.lsigma .
#' @param x11.usigma .
#' @param x11.trendAuto .
#' @param x11.trendma .
#' @param x11.seasonalma .
#' @param x11.fcasts .
#' @param x11.bcasts .
#' @param x11.excludeFcasts .
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
#' myspec <- jd_x13_specDef(spec="RSA5c")
#'
#' @export

jd_x13_specDef <-function(spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c","X11"),
                                   estimate.from=NA_character_,
                                   estimate.to=NA_character_,
                                   estimate.first=NA_integer_,
                                   estimate.last=NA_integer_,
                                   estimate.exclFirst=NA_integer_,
                                   estimate.exclLast=NA_integer_,
                                   estimate.tol=NA_integer_,
                                   transform.function=c(NA_character_,"Auto","None","Log"),
                                   transform.adjust = c(NA_character_,"None","LeapYear","LengthOfPeriod"),
                                   transform.aicdiff = NA_integer_,
                                   usrdef.outliersEnabled = NA,
                                   usrdef.outliersType = NA,
                                   usrdef.outliersDate = NA,
                                   usrdef.outliersCoef = NA,
                                   usrdef.varEnabled = NA,
                                   usrdef.var = NA,
                                   usrdef.varCoef = NA,
                                   tradingdays.option = c(NA_character_,"TradingDays","WorkingDays","None"),
                                   tradingdays.autoadjust = NA,
                                   tradingdays.leapyear = c(NA_character_,"LeapYear","LengthOfPeriod","None"),
                                   tradingdays.stocktd = NA_integer_,
                                   tradingdays.test = c(NA_character_,"Remove","Add","None"),
                                   easter.enabled = NA,
                                   easter.julian = NA,
                                   easter.duration = NA_integer_,
                                   easter.test = c(NA_character_,"Add","Remove","None"),
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
                                   outlier.method = c(NA_character_,"AddOne","AddAll"),
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
                                   arima.coef= NA,
                                   arima.coefType = NA,
                                   fcst.horizon = NA_integer_,
                                   x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive"),
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
  spec<-match.arg(spec)
  reg_spec=gsub("RSA","RG",spec)
  regarima <- jd_regarima_specDefX13(reg_spec,estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,
                                     estimate.exclLast,estimate.tol,transform.function,transform.adjust,
                                     transform.aicdiff,usrdef.outliersEnabled,usrdef.outliersType,
                                     usrdef.outliersDate,usrdef.outliersCoef,usrdef.varEnabled,usrdef.var,
                                     usrdef.varCoef,tradingdays.option,tradingdays.autoadjust,tradingdays.leapyear,
                                     tradingdays.stocktd,tradingdays.test,easter.enabled,easter.julian,
                                     easter.duration,easter.test,outlier.enabled,outlier.from,outlier.to,outlier.first,
                                     outlier.last,outlier.exclFirst,outlier.exclLast,outlier.ao,outlier.tc,outlier.ls,outlier.so,
                                     outlier.usedefcv,outlier.cv,outlier.method,outlier.tcrate,automdl.enabled,
                                     automdl.acceptdefault,automdl.cancel,automdl.ub1,automdl.ub2,automdl.mixed,automdl.balanced,
                                     automdl.armalimit,automdl.reducecv,automdl.ljungboxlimit,automdl.ubfinal,arima.mu,
                                     arima.p,arima.d,arima.q,arima.bp,arima.bd,arima.bq,arima.coefEnabled,
                                     arima.coef,arima.coefType,fcst.horizon)

  x11 <- jd_x11_specDef(spec,x11.mode,x11.seasonalComp,x11.lsigma,x11.usigma,x11.trendAuto,x11.trendma,x11.seasonalma,x11.fcasts,x11.bcasts,x11.excludeFcasts)
  z <- list(regarima = regarima, x11 = x11)
  class(z) <- c("SA_Spec","X13")
  return(z)
}

jd_x11_specDef<- function(spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c","X11"),
                          x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive"),
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
  spec<-match.arg(spec)
  x11.mode <- match.arg(x11.mode)

  x11.seasonalma <- spec_seasma(x11.seasonalma)
  x11.trendma <- spec_trendma(x11.trendma)

  list.logical <- list("x11.seasonalComp","x11.trendAuto","x11.excludeFcasts")
  list.numeric <- list("x11.lsigma","x11.usigma","x11.fcasts","x11.bcasts")

  var.list<-list()
  for (i in 1:length(list.logical)) {
    eval(parse(text=paste("if( !is.logical(",list.logical[i],")) {",list.logical[i]," = NA; var.list=append(var.list,'",list.logical[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."), call. = FALSE)}

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."), call. = FALSE)}

  # modifed values
  x11 <- do.call(data.frame, as.list(match.call()[c(-1,-2)]))
  # create the java object
  jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  rspec <- specX11_jd2r(spec = jrspec)
  x11.spec <- do.call(data.frame, rspec)
  names(x11.spec) <- paste0("x11.",names(x11.spec))
  x11.mod <- rbind(x11.spec,x11,rep(NA,length(x11.spec)))
  z <- spec_x11(x11.mod)

  class(z) <- c("X11_Spec","data.frame")
  return(z)
}

#' X13 model specification, SA/X13
#'
#' @description
#' .
#'
#' @inheritParams jd_x13_specDef
#' @param object model specification, object of class c("SA_Spec","X13") or c("SA","X13").
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
#' myspec <- jd_x13_specDef(spec="RSA5c")
#' mysa<- jd_x13(myseries,myspec)
#' myspec1 <- jd_x13_spec(myspec,x11.seasonalma=rep("S3X1",12))
#' mysa1 <-jd_x13(myseries,myspec1)
#' myspec2 <- jd_x13_spec(mysa,x11.seasonalma=rep("S3X1",12))
#' mysa2 <-jd_x13(myseries,myspec2)
#'
#' @export

jd_x13_spec <-function(object,
                          estimate.from=NA_character_,
                          estimate.to=NA_character_,
                          estimate.first=NA_integer_,
                          estimate.last=NA_integer_,
                          estimate.exclFirst=NA_integer_,
                          estimate.exclLast=NA_integer_,
                          estimate.tol=NA_integer_,
                          transform.function=c(NA_character_,"Auto","None","Log"),
                          transform.adjust = c(NA_character_,"None","LeapYear","LengthOfPeriod"),
                          transform.aicdiff = NA_integer_,
                          usrdef.outliersEnabled = NA,
                          usrdef.outliersType = NA,
                          usrdef.outliersDate = NA,
                          usrdef.outliersCoef = NA,
                          usrdef.varEnabled = NA,
                          usrdef.var = NA,
                          usrdef.varCoef = NA,
                          tradingdays.option = c(NA_character_,"TradingDays","WorkingDays","None"),
                          tradingdays.autoadjust = NA,
                          tradingdays.leapyear = c(NA_character_,"LeapYear","LengthOfPeriod","None"),
                          tradingdays.stocktd = NA_integer_,
                          tradingdays.test = c(NA_character_,"Remove","Add","None"),
                          easter.enabled = NA,
                          easter.julian = NA,
                          easter.duration = NA_integer_,
                          easter.test = c(NA_character_,"Add","Remove","None"),
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
                          outlier.method = c(NA_character_,"AddOne","AddAll"),
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
                          arima.coef= NA,
                          arima.coefType = NA,
                          fcst.horizon = NA_integer_,
                          x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive"),
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
  if (!inherits(object, "X13") & (inherits(object, c("SA","SA_Spec"))==FALSE))
    stop("use only with c(\"SA\",\"X13\") and c(\"SA_Spec\",\"X13\") objects", call. = FALSE)

  regarima <- jd_regarima_specX13(object ,estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,
                                     estimate.exclLast,estimate.tol,transform.function,transform.adjust,
                                     transform.aicdiff,usrdef.outliersEnabled,usrdef.outliersType,
                                     usrdef.outliersDate,usrdef.outliersCoef,usrdef.varEnabled,usrdef.var,
                                     usrdef.varCoef,tradingdays.option,tradingdays.autoadjust,tradingdays.leapyear,
                                     tradingdays.stocktd,tradingdays.test,easter.enabled,easter.julian,
                                     easter.duration,easter.test,outlier.enabled,outlier.from,outlier.to,outlier.first,
                                     outlier.last,outlier.exclFirst,outlier.exclLast,outlier.ao,outlier.tc,outlier.ls,outlier.so,
                                     outlier.usedefcv,outlier.cv,outlier.method,outlier.tcrate,automdl.enabled,
                                     automdl.acceptdefault,automdl.cancel,automdl.ub1,automdl.ub2,automdl.mixed,automdl.balanced,
                                     automdl.armalimit,automdl.reducecv,automdl.ljungboxlimit,automdl.ubfinal,arima.mu,
                                     arima.p,arima.d,arima.q,arima.bp,arima.bd,arima.bq,arima.coefEnabled,
                                     arima.coef,arima.coefType,fcst.horizon)

  x11 <- jd_x11_spec(object,x11.mode,x11.seasonalComp,x11.lsigma,x11.usigma,x11.trendAuto,x11.trendma,x11.seasonalma,x11.fcasts,x11.bcasts,x11.excludeFcasts)
  z <- list(regarima = regarima, x11 = x11)
  class(z) <- c("SA_Spec","X13")
  return(z)
}

jd_x11_spec<- function(object,
                          x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive"),
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
  x11.mode <- match.arg(x11.mode)

  x11.seasonalma <- spec_seasma(x11.seasonalma)
  x11.trendma <- spec_trendma(x11.trendma)

  list.logical <- list("x11.seasonalComp","x11.trendAuto","x11.excludeFcasts")
  list.numeric <- list("x11.lsigma","x11.usigma","x11.fcasts","x11.bcasts")

  var.list<-list()
  for (i in 1:length(list.logical)) {
    eval(parse(text=paste("if( !is.logical(",list.logical[i],")) {",list.logical[i]," = NA; var.list=append(var.list,'",list.logical[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."), call. = FALSE)}

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."), call. = FALSE)}

  # modifed values
  x11 <- do.call(data.frame, as.list(match.call()[c(-1,-2)]))
  x11.spec <- s_x11(object)
  x11.mod <- rbind(x11.spec, x11,rep(NA,length(x11.spec)))
  z <- spec_x11(x11.mod)

  class(z) <- c("X11_Spec","data.frame")
  return(z)
}



