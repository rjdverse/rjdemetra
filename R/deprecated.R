#' Deprecated functions
#'
#' @description
#' Use \code{\link{x13_spec}} instead of \code{x13_spec_def}.
#'
#' @inheritParams x13_spec
#' @inheritParams tramoseats_spec
#' @inheritParams x13
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
  spec <- match.arg(spec)
  reg_spec <- gsub("RSA", "RG", spec)
  regarima <- regarima_spec_def_x13(reg_spec, estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,
                                    estimate.exclLast,estimate.tol,transform.function,transform.adjust,
                                    transform.aicdiff,usrdef.outliersEnabled,usrdef.outliersType,
                                    usrdef.outliersDate,usrdef.outliersCoef,usrdef.varEnabled,usrdef.var,usrdef.varType,
                                    usrdef.varCoef,tradingdays.option,tradingdays.autoadjust,tradingdays.leapyear,
                                    tradingdays.stocktd,tradingdays.test,easter.enabled,easter.julian,
                                    easter.duration,easter.test,outlier.enabled,outlier.from,outlier.to,outlier.first,
                                    outlier.last,outlier.exclFirst,outlier.exclLast,outlier.ao,outlier.tc,outlier.ls,outlier.so,
                                    outlier.usedefcv,outlier.cv,outlier.method,outlier.tcrate,automdl.enabled,
                                    automdl.acceptdefault,automdl.cancel,automdl.ub1,automdl.ub2,automdl.mixed,automdl.balanced,
                                    automdl.armalimit,automdl.reducecv,automdl.ljungboxlimit,automdl.ubfinal,arima.mu,
                                    arima.p,arima.d,arima.q,arima.bp,arima.bd,arima.bq,arima.coefEnabled,
                                    arima.coef,arima.coefType,fcst.horizon)

  x11 <- x11_spec_def(spec,x11.mode,x11.seasonalComp,x11.lsigma,x11.usigma,x11.trendAuto,x11.trendma,x11.seasonalma,x11.fcasts,x11.bcasts,x11.excludeFcasts)
  z <- list(regarima = regarima, x11 = x11)
  class(z) <- c("SA_spec", "X13")
  return(z)
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
  spec<-match.arg(spec)
  reg_spec=gsub("RSA","TR",spec)
  regarima <-  regarima_spec_tramoseats(reg_spec,NA,estimate.from,estimate.to,estimate.first,estimate.last,estimate.exclFirst,estimate.exclLast,
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

#' @description
#' Use \code{\link{tramoseats}} instead of \code{tramoseats_def}.
#'
#' @export
#' @name deprecated-RJDemetra
tramoseats_def <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA", "RSA4", "RSA5"),
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
  spec <- match.arg(spec)
  transform.function <- match.arg(transform.function)
  tradingdays.mauto <- match.arg(tradingdays.mauto)
  tradingdays.option <- match.arg(tradingdays.option)
  tradingdays.test <- match.arg(tradingdays.test)
  easter.type <- match.arg(easter.type)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # check & define the time span variables for estimate and outlier
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                        exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                        exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # check the predefined-outliers varaiables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType,
                                 outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # check the user-defined variables
  n.usrvar <- if (is.mts(usrdef.var)) {dim(usrdef.var)[2]} else if (is.ts(usrdef.var)) {1} else {0}
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # check the mode of remaining variables
  list.logical.usrdef <-list("usrdef.outliersEnabled","usrdef.varEnabled","arima.coefEnabled")
  list.logical = list("estimate.eml","tradingdays.leapyear","easter.julian","easter.test","outlier.enabled","outlier.ao",
                      "outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","outlier.eml","automdl.enabled",
                      "automdl.acceptdefault","automdl.compare","arima.mu")
  list.logical.check <- append(list.logical.usrdef,list.logical)

  list.numeric.span <- list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                            "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast","fcst.horizon")

  list.numeric = list("estimate.tol","estimate.urfinal","transform.fct","tradingdays.pftd",
                      "tradingdays.stocktd","easter.duration","outlier.cv","outlier.tcrate",
                      "automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit","automdl.reducecv",
                      "automdl.ljungboxlimit","arima.p","arima.d","arima.q","arima.bp","arima.bd","arima.bq")

  list.numeric.check <- append(list.numeric.span,list.numeric)

  list.character <- list("transform.function","tradingdays.mauto","tradingdays.option","tradingdays.test","easter.type")

  var.list<-list()
  for (i in 1:length(list.logical.check)) {
    eval(parse(text=paste("if( !is.logical(",list.logical.check[i],")) {",list.logical.check[i]," = NA; var.list=append(var.list,'",list.logical.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."))}

  var.list<-list()
  for (i in 1:length(list.numeric.check)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric.check[i],")) {",list.numeric.check[i]," = NA; var.list=append(var.list,'",list.numeric.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."))}

  variables<-append(list("estimate.span","outlier.span"),list.logical)
  variables<-append(variables,list.numeric)
  variables<-append(variables,list.character)

  # Create the java object
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)

  # Extract model specification from the java object
  rspec <- specTS_jd2r( spec = jrspec)

  # Predefined and modified values
  predef.out <- list(Predefined = NA, Final = predef.outliers)
  predef.var <- list(Predefined = list(series = NA, description = NA), Final = predef.variables)
  arima.coeff <- list(Predefined = NA , Final = predef.coef)

  for (i in 1:length(variables)) {
    eval(parse(text=paste(variables[i],".tab=c(rspec$",variables[i],",",variables[i],",","NA)", sep="")))
  }

  v_estimate<-data.frame(span = estimate.span.tab, tolerance = estimate.tol.tab, exact_ml = estimate.eml.tab, urfinal = estimate.urfinal.tab, row.names = c("Predefined","User_modif","Final"), stringsAsFactors=FALSE)
  v_transform <- data.frame(tfunction=transform.function.tab,fct=transform.fct.tab, stringsAsFactors=FALSE)
  v_usrdef <- data.frame(outlier= c(FALSE, usrdef.outliersEnabled,NA), outlier.coef= c(FALSE,NA,NA),
                         variables =c(FALSE, usrdef.varEnabled,NA), variables.coef = c(FALSE,NA,NA), stringsAsFactors=FALSE)
  v_trading.days<-data.frame( automatic = tradingdays.mauto.tab, pftd = tradingdays.pftd.tab, option = tradingdays.option.tab,
                              leapyear = tradingdays.leapyear.tab,stocktd = tradingdays.stocktd.tab, test = tradingdays.test.tab,
                              stringsAsFactors=FALSE)
  v_easter<-data.frame(type=easter.type.tab,julian=easter.julian.tab,duration=easter.duration.tab,test=easter.test.tab,
                       stringsAsFactors=FALSE)
  v_outliers<-data.frame(enabled=outlier.enabled.tab,span=outlier.span.tab,ao=outlier.ao.tab, tc=outlier.tc.tab, ls = outlier.ls.tab,
                         so=outlier.so.tab,usedefcv=outlier.usedefcv.tab,cv=outlier.cv.tab,eml=outlier.eml.tab,
                         tcrate=outlier.tcrate.tab, stringsAsFactors=FALSE)
  v_arima <-data.frame(enabled=automdl.enabled.tab,automdl.acceptdefault=automdl.acceptdefault.tab,automdl.cancel=automdl.cancel.tab,
                       automdl.ub1=automdl.ub1.tab,automdl.ub2=automdl.ub2.tab,automdl.armalimit=automdl.armalimit.tab,
                       automdl.reducecv=automdl.reducecv.tab, automdl.ljungboxlimit=automdl.ljungboxlimit.tab, compare = automdl.compare.tab,
                       arima.mu=arima.mu.tab,arima.p=arima.p.tab,arima.d =arima.d.tab,arima.q=arima.q.tab,
                       arima.bp=arima.bp.tab,arima.bd=arima.bd.tab,arima.bq=arima.bq.tab,arima.coef = c(FALSE,arima.coefEnabled,NA),
                       stringsAsFactors=FALSE)
  v_forecast <- data.frame(horizon = c(-2,fcst.horizon,NA), stringsAsFactors=FALSE)

  span.spec <-rspec$span

  # Final values
  x <- spec_estimateTS(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformTS(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdTS(td=v_trading.days)
  easter<- spec_easterTS(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersTS(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaTS(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
            transform=transform,
            regression=regression,
            outliers=outliers,
            arima=arima,
            forecast = forecast,
            span=span)
  class(z) = c("regarima_spec","TRAMO_SEATS")
  return(z)
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
  jrobct <- new(Class = "JD2_TRAMO_java", internal = jrslt)

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

  jrobct <- new(Class = "JD2_RegArima_java", internal = jrslt)

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
  spec<-match.arg(spec)
  transform.function <- match.arg(transform.function)
  transform.adjust <- match.arg(transform.adjust)
  tradingdays.option <- match.arg(tradingdays.option)
  tradingdays.leapyear <- match.arg(tradingdays.leapyear)
  tradingdays.test <- match.arg(tradingdays.test)
  easter.test <- match.arg(easter.test)
  outlier.method <- match.arg(outlier.method)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # check and define the time span variables for estimate and outlier
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                        exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                        exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # check the predefined-outliers varaiables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType,outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # check the user-defined variables
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # check the mode of remaining variables
  list.logical.usrdef <-list("usrdef.outliersEnabled","usrdef.varEnabled","arima.coefEnabled")
  list.logical<-list("tradingdays.autoadjust","easter.enabled","easter.julian",
                     "outlier.enabled","outlier.ao","outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","automdl.enabled",
                     "automdl.acceptdefault","automdl.mixed","automdl.balanced","arima.mu")
  list.logical.check <- append(list.logical.usrdef,list.logical)
  list.numeric.span <- list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                            "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast","fcst.horizon")
  list.numeric<-list("estimate.tol","transform.aicdiff","tradingdays.stocktd","easter.duration","outlier.cv",
                     "outlier.tcrate","automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit",
                     "automdl.reducecv","automdl.ljungboxlimit","automdl.ubfinal","arima.p","arima.d",
                     "arima.q","arima.bp","arima.bd","arima.bq")
  list.numeric.check <- append(list.numeric.span,list.numeric)
  list.character<-list("transform.function","transform.adjust","tradingdays.option","tradingdays.leapyear","tradingdays.test",
                       "easter.test","outlier.method")

  var.list<-list()
  for (i in 1:length(list.logical.check)) {
    eval(parse(text=paste("if( !is.logical(",list.logical.check[i],")) {",list.logical.check[i]," = NA; var.list=append(var.list,'",list.logical.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."))}

  var.list<-list()
  for (i in 1:length(list.numeric.check)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric.check[i],")) {",list.numeric.check[i]," = NA; var.list=append(var.list,'",list.numeric.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."))}

  variables<-append(list("estimate.span","outlier.span"),list.logical)
  variables<-append(variables,list.numeric)
  variables<-append(variables,list.character)

  # create the java object
  jrspec<-.jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", spec)

  # extract model specification from the java object
  rspec <- specX13_jd2r(spec = jrspec, extra_info = FALSE)

  # Predefined and modified values
  predef.out <- list(Predefined = NA, Final = predef.outliers)
  predef.var <- list(Predefined = list(series = NA, description = NA), Final = predef.variables)
  arima.coeff <- list(Predefined = NA , Final = predef.coef)

  for (i in 1:length(variables)) {
    eval(parse(text=paste(variables[i],".tab=c(rspec$",variables[i],",",variables[i],",","NA)", sep="")))
  }

  v_estimate <-data.frame(span = estimate.span.tab, tolerance = estimate.tol.tab, stringsAsFactors=FALSE)
  v_transform <- data.frame(tfunction=transform.function.tab,adjust=transform.adjust.tab,aicdiff=transform.aicdiff.tab,
                            stringsAsFactors=FALSE)
  v_trading.days<-data.frame( option = tradingdays.option.tab, autoadjust=tradingdays.autoadjust.tab, leapyear = tradingdays.leapyear.tab,
                              stocktd = tradingdays.stocktd.tab, test = tradingdays.test.tab, stringsAsFactors=FALSE)
  v_easter<-data.frame(enabled=easter.enabled.tab,julian=easter.julian.tab,duration=easter.duration.tab,test=easter.test.tab, stringsAsFactors=FALSE)
  v_usrdef <- data.frame(outlier= c(FALSE, usrdef.outliersEnabled,NA),outlier.coef= c(FALSE,NA,NA),
                         variables =c(FALSE, usrdef.varEnabled,NA), variables.coef = c(FALSE,NA,NA),stringsAsFactors=FALSE)
  v_outliers<-data.frame(enabled=outlier.enabled.tab,span=outlier.span.tab,ao=outlier.ao.tab, tc=outlier.tc.tab, ls = outlier.ls.tab,
                         so=outlier.so.tab,usedefcv=outlier.usedefcv.tab,cv=outlier.cv.tab,method=outlier.method.tab,
                         tcrate=outlier.tcrate.tab,stringsAsFactors=FALSE)
  v_arima <-data.frame(enabled=automdl.enabled.tab,automdl.acceptdefault=automdl.acceptdefault.tab,automdl.cancel=automdl.cancel.tab,
                       automdl.ub1=automdl.ub1.tab,automdl.ub2=automdl.ub2.tab,automdl.mixed=automdl.mixed.tab,automdl.balanced=automdl.balanced.tab,
                       automdl.armalimit=automdl.armalimit.tab,automdl.reducecv=automdl.reducecv.tab, automdl.ljungboxlimit=automdl.ljungboxlimit.tab,
                       automdl.ubfinal=automdl.ubfinal.tab,arima.mu=arima.mu.tab,arima.p=arima.p.tab,arima.d =arima.d.tab,arima.q=arima.q.tab,
                       arima.bp=arima.bp.tab,arima.bd=arima.bd.tab,arima.bq=arima.bq.tab,arima.coef = c(FALSE,arima.coefEnabled,NA), stringsAsFactors=FALSE)
  v_forecast <- data.frame(horizon = c(-2,fcst.horizon,NA), stringsAsFactors=FALSE)

  span.spec <-rspec$span

  # Final values
  x <- spec_estimateX13(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformX13(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdX13(td=v_trading.days,tf = transform[3,1], tadj = transform[3,2])
  easter<- spec_easterX13(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersX13(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaX13(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
            transform=transform,
            regression=regression,
            outliers=outliers,
            arima=arima,
            forecast = forecast,
            span=span)
  class(z) = c("regarima_spec","X13")
  return(z)
}
