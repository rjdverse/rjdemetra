# The function creates a "JD_RegArima_Spec" S3 class object from a JD+ defined specification for X13 method
jd_regarima_specDefTS <-function(spec=c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5"),
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
                            fcst.horizon = NA_integer_)
{
  spec<-match.arg(spec)
  transform.function <-match.arg(transform.function)
  tradingdays.mauto <-match.arg(tradingdays.mauto)
  tradingdays.option <-match.arg(tradingdays.option)
  tradingdays.test <-match.arg(tradingdays.test)
  easter.type <-match.arg(easter.type)
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
  predef.variables <- spec_preVar(var = usrdef.var, vartype = rep("Undefined",n.usrvar), varcoef = usrdef.varCoef)

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
  if (exists("jd_clobj"))
    rm(jd_clobj)
  jd_clobj <-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)

  # Extract model specification from the java object
  rspec <- specTS_jd2r( spec = jrspec)

  # Remove the java object
  if (exists("jd_clobj", envir = .GlobalEnv))
    rm(jd_clobj, envir = .GlobalEnv)

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
  class(z) = c("JD_RegArima_Spec","TRAMO_SEATS")
  return(z)
}

# The function creates a ("JD_RegArima_Spec","TRAMO_SEATS") class object from from a JD_RegArima_Spec or JD_RegArima object
jd_regarima_specTS <-function(object,
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
                          fcst.horizon=NA_integer_)
{

  if (!inherits(object, "TRAMO_SEATS") & (!inherits(object, "JD_RegArima") | !inherits(object, "JD_RegArima_Spec")))
    stop("use only with c(\"JD_RegArima\",\"TRAMO_SEATS\") or c(\"JD_RegArima_Spec\",\"TRAMO_SEATS\") objects", call. = FALSE)

  transform.function <-match.arg(transform.function)
  tradingdays.mauto <-match.arg(tradingdays.mauto)
  tradingdays.option <-match.arg(tradingdays.option)
  tradingdays.test <-match.arg(tradingdays.test)
  easter.type <-match.arg(easter.type)
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
  predef.variables <- spec_preVar(var = usrdef.var, vartype = rep("Undefined",n.usrvar), varcoef = usrdef.varCoef)

  # check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # check the mode of remaining variables
  list.logical = list("usrdef.outliersEnabled","usrdef.varEnabled","estimate.eml","tradingdays.leapyear",
                      "easter.julian","easter.test","outlier.enabled","outlier.ao",
                      "outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","outlier.eml","automdl.enabled",
                      "automdl.acceptdefault","automdl.compare","arima.mu","arima.coefEnabled")

  list.numeric = list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                      "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast",
                      "estimate.tol","estimate.urfinal","transform.fct","tradingdays.pftd",
                      "tradingdays.stocktd","easter.duration","outlier.cv","outlier.tcrate",
                      "automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit","automdl.reducecv",
                      "automdl.ljungboxlimit","arima.p","arima.d","arima.q","arima.bp","arima.bd","arima.bq",
                      "fcst.horizon")

  var.list<-list()
  for (i in 1:length(list.logical)) {
    eval(parse(text=paste("if( !is.logical(",list.logical[i],")) {",list.logical[i]," = NA; var.list=append(var.list,'",list.logical[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."))}

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."))}

  # Predefined values
  estimate.spec <- s_estimate(object)
  transform.spec <- s_transform(object)
  usrdef.spec <- s_usrdef(object)
  trading.days.spec <- s_td(object)
  easter.spec <- s_easter(object)
  outliers.spec <- s_out(object)
  arima.spec <- s_arima(object)
  forecast.spec <- s_fcst(object)
  span.spec <- s_span(object)

  predef.outliers.spec <- s_preOut(object)
  predef.variables.spec <- s_preVar(object)
  predef.coef.spec <- s_arimaCoef(object)

  # Modified values
  predef.out <- list(Predefined = predef.outliers.spec, Final = predef.outliers)
  predef.var <- list(Predefined = predef.variables.spec, Final = predef.variables)
  arima.coeff <- list(Predefined = predef.coef.spec , Final = predef.coef)

  estimate.mod <-data.frame(span = estimate.span, tolerance = estimate.tol, exact_ml = estimate.eml, urfinal = estimate.urfinal, stringsAsFactors=FALSE)
  transform.mod <- data.frame(tfunction=transform.function,fct=transform.fct, stringsAsFactors=FALSE)
  usrdef.mod <- data.frame(outlier=usrdef.outliersEnabled, outlier.coef= NA,
                           variables = usrdef.varEnabled, variables.coef = NA, stringsAsFactors=FALSE)
  trading.days.mod <-data.frame( automatic = tradingdays.mauto, pftd = tradingdays.pftd, option = tradingdays.option,
                                leapyear = tradingdays.leapyear,stocktd = tradingdays.stocktd, test = tradingdays.test, stringsAsFactors=FALSE)
  easter.mod <-data.frame(type=easter.type,julian=easter.julian,duration=easter.duration,test=easter.test, stringsAsFactors=FALSE)
  outliers.mod <-data.frame(enabled=outlier.enabled,span=outlier.span,ao=outlier.ao, tc=outlier.tc, ls = outlier.ls,
                       so=outlier.so,usedefcv=outlier.usedefcv,cv=outlier.cv,eml=outlier.eml,tcrate=outlier.tcrate, stringsAsFactors=FALSE)
  arima.mod <-data.frame(enabled=automdl.enabled,automdl.acceptdefault=automdl.acceptdefault,automdl.cancel=automdl.cancel,
                    automdl.ub1=automdl.ub1,automdl.ub2=automdl.ub2,automdl.armalimit=automdl.armalimit,
                    automdl.reducecv=automdl.reducecv, automdl.ljungboxlimit=automdl.ljungboxlimit, compare = automdl.compare,
                    arima.mu=arima.mu,arima.p=arima.p,arima.d =arima.d,arima.q=arima.q,arima.bp=arima.bp,arima.bd=arima.bd,
                    arima.bq=arima.bq,arima.coef = arima.coefEnabled, stringsAsFactors=FALSE)
  forecast.mod <-data.frame(horizon=fcst.horizon)

  v_estimate <- rbind(estimate.spec,estimate.mod,rep(NA,length(estimate.spec)))
  v_transform <- rbind(transform.spec,transform.mod,rep(NA,length(transform.spec)))
  v_usrdef <- rbind(usrdef.spec,usrdef.mod,rep(NA,length(usrdef.spec)))
  v_trading.days <- rbind(trading.days.spec,trading.days.mod,rep(NA,length(trading.days.spec)))
  v_easter <- rbind(easter.spec,easter.mod,rep(NA,length(easter.spec)))
  v_outliers <- rbind(outliers.spec,outliers.mod,rep(NA,length(outliers.spec)))
  v_arima <- rbind(arima.spec,arima.mod,rep(NA,length(arima.spec)))
  v_forecast <-rbind(forecast.spec,forecast.mod,NA)

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
  class(z) = c("JD_RegArima_Spec","TRAMO_SEATS")
  return(z)

}

