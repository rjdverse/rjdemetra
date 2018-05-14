#Define the S4 java object
setClass(
  Class="JD2_RegArima_java",
  contains = "JD2_ProcResults"
)
setClass(
  Class="JD2_TRAMO_java",
  contains = "JD2_ProcResults"
)

# The function creates a "JD_RegArima" S3 class object from a JD+ defined specification for X13 method
jd_regarima_defX13 <-function(series, spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c")){

  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)

  #create the java objects
  if (exists("jd_clobj"))
    rm(jd_clobj)
  if (exists("jrobct"))
    rm(jrobct)
  jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec<-.jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/regarima/Processor", "Lec/tstoolkit/jdr/regarima/Processor$Results;", "x12", ts_r2jd(series), jspec, jdictionary)

  jrobct <- new (Class = "JD2_RegArima_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{

    #Extract model specification from the java object
    rspec <- specX13_jd2r( spec = jrspec)

    estimate<-data.frame(span = rspec$estimate.span, tolerance = rspec$estimate.tol, row.names="", stringsAsFactors=FALSE)
    transform <- data.frame(tfunction=rspec$transform.function,adjust=rspec$transform.adjust,aicdiff=rspec$transform.aicdiff, row.names="", stringsAsFactors=FALSE)
    trading.days<-data.frame( option = rspec$tradingdays.option, autoadjust=rspec$tradingdays.autoadjust, leapyear = rspec$tradingdays.leapyear,
                              stocktd = rspec$tradingdays.stocktd, test = rspec$tradingdays.test, row.names="", stringsAsFactors=FALSE)
    easter<-data.frame(enabled=rspec$easter.enabled,julian=rspec$easter.julian,duration=rspec$easter.duration,test=rspec$easter.test, row.names="",stringsAsFactors=FALSE)
    usrdef <- data.frame(outlier=FALSE, outlier.coef= FALSE, variables = FALSE, variables.coef = FALSE, stringsAsFactors=FALSE)
    userdef <-list(specification = usrdef, outliers = NA, variables = list(series = NA, description = NA))
    regression<-list(userdef=userdef, trading.days = trading.days, easter = easter)
    outliers<-data.frame(enabled=rspec$outlier.enabled,span=rspec$outlier.span,ao=rspec$outlier.ao, tc=rspec$outlier.tc, ls = rspec$outlier.ls,
                         so=rspec$outlier.so,usedefcv=rspec$outlier.usedefcv,cv=rspec$outlier.cv,method=rspec$outlier.method,
                         tcrate=rspec$outlier.tcrate, row.names="", stringsAsFactors=FALSE)
    arima.dsc <-data.frame(enabled=rspec$automdl.enabled,automdl.acceptdefault=rspec$automdl.acceptdefault,automdl.cancel=rspec$automdl.cancel,
                           automdl.ub1=rspec$automdl.ub1,automdl.ub2=rspec$automdl.ub2,automdl.mixed=rspec$automdl.mixed,automdl.balanced=rspec$automdl.balanced,
                           automdl.armalimit=rspec$automdl.armalimit,automdl.reducecv=rspec$automdl.reducecv, automdl.ljungboxlimit=rspec$automdl.ljungboxlimit,
                           automdl.ubfinal=rspec$automdl.ubfinal,arima.mu=rspec$arima.mu,arima.p=rspec$arima.p,arima.d =rspec$arima.d,arima.q=rspec$arima.q,
                           arima.bp=rspec$arima.bp,arima.bd=rspec$arima.bd,arima.bq=rspec$arima.bq, arima.coef = FALSE,row.names="",stringsAsFactors=FALSE)
    arima <- list(specification = arima.dsc, coefficients = NA)
    forecast <- data.frame(horizon = c(-2),row.names = c(""), stringsAsFactors=FALSE)
    span <-rspec$span

    specification <- list(estimate=estimate, transform=transform, regression=regression, outliers=outliers,
                          arima=arima, forecast = forecast, span=span)

    # Class elements - results
    jd_results <- regarima_rslts(jd_clobj,jrobct,as.numeric(forecast))

    #remove the java objects
    if (exists("jd_clobj"))
      rm(jd_clobj)
    if (exists("jrobct"))
      rm(jrobct)

    # New S3 class "JD_RegArima"
    z<- list( specification = specification,
      arma=jd_results$arma,
      arima.coefficients =jd_results$arima.coefficients,
      regression.coefficients = jd_results$regression.coefficients,
      loglik=jd_results$loglik,
      model=jd_results$model,
      residuals = jd_results$residuals,
      residuals.stat = jd_results$residuals.stat,
      forecast = jd_results$forecast)

  class(z) <- c("JD_RegArima","X13")

  z
  }
}

# The function creates a "JD_RegArima" S3 class object from a JD+ defined specification for TRAMO-SEATS method
jd_regarima_defTS <-function(series, spec=c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5")){

  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)

  #create the java objects
  if (exists("jd_clobj"))
    rm(jd_clobj)
  if (exists("jrobct"))
    rm(jrobct)
  jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;", "getCore")
  jdictionary <- .jnull("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/regarima/Processor", "Lec/tstoolkit/jdr/regarima/Processor$Results;", "tramo", ts_r2jd(series), jspec, jdictionary)
  jrobct<- new (Class = "JD2_TRAMO_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{

    #Extract model specification from the java object
    rspec <- specTS_jd2r( spec = jrspec)

    # Specification list
    estimate<-data.frame(span = rspec$estimate.span, tolerance = rspec$estimate.tol, exact_ml = rspec$estimate.eml, urfinal = rspec$estimate.urfinal,
                         row.names = "", stringsAsFactors=FALSE)
    transform <- data.frame(tfunction=rspec$transform.function,fct=rspec$transform.fct,row.names = "", stringsAsFactors=FALSE)
    usrdef <- data.frame(outlier=FALSE, outlier.coef= FALSE, variables = FALSE, variables.coef = FALSE, stringsAsFactors=FALSE)
    userdef <-list(specification = usrdef, outliers = NA, variables = list(series = NA, description = NA))
    trading.days<-data.frame( automatic = rspec$tradingdays.mauto, pftd = rspec$tradingdays.pftd, option = rspec$tradingdays.option,
                              leapyear = rspec$tradingdays.leapyear,stocktd = rspec$tradingdays.stocktd, test = rspec$tradingdays.test,
                              row.names = "", stringsAsFactors=FALSE)
    easter<-data.frame(type=rspec$easter.type,julian=rspec$easter.julian,duration=rspec$easter.duration,test=rspec$easter.test,
                       row.names = "", stringsAsFactors=FALSE)
    regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
    outliers<-data.frame(enabled=rspec$outlier.enabled,span=rspec$outlier.span,ao=rspec$outlier.ao, tc=rspec$outlier.tc, ls = rspec$outlier.ls,
                         so=rspec$outlier.so,usedefcv=rspec$outlier.usedefcv,cv=rspec$outlier.cv,eml=rspec$outlier.eml,
                         tcrate=rspec$outlier.tcrate, row.names = "", stringsAsFactors=FALSE)
    arima.dsc <-data.frame(enabled=rspec$automdl.enabled,automdl.acceptdefault=rspec$automdl.acceptdefault,automdl.cancel=rspec$automdl.cancel,
                      automdl.ub1=rspec$automdl.ub1,automdl.ub2=rspec$automdl.ub2,automdl.armalimit=rspec$automdl.armalimit,
                      automdl.reducecv=rspec$automdl.reducecv, automdl.ljungboxlimit=rspec$automdl.ljungboxlimit, compare = rspec$automdl.compare,
                      arima.mu=rspec$arima.mu,arima.p=rspec$arima.p,arima.d =rspec$arima.d,arima.q=rspec$arima.q,
                      arima.bp=rspec$arima.bp,arima.bd=rspec$arima.bd,arima.bq=rspec$arima.bq, arima.coef = FALSE,
                      row.names = "", stringsAsFactors=FALSE)
   arima <- list(specification = arima.dsc, coefficients = NA)
   forecast <- data.frame(horizon = c(-2),row.names = c(""), stringsAsFactors=FALSE)
   span <-rspec$span

   specification <- list(estimate=estimate, transform=transform, regression=regression, outliers=outliers, arima=arima,
                          forecast = forecast, span=span)

    # Class elements - results
    jd_results <- regarima_rslts(jd_clobj,jrobct,as.numeric(forecast))

    #remove the java objects
    if (exists("jd_clobj"))
      rm(jd_clobj)
    if (exists("jrobct"))
      rm(jrobct)

    # New S3 class "JD_RegArima"
    z<- list( specification = specification,
              arma=jd_results$arma,
              arima.coefficients =jd_results$arima.coefficients,
              regression.coefficients = jd_results$regression.coefficients,
              loglik=jd_results$loglik,
              model=jd_results$model,
              residuals = jd_results$residuals,
              residuals.stat = jd_results$residuals.stat,
              forecast = jd_results$forecast)

    class(z) <- c("JD_RegArima","TRAMO_SEATS")

    z
  }
}

# Generic function to create a "JD_RegArima" S3 class object from a user-defined specification (for X13 or TRAMO-SEATS method)
jd_regarima<-function(series, spec = NA){
  if (!inherits(spec, "JD_RegArima_Spec")){
    stop("use only with \"JD_RegArima_Spec\" object", call. = FALSE)
  }else{
    UseMethod("jd_regarima", spec)
  }
}

# Method: "X13"
jd_regarima.X13 <-function(series, spec = NA){

  if (!is.ts(series))
    stop("series must be a time series")
  if (!inherits(spec, "JD_RegArima_Spec") | !inherits(spec, "X13"))
    stop("use only with c(\"JD_RegArima_Spec\",\"X13\") class object")

  # Create the java spec object
  if (exists("jd_clobj"))
    rm(jd_clobj)
  if (exists("jrobct"))
    rm(jrobct)
  jd_clobj <-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec <-.jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", "RG1")

  # Introduce modifications from the spec and create the java dictionary with the user-defined variables
  jdictionary <- specX13_r2jd(spec,jrspec)

  # Create the java regarima object
  jspec<-.jcall(jrspec, "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;", "getCore")
  jrslt<-.jcall("ec/tstoolkit/jdr/regarima/Processor", "Lec/tstoolkit/jdr/regarima/Processor$Results;", "x12", ts_r2jd(series), jspec, jdictionary)
  jrobct <- new (Class = "JD2_RegArima_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{

    # Class elements - results
    jd_results <- regarima_rslts(jd_clobj,jrobct,as.numeric(s_fcst(spec)))

    # Remove the java objects
    if (exists("jd_clobj"))
      rm(jd_clobj)
    if (exists("jrobct"))
      rm(jrobct)

    # Import the model specification
    estimate <- s_estimate(spec)
    transform <- s_transform(spec)
    usrdef <- s_usrdef(spec)
    predef.outliers <- s_preOut(spec)
    predef.variables <- s_preVar(spec)
    trading.days <- s_td(spec)
    easter <- s_easter(spec)
    outliers <- s_out(spec)
    arima.dsc <- s_arima(spec)
    predef.coef <- s_arimaCoef(spec)
    span <- s_span(spec)
    userdef <-list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
    regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
    arima <- list(specification = arima.dsc, coefficients = predef.coef)
    forecast <- s_fcst(spec)

    specification <- list(estimate=estimate, transform=transform, regression=regression,
                          outliers=outliers, arima=arima, forecast = forecast, span=span)

    # New S3 class "JD_RegArima"
    z<- list( specification = specification,
              arma=jd_results$arma,
              arima.coefficients =jd_results$arima.coefficients,
              regression.coefficients = jd_results$regression.coefficients,
              loglik=jd_results$loglik,
              model=jd_results$model,
              residuals = jd_results$residuals,
              residuals.stat = jd_results$residuals.stat,
              forecast = jd_results$forecast)

    class(z) = c("JD_RegArima","X13")

    z
  }
}

# Method: "TRAMO_SEATS"
jd_regarima.TRAMO_SEATS<-function(series, spec = NA){

  if (!is.ts(series))
    stop("series must be a time series")
  if (!inherits(spec, "JD_RegArima_Spec") | !inherits(spec, "TRAMO_SEATS"))
    stop("use only with c(\"JD_RegArima_Spec\",\"TRAMO_SEATS\") class object")

  # Create the spec java object
  if (exists("jd_clobj"))
    rm(jd_clobj)
  if (exists("jrobct"))
    rm(jrobct)
  jd_clobj <-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", "TR1")

  # Introduce modifications from the spec and create the java dictionary with the user-defined variables
  jdictionary <- specTS_r2jd(spec,jrspec)

  # Create the java object
  jspec<-.jcall(jrspec, "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;", "getCore")
  jrslt<-.jcall("ec/tstoolkit/jdr/regarima/Processor", "Lec/tstoolkit/jdr/regarima/Processor$Results;", "tramo", ts_r2jd(series), jspec, jdictionary)
  jrobct<- new (Class = "JD2_TRAMO_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{

    # Class elements - results
    jd_results <- regarima_rslts(jd_clobj,jrobct,as.numeric(s_fcst(spec)))

    #remove the java objects
    if (exists("jd_clobj"))
      rm(jd_clobj)
    if (exists("jrobct"))
      rm(jrobct)

    # Import the model specification
    estimate <- s_estimate(spec)
    transform <- s_transform(spec)
    usrdef <- s_usrdef(spec)
    predef.outliers <- s_preOut(spec)
    predef.variables <- s_preVar(spec)
    trading.days <- s_td(spec)
    easter <- s_easter(spec)
    outliers <- s_out(spec)
    arima.dsc <- s_arima(spec)
    predef.coef <- s_arimaCoef(spec)
    span <- s_span(spec)
    userdef <-list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
    regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
    arima <- list(specification = arima.dsc, coefficients = predef.coef)
    forecast <- s_fcst(spec)

    specification <- list(estimate=estimate, transform=transform, regression=regression,
                          outliers=outliers, arima=arima, forecast = forecast, span=span)

    # New S3 class "JD_RegArima"
    z<- list( specification = specification,
              arma=jd_results$arma,
              arima.coefficients =jd_results$arima.coefficients,
              regression.coefficients = jd_results$regression.coefficients,
              loglik=jd_results$loglik,
              model=jd_results$model,
              residuals = jd_results$residuals,
              residuals.stat = jd_results$residuals.stat,
              forecast = jd_results$forecast)

    class(z) = c("JD_RegArima","TRAMO_SEATS")

    z
  }
}
