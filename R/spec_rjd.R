# Functions to extract information from the java object

jd_span <- function(type,d0,d1,n0,n1){

  x <- if (type=="All") {"All"} else if (type=="From") {paste("From",d0, sep=" ")}
  else if (type=="To") {paste("Until",d1, sep=" ")}
  else if (type=="Between") {paste(d0,d1,sep=" - ")}
  else if (type=="First") {paste("All but first",n0,"periods", sep=" ")}
  else if (type=="Last") {paste("All but last",n1,"periods", sep=" ")}
  else if (type=="Excluding") {paste("All but first",n0,"periods and last",n1,"periods", sep=" ")}

  return(x)
}


spec_regarima_X13_jd2r <- function(spec = NA, context_dictionary = NULL,
                         extra_info = FALSE,
                         freq = NA){

  #Estimate
  preliminary.check <- spec$getBasic()$isPreliminaryCheck()

  jestimate <-.jcall(spec,"Ljdr/spec/x13/EstimateSpec;","getEstimate")
  jest.span <-.jcall(jestimate,"Ljdr/spec/ts/SpanSelector;","getSpan")

  estimate.type <- .jcall(jest.span,"S","getType")
  estimate.d0 <- .jcall(jest.span,"S","getD0")
  estimate.d1 <- .jcall(jest.span,"S","getD1")
  estimate.n0 <- .jcall(jest.span,"I","getN0")
  estimate.n1 <- .jcall(jest.span,"I","getN1")
  estimate.span <- jd_span(type= estimate.type,d0=estimate.d0,d1=estimate.d1,n0=estimate.n0,n1=estimate.n1)
  estimate.tol <-.jcall(jestimate ,"D","getTol")

  #Transform
  jtransform <-.jcall(spec,"Ljdr/spec/x13/TransformSpec;","getTransform")

  transform.function<-.jcall(jtransform,"S","getFunction")
  transform.adjust<-.jcall(jtransform,"S","getAdjust")
  transform.aicdiff<-.jcall(jtransform,"D","getAic")

  #Regression
  jregression<-.jcall(spec,"Ljdr/spec/x13/RegressionSpec;","getRegression")

  #Calendar
  jcalendar<-.jcall(jregression,"Ljdr/spec/x13/CalendarSpec;","getCalendar")
  jtd<-.jcall(jcalendar,"Ljdr/spec/x13/TradingDaysSpec;","getTradingDays")
  jeaster<-.jcall(jcalendar,"Ljdr/spec/x13/EasterSpec;","getEaster")
  tradingdays.option <- .jcall(jtd,"S","getOption")
  if(tradingdays.option != "UserDefined"){
    tradingdays.option <- .jcall(jtd,"S","getTradingDays")
  }

  tradingdays.autoadjust <- .jcall(jtd,"Z","isAutoAdjust")
  tradingdays.leapyear <- .jcall(jtd,"S","getLengthOfPeriod")
  tradingdays.stocktd <- .jcall(jtd,"I","getW")
  tradingdays.test <- .jcall(jtd,"S","getTest")
  easter.enabled <- .jcall(jeaster,"Z","isEnabled")
  easter.julian <- .jcall(jeaster,"Z","isJulian")
  easter.duration <- .jcall(jeaster,"I","getDuration")
  easter.test <- .jcall(jeaster,"S","getTest")

  #Outlier
  joutlier <- .jcall(spec,"Ljdr/spec/x13/OutlierSpec;","getOutliers")
  joutlier.span <- .jcall(joutlier,"Ljdr/spec/ts/SpanSelector;","getSpan")

  outlier.enabled <- .jcall(joutlier,"Z","isEnabled")
  outlier.type <- .jcall(joutlier.span,"S","getType")
  outlier.d0 <- .jcall(joutlier.span,"S","getD0")
  outlier.d1 <- .jcall(joutlier.span,"S","getD1")
  outlier.n0 <- .jcall(joutlier.span,"I","getN0")
  outlier.n1 <- .jcall(joutlier.span,"I","getN1")
  outlier.span <- jd_span(type = outlier.type,
                          d0 = outlier.d0, d1 = outlier.d1,
                          n0 = outlier.n0, n1 = outlier.n1)
  outlier.ao <- .jcall(joutlier, "Z", "isAO")
  outlier.tc <- .jcall(joutlier, "Z", "isTC")
  outlier.ls <- .jcall(joutlier, "Z", "isLS")
  outlier.so <- .jcall(joutlier, "Z", "isSO")
  outlier.usedefcv <- .jcall(joutlier, "Z", "isDefaultVa")
  outlier.cv <- .jcall(joutlier, "D", "getVa")
  outlier.method <- .jcall(joutlier, "S", "getMethod")
  outlier.tcrate <- .jcall(joutlier, "D", "getTCRate")

  #Arima
  jarima <- .jcall(spec, "Ljdr/spec/x13/ArimaSpec;", "getArima")

  automdl.enabled <- .jcall(jarima, "Z", "isAmiEnabled")
  automdl.acceptdefault <- .jcall(jarima,"Z","isAcceptDefault")
  automdl.cancel <- .jcall(jarima,"D","getCancel")
  automdl.ub1 <- .jcall(jarima,"D","getUb1")
  automdl.ub2 <- .jcall(jarima,"D","getUb2")
  automdl.mixed  <- .jcall(jarima,"Z","isMixed")
  automdl.balanced <- .jcall(jarima,"Z","isBalanced")
  automdl.armalimit <- .jcall(jarima,"D","getTsig")
  automdl.reducecv <- .jcall(jarima,"D","getPredCV")
  automdl.ljungboxlimit <- .jcall(jarima,"D","getPcr")
  automdl.ubfinal <- .jcall(jarima,"D","getUbFinal")
  arima.mu <- .jcall(jarima,"Z","isMean")
  arima.p <- .jcall(jarima,"I","getP")
  arima.d <- .jcall(jarima,"I","getD")
  arima.q <- .jcall(jarima,"I","getQ")
  arima.bp <- .jcall(jarima,"I","getBP")
  arima.bd <- .jcall(jarima,"I","getBD")
  arima.bq <- .jcall(jarima,"I","getBQ")

  #span matrix
  type <- c(estimate.type,outlier.type)
  d0 <- c(estimate.d0,outlier.d0)
  d1 <- c(estimate.d1,outlier.d1)
  n0 <- c(estimate.n0,outlier.n0)
  n1 <- c(estimate.n1,outlier.n1)

  span <- data.frame(type=type,d0=d0,d1=d1,n0=n0,n1=n1)
  rownames(span) <- c("estimate","outlier")

  #Default values:

  arima.coef <- FALSE
  userdef_spec <- list(specification = data.frame(outlier = FALSE,
                                                 outlier.coef = FALSE,
                                                 variables = FALSE,
                                                 variables.coef = FALSE,
                                                 stringsAsFactors = FALSE),
                      outliers = NA, variables = list(series = NA, description = NA))
  arima.coef.spec <- NA
  result <- list(preliminary.check = preliminary.check, estimate.type = estimate.type, estimate.d0 = estimate.d0, estimate.d1 = estimate.d1,
       estimate.n0 = estimate.n0 , estimate.n1 = estimate.n1, estimate.span = estimate.span, estimate.tol = estimate.tol,
       transform.function = transform.function, transform.adjust = transform.adjust, transform.aicdiff = transform.aicdiff,
       tradingdays.option = tradingdays.option , tradingdays.autoadjust = tradingdays.autoadjust,
       tradingdays.leapyear = tradingdays.leapyear , tradingdays.stocktd = tradingdays.stocktd, tradingdays.test = tradingdays.test,
       easter.enabled = easter.enabled , easter.julian = easter.julian , easter.duration = easter.duration, easter.test = easter.test,
       outlier.enabled = outlier.enabled, outlier.type = outlier.type, outlier.d0 = outlier.d0, outlier.d1 = outlier.d1,
       outlier.n0 = outlier.n0, outlier.n1 = outlier.n1, outlier.span = outlier.span, outlier.ao = outlier.ao,
       outlier.tc = outlier.tc, outlier.ls = outlier.ls, outlier.so = outlier.so, outlier.usedefcv = outlier.usedefcv,
       outlier.cv = outlier.cv, outlier.method = outlier.method, outlier.tcrate = outlier.tcrate,
       automdl.enabled = automdl.enabled, automdl.acceptdefault = automdl.acceptdefault, automdl.cancel = automdl.cancel,
       automdl.ub1 = automdl.ub1, automdl.ub2 = automdl.ub2, automdl.mixed  = automdl.mixed , automdl.balanced = automdl.balanced ,
       automdl.armalimit = automdl.armalimit, automdl.reducecv = automdl.reducecv, automdl.ljungboxlimit = automdl.ljungboxlimit,
       automdl.ubfinal = automdl.ubfinal, arima.mu = arima.mu, arima.p = arima.p, arima.d = arima.d, arima.q = arima.q,
       arima.bp = arima.bp, arima.bd = arima.bd, arima.bq = arima.bq, span = span,
       arima.coef = arima.coef,arima.coef.spec = arima.coef.spec,
       userdef_spec = userdef_spec)

  # Extra info importing from a workspace
  if(!extra_info)
    return(result)

  n_prespecified_out <- .jcall(jregression,"I","getPrespecifiedOutliersCount")

  if(n_prespecified_out > 0 ){

    result$userdef_spec$specification$outlier <- TRUE

    outliers <- lapply(1:n_prespecified_out, function(i){
      .jcall(jregression,
             "Ljdr/spec/ts/Utility$Outlier;",
             "getPrespecifiedOutlier",
             as.integer(i-1))
    })
    type <- sapply(outliers, function(x) x$getCode())
    date <- sapply(outliers, function(x) x$getPosition())
    coeff <- sapply(outliers, function(x) x$getCoefficient())
    if(all(coeff == 0)){ #All coefficients are equal to 0: they are not fixed
      result$userdef_spec$specification$outlier.coef <- FALSE
      coeff <- coeff * NA
    }else{
      result$userdef_spec$specification$outlier.coef <- TRUE
    }
    outlier_spec <- data.frame(type = type, date = date, coeff = coeff)

    result$userdef_spec$outliers <- outlier_spec

  }

  n_userdefined_var <- .jcall(jregression,"I","getUserDefinedVariablesCount")

  if(n_userdefined_var > 0 ){
    result$userdef_spec$specification$variables <- TRUE

    ud_vars <- lapply(1:n_userdefined_var, function(i){
      .jcall(jregression,
             "Ljdr/spec/ts/Utility$UserDefinedVariable;",
             "getUserDefinedVariable",
             as.integer(i-1))
    })

    type <- sapply(ud_vars, function(x) x$getComponent())
    coeff <- sapply(ud_vars, function(x) x$getCoefficient())
    var_names <- sapply(ud_vars, function(x) x$getName())
    var_names_split <- strsplit(var_names,"[.]")
    var_names <- sapply(var_names_split, function(x) x[2])
    var_names <- base::make.names(var_names, unique = TRUE)
    var_names <- gsub(".","_", var_names, fixed = TRUE)

    if(all(coeff == 0)){ #All coefficients are equal to 0: they are not fixed
      result$userdef_spec$specification$variables.coef <- FALSE
      coeff <- coeff * NA
    }else{
      result$userdef_spec$specification$variables.coef <- TRUE
    }

    result$userdef_spec$variables$description <- data.frame(type = type,
                                                            coeff = coeff,
                                                            row.names = var_names)

    # To check variables group names
    # t <- context_dictionary$getTsVariableManagers()
    # t$getNames()
    #
    if(!is.null(context_dictionary)){

      var_series <- lapply(var_names_split,function(names){
        ts_variable <- context_dictionary$getTsVariable(names[1],
                                                         names[2])
        ts_jd2r(ts_variable$getTsData())
      })
      var_series <- ts(simplify2array(var_series),
                       start = start(var_series[[1]]), frequency = frequency(var_series[[1]]))

      if(is.mts(var_series))
        colnames(var_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- var_series
    }

  }

  #Calendar
  user_td <- jtd$getUserVariables()
  if(length(user_td) > 0 ){
    var_names_split <- strsplit(user_td,"[.]")
    var_names <- sapply(var_names_split, function(x) x[2])
    var_names <- base::make.names(var_names, unique = TRUE)
    var_names <- gsub(".","_", var_names, fixed = TRUE)

    result$userdef_spec$specification$variables <-
      TRUE

    td_var_description <- data.frame(type = rep("Calendar",length(var_names)),
                                     coeff = NA, row.names = var_names)
    if(identical_na(result$userdef_spec$variables$description)){
      result$userdef_spec$variables$description <- td_var_description
    }else{
      result$userdef_spec$variables$description <- rbind(result$userdef_spec$variables$description,
                                                         td_var_description)
    }

    if(!is.null(context_dictionary)){

      var_series <- lapply(var_names_split,function(names){
        ts_variable <- context_dictionary$getTsVariable(names[1],
                                                         names[2])
        ts_jd2r(ts_variable$getTsData())
      })
      var_series <- ts(simplify2array(var_series),
                       start = start(var_series[[1]]), frequency = frequency(var_series[[1]]))
      if(!identical_na(result$userdef_spec$variables$series)){
        var_series <- ts.union(result$userdef_spec$variables$series, var_series)
      }
      if(is.mts(var_series))
        colnames(var_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- var_series
    }
  }

  ## Ramp effects
  core_regression <- jregression$getCore()$getRegression()
  jramps <- core_regression$getRamps()
  nb_ramps <- core_regression$getRampsCount()
  if (nb_ramps > 0) {
    var_names <- sapply(seq_len(nb_ramps), function(x) jramps[[x]]$getDescription())

    result$userdef_spec$specification$variables <-
      TRUE

    coeff <- NA
    if (core_regression$hasFixedCoefficients()) {
      coeff <- sapply(seq_len(nb_ramps), function(i){
        jramp <- jramps[[i]]
        ramp_name <- jramp$getName()
        fixed_coeff <- core_regression$getFixedCoefficients(ramp_name)
        if (is.null(fixed_coeff)) {
          NA
        }else{
          fixed_coeff
        }
      })
      result$userdef_spec$specification$variables.coef <- TRUE
    }

    td_var_description <- data.frame(type = rep("Series", length(var_names)),
                                     coeff = coeff, row.names = var_names)
    if (identical_na(result$userdef_spec$variables$description)) {
      result$userdef_spec$variables$description <-
        td_var_description
    }else{
      result$userdef_spec$variables$description <-
        rbind(result$userdef_spec$variables$description,
              td_var_description)
    }

    if (!is.na(freq) || !identical_na(result$userdef_spec$variables$series)) {
      if (is.na(freq))
        freq <- frequency(result$userdef_spec$variables$series)

      if (!identical_na(result$userdef_spec$variables$series)) {
        start_ts <- start(result$userdef_spec$variables$series)
        end_ts <- end(result$userdef_spec$variables$series)
        ramp_series <- lapply(seq_len(nb_ramps), function(i){
          jramp <- jramps[[i]]
          jstart <- jramp$getStart()
          jend <- jramp$getEnd()

          start_ramp <- c(jstart$getYear(), jstart$getMonth())
          end_ramp <- c(jend$getYear(), jend$getMonth())
          ramp(start = start_ts, end = end_ts,
               start_ramp = start_ramp, end_ramp = end_ramp,
               frequency = freq)
        })
      } else {
        ramp_series <- lapply(seq_len(nb_ramps), function(i){
          jramp <- jramps[[i]]
          jstart <- jramp$getStart()
          jend <- jramp$getEnd()

          start_ramp <- c(jstart$getYear(), jstart$getMonth())
          end_ramp <- c(jend$getYear(), jend$getMonth())
          ramp(start_ramp = start_ramp, end_ramp = end_ramp,
               frequency = freq)
        })
      }
      ramp_series <- ts(simplify2array(ramp_series),
                        start = start(ramp_series[[1]]),
                        frequency = frequency(ramp_series[[1]]))
      if (!identical_na(result$userdef_spec$variables$series)) {
        ramp_series <- ts.union(result$userdef_spec$variables$series, ramp_series)
      }
      if (is.mts(ramp_series))
        colnames(ramp_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- ramp_series
    }
  }

  Phi <- jarima$getPhi()
  BPhi <- jarima$getBPhi()
  Theta <- jarima$getTheta()
  BTheta <- jarima$getBTheta()
  arima_coefficients_spec <-
    rbind(arimaCoef_jd2r(Phi),
          arimaCoef_jd2r(BPhi),
          arimaCoef_jd2r(Theta),
          arimaCoef_jd2r(BTheta))

  if(!is.null(arima_coefficients_spec) &&
     any(arima_coefficients_spec$Type != "Undefined")){
    result$arima.coef <- TRUE
    result$arima.coef.spec <- arima_coefficients_spec
  }

  result
}

spec_TRAMO_jd2r <- function(spec = NA, context_dictionary = NULL,
                       extra_info = FALSE, freq = NA){

  #Estimate
  preliminary.check <- spec$getBasic()$isPreliminaryCheck()
  jestimate <- .jcall(spec,"Ljdr/spec/tramoseats/EstimateSpec;","getEstimate")
  jest.span <- .jcall(jestimate,"Ljdr/spec/ts/SpanSelector;","getSpan")

  estimate.type <- .jcall(jest.span,"S","getType")
  estimate.d0 <- .jcall(jest.span,"S","getD0")
  estimate.d1 <- .jcall(jest.span,"S","getD1")
  estimate.n0 <- .jcall(jest.span,"I","getN0")
  estimate.n1 <- .jcall(jest.span,"I","getN1")
  estimate.span <- jd_span(type= estimate.type,d0=estimate.d0,d1=estimate.d1,n0=estimate.n0,n1=estimate.n1)
  estimate.tol <-.jcall(jestimate ,"D","getTol")
  estimate.eml <-.jcall(jestimate ,"Z","isEml")
  estimate.urfinal <-.jcall(jestimate ,"D","getUbp")

  #Transform
  jtransform <-.jcall(spec,"Ljdr/spec/tramoseats/TransformSpec;","getTransform")

  transform.function <-.jcall(jtransform,"S","getFunction")
  transform.fct <-.jcall(jtransform,"D","getFct")

  #Regression
  jregression<-.jcall(spec,"Ljdr/spec/tramoseats/RegressionSpec;","getRegression")

  #Calendar
  jcalendar<-.jcall(jregression,"Ljdr/spec/tramoseats/CalendarSpec;","getCalendar")
  jtd<-.jcall(jcalendar,"Ljdr/spec/tramoseats/TradingDaysSpec;","getTradingDays")
  jeaster<-.jcall(jcalendar,"Ljdr/spec/tramoseats/EasterSpec;","getEaster")

  tradingdays.mauto <-.jcall(jtd,"S","getAutomatic")
  tradingdays.pftd <-.jcall(jtd,"D","getPftd")
  tradingdays.option <- .jcall(jtd,"S","getOption")
  if(tradingdays.option != "UserDefined"){
    tradingdays.option <- .jcall(jtd,"S","getTradingDays")
  }
  tradingdays.leapyear <-.jcall(jtd,"Z","getLeapYear")
  tradingdays.stocktd <-.jcall(jtd,"I","getW")
  tradingdays.test <-.jcall(jtd,"S","getRegressionTestType")
  easter.type <- .jcall(jeaster,"S","getOption")
  easter.julian <-.jcall(jeaster,"Z","isJulian")
  easter.duration <-.jcall(jeaster,"I","getDuration")
  easter.test <-.jcall(jeaster,"Z","isTest")

  #Outlier
  joutlier<-.jcall(spec,"Ljdr/spec/tramoseats/OutlierSpec;","getOutlier")
  joutlier.span <-.jcall(joutlier,"Ljdr/spec/ts/SpanSelector;","getSpan")

  outlier.enabled <-.jcall(joutlier,"Z","isOutliersDetectionEnabled")
  outlier.type <- .jcall(joutlier.span,"S","getType")
  outlier.d0 <- .jcall(joutlier.span,"S","getD0")
  outlier.d1 <- .jcall(joutlier.span,"S","getD1")
  outlier.n0 <- .jcall(joutlier.span,"I","getN0")
  outlier.n1 <- .jcall(joutlier.span,"I","getN1")
  outlier.span <- jd_span(type= outlier.type,d0=outlier.d0,d1=outlier.d1,n0=outlier.n0,n1=outlier.n1)
  outlier.ao <-.jcall(joutlier,"Z","isAO")
  outlier.tc <-.jcall(joutlier,"Z","isTC")
  outlier.ls <-.jcall(joutlier,"Z","isLS")
  outlier.so <-.jcall(joutlier,"Z","isSO")
  outlier.usedefcv <-.jcall(joutlier,"Z","isAutoVa")
  outlier.cv <-.jcall(joutlier,"D","getVa")
  outlier.eml <-.jcall(joutlier,"Z","isEML")
  outlier.tcrate <-.jcall(joutlier,"D","getTCRate")

  #Arima
  jarima<-.jcall(spec,"Ljdr/spec/tramoseats/ArimaSpec;","getArima")

  automdl.enabled <-.jcall(jarima,"Z","isEnabled")
  automdl.acceptdefault <-.jcall(jarima,"Z","isAcceptDefault")
  automdl.cancel <-.jcall(jarima,"D","getCancel")
  automdl.ub1 <-.jcall(jarima,"D","getUb1")
  automdl.ub2 <-.jcall(jarima,"D","getUb2")
  automdl.armalimit <-.jcall(jarima,"D","getTsig")
  automdl.reducecv <-.jcall(jarima,"D","getPc")
  automdl.ljungboxlimit <-.jcall(jarima,"D","getPcr")
  automdl.compare <-.jcall(jarima,"Z","isAmiCompare")
  arima.mu <-.jcall(jarima,"Z","isMean")
  arima.p <-.jcall(jarima,"I","getP")
  arima.d <-.jcall(jarima,"I","getD")
  arima.q <-.jcall(jarima,"I","getQ")
  arima.bp <-.jcall(jarima,"I","getBP")
  arima.bd <-.jcall(jarima,"I","getBD")
  arima.bq <-.jcall(jarima,"I","getBQ")

  #span matrix
  type<-c(estimate.type,outlier.type)
  d0<-c(estimate.d0,outlier.d0)
  d1<-c(estimate.d1,outlier.d1)
  n0<-c(estimate.n0,outlier.n0)
  n1<-c(estimate.n1,outlier.n1)

  span<-data.frame(type=type,d0=d0,d1=d1,n0=n0,n1=n1)
  rownames(span)<-c("estimate","outlier")

  arima.coef <- FALSE
  userdef_spec <-list(specification = data.frame(outlier = FALSE,
                                                 outlier.coef = FALSE,
                                                 variables = FALSE,
                                                 variables.coef = FALSE,
                                                 stringsAsFactors = FALSE),
                      outliers = NA, variables = list(series = NA, description = NA))
  arima.coef.spec <- NA

  result <- list(preliminary.check = preliminary.check,
                 estimate.type = estimate.type,estimate.d0 = estimate.d0,estimate.d1 = estimate.d1,estimate.n0 = estimate.n0,
                 estimate.n1 = estimate.n1,estimate.span = estimate.span,estimate.tol = estimate.tol,estimate.eml = estimate.eml,
                 estimate.urfinal = estimate.urfinal,transform.function = transform.function,transform.fct = transform.fct,
                 tradingdays.mauto = tradingdays.mauto,tradingdays.pftd = tradingdays.pftd,tradingdays.option = tradingdays.option,
                 tradingdays.leapyear = tradingdays.leapyear,tradingdays.stocktd = tradingdays.stocktd,
                 tradingdays.test = tradingdays.test,easter.type = easter.type,easter.julian = easter.julian,easter.duration = easter.duration,
                 easter.test = easter.test,outlier.enabled = outlier.enabled,outlier.type = outlier.type,outlier.d0 = outlier.d0,
                 outlier.d1 = outlier.d1,outlier.n0 = outlier.n0,outlier.n1 = outlier.n1,outlier.span = outlier.span,outlier.ao = outlier.ao,
                 outlier.tc = outlier.tc,outlier.ls = outlier.ls,outlier.so = outlier.so,outlier.usedefcv = outlier.usedefcv,
                 outlier.cv = outlier.cv,outlier.eml = outlier.eml,outlier.tcrate = outlier.tcrate,automdl.enabled = automdl.enabled,
                 automdl.acceptdefault = automdl.acceptdefault,automdl.cancel = automdl.cancel,automdl.ub1 = automdl.ub1,
                 automdl.ub2 = automdl.ub2,automdl.armalimit = automdl.armalimit,automdl.reducecv = automdl.reducecv,
                 automdl.ljungboxlimit = automdl.ljungboxlimit,automdl.compare = automdl.compare,arima.mu = arima.mu,arima.p = arima.p,
                 arima.d = arima.d,arima.q = arima.q,arima.bp = arima.bp,arima.bd = arima.bd,arima.bq = arima.bq, span = span,
                 arima.coef = arima.coef,arima.coef.spec = arima.coef.spec,
                 userdef_spec = userdef_spec)

  # Extra info importing from a workspace
  if(!extra_info)
    return(result)

  n_prespecified_out <- .jcall(jregression,"I","getPrespecifiedOutliersCount")
  if(n_prespecified_out > 0 ){
    # Outlier.coef is set to TRUE: if the coefficient isn't fixed, the
    # coef value will be equal to 0 and the outlier will be estimated
    result$userdef_spec$specification$outlier <- TRUE

    outliers <- lapply(1:n_prespecified_out, function(i){
      .jcall(jregression,
             "Ljdr/spec/ts/Utility$Outlier;",
             "getPrespecifiedOutlier",
             as.integer(i-1))
    })
    type <- sapply(outliers, function(x) x$getCode())
    date <- sapply(outliers, function(x) x$getPosition())
    coeff <- sapply(outliers, function(x) x$getCoefficient())

    if(all(coeff == 0)){ #All coefficients are equal to 0: they are not fixed
      result$userdef_spec$specification$outlier.coef <- FALSE
      coeff <- coeff * NA
    }else{
      result$userdef_spec$specification$outlier.coef <- TRUE
    }
    outlier_spec <- data.frame(type = type, date = date, coeff = coeff)

    result$userdef_spec$outliers <- outlier_spec

  }

  n_userdefined_var <- .jcall(jregression,"I","getUserDefinedVariablesCount")
  if(n_userdefined_var > 0 ){

    # variables.coef is set to TRUE: if the coefficient isn't fixed, the
    # coef value will be equal to 0 and the variable will be estimated
    result$userdef_spec$specification$variables <- TRUE

    ud_vars <- lapply(1:n_userdefined_var, function(i){
      .jcall(jregression,
             "Ljdr/spec/ts/Utility$UserDefinedVariable;",
             "getUserDefinedVariable",
             as.integer(i-1))
    })

    type <- sapply(ud_vars, function(x) x$getComponent())
    coeff <- sapply(ud_vars, function(x) x$getCoefficient())
    var_names <- sapply(ud_vars, function(x) x$getName())
    var_names_split <- strsplit(var_names,"[.]")
    var_names <- sapply(var_names_split, function(x) x[2])

    if(all(coeff == 0)){ #All coefficients are equal to 0: they are not fixed
      result$userdef_spec$specification$variables.coef <- FALSE
      coeff <- coeff * NA
    }else{
      result$userdef_spec$specification$variables.coef <- TRUE
    }

    result$userdef_spec$variables$description <- data.frame(type = type,
                                                            coeff = coeff,
                                                            row.names = var_names)

    # To check variables group names
    # t <- context_dictionary$getTsVariableManagers()
    # t$getNames()
    #
    if(!is.null(context_dictionary)){

      var_series <- lapply(var_names_split,function(names){
        ts_variable <- context_dictionary$getTsVariable(names[1],
                                                         names[2])
        ts_jd2r(ts_variable$getTsData())
      })
      var_series <- ts(simplify2array(var_series),
                       start = start(var_series[[1]]), frequency = frequency(var_series[[1]]))

      if(is.mts(var_series))
        colnames(var_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- var_series
    }

  }

  #Calendar
  user_td <- jtd$getUserVariables()
  if(length(user_td) > 0 ){
    var_names_split <- strsplit(user_td,"[.]")
    var_names <- sapply(var_names_split, function(x) x[2])

    result$userdef_spec$specification$variables <-
      TRUE
    coeff <- NA
    # if (core_regression$hasFixedCoefficients()) {
    #   # coeff <- sapply(seq_len(nb_ramps), function(i){
    #   #   jramp <- jramps[[i]]
    #   #   ramp_name <- jramp$getName()
    #   #   fixed_coeff <- core_regression$getFixedCoefficients(ramp_name)
    #   #   if (is.null(fixed_coeff)) {
    #   #     NA
    #   #   }else{
    #   #     fixed_coeff
    #   #   }
    #   # })
    #   # result$userdef_spec$specification$variables.coef <- TRUE
    # }

    td_var_description <- data.frame(type = rep("Calendar",length(var_names)),
                                     coeff = coeff, row.names = var_names)
    if(identical_na(result$userdef_spec$variables$description)){
      result$userdef_spec$variables$description <- td_var_description
    }else{
      result$userdef_spec$variables$description <- rbind(result$userdef_spec$variables$description,
                                                         td_var_description)
    }

    if(!is.null(context_dictionary)){

      var_series <- lapply(var_names_split,function(names){
        ts_variable <- context_dictionary$getTsVariable(names[1],
                                                         names[2])
        ts_jd2r(ts_variable$getTsData())
      })
      var_series <- ts(simplify2array(var_series),
                       start = start(var_series[[1]]), frequency = frequency(var_series[[1]]))
      if(!identical_na(result$userdef_spec$variables$series)){
        var_series <- ts.union(result$userdef_spec$variables$series, var_series)
      }
      if(is.mts(var_series))
        colnames(var_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- var_series
    }
  }

  ## Ramp effects
  core_regression <- jregression$getCore()$getRegression()
  jramps <- core_regression$getRamps()
  nb_ramps <- core_regression$getRampsCount()
  if (nb_ramps > 0) {
    var_names <- sapply(seq_len(nb_ramps), function(x) jramps[[x]]$getDescription())

    result$userdef_spec$specification$variables <-
      TRUE
    coeff <- NA
    if (core_regression$hasFixedCoefficients()) {
      coeff <- sapply(seq_len(nb_ramps), function(i){
        jramp <- jramps[[i]]
        ramp_name <- jramp$getName()
        fixed_coeff <- core_regression$getFixedCoefficients(ramp_name)
        if (is.null(fixed_coeff)) {
          NA
        }else{
          fixed_coeff
        }
      })
      result$userdef_spec$specification$variables.coef <- TRUE
    }

    td_var_description <- data.frame(type = rep("Series", length(var_names)),
                                     coeff = coeff, row.names = var_names)
    if (identical_na(result$userdef_spec$variables$description)) {
      result$userdef_spec$variables$description <-
        td_var_description
    }else{
      result$userdef_spec$variables$description <-
        rbind(result$userdef_spec$variables$description,
              td_var_description)
    }

    if (!is.na(freq) || !identical_na(result$userdef_spec$variables$series)) {
      if (is.na(freq))
        freq <- frequency(result$userdef_spec$variables$series)

      if (!identical_na(result$userdef_spec$variables$series)) {
        start_ts <- start(result$userdef_spec$variables$series)
        end_ts <- end(result$userdef_spec$variables$series)
        ramp_series <- lapply(seq_len(nb_ramps), function(i){
          jramp <- jramps[[i]]
          jstart <- jramp$getStart()
          jend <- jramp$getEnd()

          start_ramp <- c(jstart$getYear(), jstart$getMonth())
          end_ramp <- c(jend$getYear(), jend$getMonth())
          ramp(start = start_ts, end = end_ts,
               start_ramp = start_ramp, end_ramp = end_ramp,
               frequency = freq)
        })
      } else {
        ramp_series <- lapply(seq_len(nb_ramps), function(i){
          jramp <- jramps[[i]]
          jstart <- jramp$getStart()
          jend <- jramp$getEnd()

          start_ramp <- c(jstart$getYear(), jstart$getMonth())
          end_ramp <- c(jend$getYear(), jend$getMonth())
          ramp(start_ramp = start_ramp, end_ramp = end_ramp,
               frequency = freq)
        })
      }
      ramp_series <- ts(simplify2array(ramp_series),
                       start = start(ramp_series[[1]]),
                       frequency = frequency(ramp_series[[1]]))
      if (!identical_na(result$userdef_spec$variables$series)) {
        ramp_series <- ts.union(result$userdef_spec$variables$series, ramp_series)
      }
      if (is.mts(ramp_series))
        colnames(ramp_series) <- rownames(result$userdef_spec$variables$description)

      result$userdef_spec$variables$series <- ramp_series
    }
  }

  #Arima
  Phi <- jarima$getPhi()
  BPhi <- jarima$getBPhi()
  Theta <- jarima$getTheta()
  BTheta <- jarima$getBTheta()
  arima_coefficients_spec <-
    rbind(arimaCoef_jd2r(Phi),
          arimaCoef_jd2r(BPhi),
          arimaCoef_jd2r(Theta),
          arimaCoef_jd2r(BTheta))

  if(!is.null(arima_coefficients_spec) &&
     any(arima_coefficients_spec$Type != "Undefined")){
    result$arima.coef <- TRUE
    result$arima.coef.spec <- arima_coefficients_spec
  }

  result
}

specX11_jd2r <- function(spec = NA, freq = NA){
  jx11 <- .jcall(spec,"Ljdr/spec/x13/X11Spec;","getX11")

  if (!is.na(freq)) {
    .jcall(jx11,"V","setFreq", as.integer(freq))
    fullseasonalma <- .jcall(jx11,"[S","getFullSeasonalMA")
    if (!is.null(fullseasonalma) && length(unique(fullseasonalma)) > 1) {
      seasonalma <- paste(fullseasonalma, collapse = ", ")
    }else{
      seasonalma <- .jcall(jx11,"S","getSeasonalMA")
    }
  }else{
    seasonalma <- .jcall(jx11,"S","getSeasonalMA")
  }

  mode <- .jcall(jx11,"S","getMode")
  seasonalComp <- .jcall(jx11,"Z","isSeasonal")
  lsigma <- .jcall(jx11,"D","getLSigma")
  usigma <- .jcall(jx11,"D","getUSigma")
  trendAuto <- .jcall(jx11,"Z","isAutoTrendMA")
  trendma <- .jcall(jx11,"I","getTrendMA")
  fcasts <- .jcall(jx11,"I","getForecastHorizon")
  bcasts <- .jcall(jx11,"I","getBackcastHorizon")
  excludeFcasts <- .jcall(jx11,"Z","isExcludefcst")
  calendarSigma <- jx11$getCalendarSigma()$toString()
  sigmaVector <- .jcall(jx11,returnSig = "[S", "getSigmavec") #TODO
  if (length(sigmaVector) == 0) {
    sigmaVector <- NA
  }else{
    sigmaVector <- paste(sigmaVector, collapse = ", ")
  }

  var <- data.frame(mode,seasonalComp,lsigma,usigma,trendAuto,trendma,seasonalma,
                    fcasts, bcasts,
                    calendarSigma, sigmaVector,
                    excludeFcasts,
                    stringsAsFactors = FALSE)
  names(var) <- sprintf("x11.%s",
                        c("mode","seasonalComp","lsigma","usigma",
                          "trendAuto","trendma","seasonalma",
                          "fcasts","bcasts",
                          "calendarSigma", "sigmaVector",
                          "excludeFcasts"))
  return(var)
}

specSeats_jd2r <- function(spec = NA){

  jseats <- .jcall(spec,"Ljdr/spec/tramoseats/SeatsSpec;","getSeats")
  predictionLength <- .jcall(jseats,"I","getPredictionLength")
  approx <- .jcall(jseats,"S","getApproximationMode")
  maBoundary <- .jcall(jseats,"D","getXl")
  trendBoundary <- .jcall(jseats,"D","getRMod")
  seasdBoundary <- .jcall(jseats,"D","getSMod")
  seasdBoundary1 <- .jcall(jseats,"D","getSMod1")
  seasTol <- .jcall(jseats,"D","getEpsPhi")
  method <- .jcall(jseats,"S","getMethod")

  var <- list(predictionLength, approx,maBoundary,trendBoundary,seasdBoundary,seasdBoundary1,seasTol,
              method)
  names(var) <- c("predictionLength", "approx", "maBoundary", "trendBoundary",
                  "seasdBoundary","seasdBoundary1","seasTol",
                  "method")
  return(var)

}

# Functions to introduce modifications in the java object
span_r2jd <- function(jsobjct = NA, type = NA, d0=NA, d1=NA, n0 = NA, n1=NA){
  if (type =="All") {
    .jcall(jsobjct,"V","all")
  } else if (type=="From"){
    .jcall(jsobjct,"V","from",d0)
  } else if (type=="To"){
    .jcall(jsobjct,"V","to",d1)
  } else if (type=="Between"){
    .jcall(jsobjct,"V","between",d0,d1)
  } else if (type=="First"){
    .jcall(jsobjct,"V","first",n0)
  } else if (type=="Last"){
    .jcall(jsobjct,"V","last",n1)
  } else if (type=="Excluding"){
    .jcall(jsobjct,"V","excluding",n0,n1)
  }
}

preOut_r2jd <- function(jsobjct = NA, coefEna = NA, out = NA, outDate = NA, outCoef = NA){
  coef <- if (coefEna == TRUE) {outCoef} else {rep(0,length(out))}
  for (i in 1:length(out)){
    .jcall(jsobjct,"V","addPrespecifiedOutlier", out[i],outDate[i],coef[i])
  }
}

preVar_r2jd <- function(jsobjct = NA, jsdict = NA, coefEna = NA,
                        prevar_spec = list(series = NA, description = data.frame(NA,NA)),
                        jtd = NA) {
  series = prevar_spec$series
  varType = prevar_spec$description[,1]
  varCoef = prevar_spec$description[,2]
  varNames <- rownames(prevar_spec$description)

  nvar <- if (is.mts(series)) {dim(series)[2]} else if (is.ts(series)) {1} else {0}
  type <- if (all(is.na(varType))) {rep("Undefined",nvar)} else {as.character(varType)}
  coef <- if (coefEna == TRUE) {varCoef} else {rep(0,nvar)}

  if(nvar == 0)
    return(NULL)

  calendar_def <- grep("Calendar",type)
  n_calendar_def <- length(calendar_def)
  if (nvar == 1){
    if(n_calendar_def == 1){
      .jcall(jsdict,"V","add",varNames,ts_r2jd(series))
      .jcall(jtd,"V","setUserVariables", .jarray(paste0("r.",varNames)))

    }else{
      .jcall(jsdict,"V","add",varNames,ts_r2jd(series))
      .jcall(jsobjct,"V","addUserDefinedVariable",varNames,type[1],coef[1])
    }

  }else{
    if(n_calendar_def == 0 | n_calendar_def == nvar){
      if(n_calendar_def >0){
        for (i in 1:nvar){
          .jcall(jsdict,"V","add",varNames[i],ts_r2jd(series[,i]))
        }
        .jcall(jtd,"V","setUserVariables",
               paste0("r.",varNames)
               )
      }else{
        for (i in 1:nvar){
          .jcall(jsdict,"V","add",varNames[i],ts_r2jd(series[,i]))
          .jcall(jsobjct,"V","addUserDefinedVariable",varNames[i],type[i],coef[i])
        }
      }

    }else{
      i_ud <- (1:nvar)[-calendar_def]
      for (i in i_ud){
        .jcall(jsdict,"V","add", varNames[i],
               ts_r2jd(series[, i]))
        .jcall(jsobjct,"V","addUserDefinedVariable", varNames[i],
               type[i], coef[i])
      }

      for (i in calendar_def){
        .jcall(jsdict,"V","add",varNames[i],
               ts_r2jd(series[, i]))
      }
      .jcall(jtd,"V","setUserVariables",
             .jarray(paste0("r.",varNames[calendar_def])))
    }
  }

}

arimaCoef_r2jd <- function(jsobjct = NA, acoef = NA, p = NA , q = NA, bp = NA, bq = NA){
  typ <- acoef[,1]
  val <- acoef[,2]
  np <- length(typ)

  par <- val
  par[is.na(val) | val==0 | typ=="Undefined"] <- NaN
  fix<-rep(FALSE,np)
  fix[typ=="Fixed" & !is.na(par)] <- TRUE

  n <-1
  if (p!=0){
    param <- parameters_r2jd(par[n:(n+p-1)],fix[n:(n+p-1)])
    .jcall(jsobjct,"V","setPhi",param)
    n <- n+p
  }
  if (q!=0){
    param <- parameters_r2jd(par[n:(n+q-1)],fix[n:(n+q-1)])
    .jcall(jsobjct,"V","setTheta",param)
    n <- n+q
  }
  if (bp!=0){
    param <- parameters_r2jd(par[n:(n+bp-1)],fix[n:(n+bp-1)])
    .jcall(jsobjct,"V","setBPhi",param)
    n <- n+bp
  }
  if (bq!=0){
    param <- parameters_r2jd(par[n:(n+bq-1)],fix[n:(n+bq-1)])
    .jcall(jsobjct,"V","setBTheta",param)
  }
}
arimaCoef_jd2r <- function(jparams){
  if (is.jnull(jparams))
    return(NULL)
  param<-.jcastToArray(jparams)
  len <- length(param)
  if (len==0)
    return (NULL)
  param_name <- deparse(substitute(jparams))
  Type <- sapply(param, function(x) x$getType()$toString())
  Value <- sapply(param, function(x) x$getValue())
  data_param <- data.frame(Type = Type, Value = Value)
  rownames(data_param) <- sprintf("%s(%i)",
                                  param_name,
                                  1:len)
  data_param
}
spec_regarima_X13_r2jd <- function(rspec = NA, jdspec = NA){
  if (is.null(s_estimate(rspec)))
    return(.jnew("jdr/spec/ts/Utility$Dictionary"))

  est <- s_estimate(rspec)
  trans <- s_transform(rspec)
  usrspc <- s_usrdef(rspec)
  outF <- s_preOut(rspec)
  varF <- s_preVar(rspec)
  td <- s_td(rspec)
  easter <- s_easter(rspec)
  outliers <- s_out(rspec)
  arimaspc <- s_arima(rspec)
  arimacoF <- s_arimaCoef(rspec)
  span <- s_span(rspec)

  #Estimate
  jestimate <-.jcall(jdspec,"Ljdr/spec/x13/EstimateSpec;","getEstimate")
  jest.span <-.jcall(jestimate,"Ljdr/spec/ts/SpanSelector;","getSpan")
  span_r2jd(jsobjct = jest.span, type = span[1,1], d0=as.character(span[1,2]), d1=as.character(span[1,3]),
            n0 = as.integer(span[1,4]), n1=as.integer(span[1,5]))
  .jcall(jestimate ,"V","setTol", as.numeric(est["tolerance"]))

  jdspec$getBasic()$setPreliminaryCheck(est[1, "preliminary.check"])

  #Transform
  jtransform <-.jcall(jdspec,"Ljdr/spec/x13/TransformSpec;","getTransform")

  .jcall(jtransform,"V","setFunction",as.character(trans[1]))
  .jcall(jtransform,"V","setAdjust",as.character(trans[2]))
  .jcall(jtransform,"V","setAic",as.numeric(trans[3]))

  #Regression
  jregression<-.jcall(jdspec,"Ljdr/spec/x13/RegressionSpec;","getRegression")

  #Pre-specified outliers
  .jcall(jregression,"V","clearPrespecifiedOutliers")
  if (usrspc[1]==TRUE)
    preOut_r2jd(jsobjct = jregression, coefEna= usrspc[2], out= as.character(outF[,1]),outDate= as.character(outF[,2]),outCoef= outF[,3])

  #User-defined variables
  .jcall(jregression,"V","clearUserDefinedVariables")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")



  #Calendar
  jcalendar<-.jcall(jregression,"Ljdr/spec/x13/CalendarSpec;","getCalendar")
  jtd<-.jcall(jcalendar,"Ljdr/spec/x13/TradingDaysSpec;","getTradingDays")
  jeaster<-.jcall(jcalendar,"Ljdr/spec/x13/EasterSpec;","getEaster")

  .jcall(jeaster,"V","setJulian",as.logical(easter[2]))
  .jcall(jeaster,"V","setDuration", as.integer(easter[3]))
  .jcall(jeaster,"V","setTest", as.character(easter[4]))
  .jcall(jeaster,"V","setEnabled",as.logical(easter[1]))

  #Calendar options:
  if(td[1] == "UserDefined"){
    .jcall(jtd,"V","setOption","UserDefined")
  }else{
    if (td[4]==0) {
      .jcall(jtd,"V","setW",as.integer(td[4]))
      .jcall(jtd,"V","setTradingDays",as.character(td[1]))
    }else{
      .jcall(jtd,"V","setTradingDays",as.character(td[1]))
      .jcall(jtd,"V","setW",as.integer(td[4]))
    }
    .jcall(jtd,"V","setAutoAdjust",as.logical(td[2]))
    .jcall(jtd,"V","setLengthOfPeriod", as.character(td[3]))
  }
  .jcall(jtd,"V","setTest",as.character(td[5]))

  #user-defined specification
  if (usrspc[3]==TRUE)
    preVar_r2jd(jsobjct = jregression, jsdict = jdictionary, coefEna = usrspc[4],
                prevar_spec = varF, jtd = jtd)

  #Outlier
  joutlier<-.jcall(jdspec,"Ljdr/spec/x13/OutlierSpec;","getOutliers")
  joutlier.span <-.jcall(joutlier,"Ljdr/spec/ts/SpanSelector;","getSpan")

  span_r2jd(jsobjct = joutlier.span, type = span[2,1], d0=as.character(span[2,2]), d1=as.character(span[2,3]),
            n0 = as.integer(span[2,4]), n1=as.integer(span[2,5]))
  .jcall(joutlier,"V","setAO",as.logical(outliers[3]))
  .jcall(joutlier,"V","setTC",as.logical(outliers[4]))
  .jcall(joutlier,"V","setLS",as.logical(outliers[5]))
  .jcall(joutlier,"V","setSO",as.logical(outliers[6]))
  # Default critical value
  if (outliers[7] == TRUE){
    .jcall(joutlier,"V","setDefaultVa",as.logical(outliers[7]))
  }else{
    .jcall(joutlier,"V","setVa",as.numeric(outliers[8]))
  }
  .jcall(joutlier,"V","setMethod",as.character(outliers[9]))
  .jcall(joutlier,"V","setTCRate",as.numeric(outliers[10]))
  .jcall(joutlier,"V","setEnabled",as.logical(outliers[1]))

  #Arima
  jarima<-.jcall(jdspec,"Ljdr/spec/x13/ArimaSpec;","getArima")

  .jcall(jarima,"V","setAmiEnabled",as.logical(arimaspc[1]))
  .jcall(jarima,"V","setAcceptDefault",as.logical(arimaspc[2]))
  .jcall(jarima,"V","setCancel",as.numeric(arimaspc[3]))
  .jcall(jarima,"V","setUb1", as.numeric(arimaspc[4]))
  .jcall(jarima,"V","setUb2", as.numeric(arimaspc[5]))
  .jcall(jarima,"V","setMixed", as.logical(arimaspc[6]))
  .jcall(jarima,"V","setBalanced", as.logical(arimaspc[7]))
  .jcall(jarima,"V","setTsig",as.numeric(arimaspc[8]))
  .jcall(jarima,"V","setPredCV",as.numeric(arimaspc[9]))
  .jcall(jarima,"V","setPcr", as.numeric(arimaspc[10]))
  .jcall(jarima,"V","setUbFinal", as.numeric(arimaspc[11]))
  .jcall(jarima,"V","setMean",as.logical(arimaspc[12]))
  .jcall(jarima,"V","setP",as.integer(arimaspc[13]))
  .jcall(jarima,"V","setD",as.integer(arimaspc[14]))
  .jcall(jarima,"V","setQ",as.integer(arimaspc[15]))
  .jcall(jarima,"V","setBP",as.integer(arimaspc[16]))
  .jcall(jarima,"V","setBD",as.integer(arimaspc[17]))
  .jcall(jarima,"V","setBQ",as.integer(arimaspc[18]))

  # Fixed ARIMA coefficients
  if (arimaspc[19]==TRUE)
    arimaCoef_r2jd(jsobjct = jarima, acoef = arimacoF, p = as.numeric(arimaspc[13]) , q = as.numeric(arimaspc[15]),
                   bp = as.numeric(arimaspc[16]), bq = as.numeric(arimaspc[18]))

  return(jdictionary)
}

spec_TRAMO_r2jd <- function(rspec = NA, jdspec =NA){

  est <- s_estimate(rspec)
  trans <- s_transform(rspec)
  usrspc <- s_usrdef(rspec)
  outF <- s_preOut(rspec)
  varF <- s_preVar(rspec)
  td <- s_td(rspec)
  easter <- s_easter(rspec)
  outliers <- s_out(rspec)
  arimaspc <- s_arima(rspec)
  arimacoF <- s_arimaCoef(rspec)
  span <- s_span(rspec)

  #Estimate
  jdspec$getBasic()$setPreliminaryCheck(est[1, "preliminary.check"])

  jestimate <-.jcall(jdspec,"Ljdr/spec/tramoseats/EstimateSpec;","getEstimate")

  jest.span <-.jcall(jestimate,"Ljdr/spec/ts/SpanSelector;","getSpan")

  span_r2jd(jsobjct = jest.span, type = span[1,1], d0=as.character(span[1,2]), d1=as.character(span[1,3]),
            n0 = as.integer(span[1,4]), n1=as.integer(span[1,5]))

  .jcall(jestimate ,"V","setTol",as.numeric(est["tolerance"]))
  .jcall(jestimate ,"V","setEml",as.logical(est["exact_ml"]))
  .jcall(jestimate ,"V","setUbp",as.numeric(est["urfinal"]))

  #Transform
  jtransform <-.jcall(jdspec,"Ljdr/spec/tramoseats/TransformSpec;","getTransform")

  .jcall(jtransform,"V","setFunction",as.character(trans[1]))
  .jcall(jtransform,"V","setFct", as.numeric(trans[2]))

  #Regression
  jregression<-.jcall(jdspec,"Ljdr/spec/tramoseats/RegressionSpec;","getRegression")

  #Pre-specified outliers
  .jcall(jregression,"V","clearPrespecifiedOutliers")
  if (usrspc[1]==TRUE)
    preOut_r2jd(jsobjct = jregression, coefEna= usrspc[2],out= as.character(outF[,1]), outDate= as.character(outF[,2]),outCoef= outF[,3])

  #User-defined variables
  .jcall(jregression,"V","clearUserDefinedVariables")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")


  #Calendar
  jcalendar<-.jcall(jregression,"Ljdr/spec/tramoseats/CalendarSpec;","getCalendar")
  jtd<-.jcall(jcalendar,"Ljdr/spec/tramoseats/TradingDaysSpec;","getTradingDays")
  jeaster<-.jcall(jcalendar,"Ljdr/spec/tramoseats/EasterSpec;","getEaster")

  .jcall(jtd,"V","setAutomatic", as.character(td[1]))
  .jcall(jtd,"V","setPftd", as.numeric(td[2]))

  if(td[3] == "UserDefined"){
    .jcall(jtd,"V","setOption","UserDefined")
  }else{
    if (td[5]==0) {
      .jcall(jtd,"V","setW",as.integer(td[5]))
      .jcall(jtd,"V","setTradingDays",as.character(td[3]))
    }else{
      .jcall(jtd,"V","setTradingDays",as.character(td[3]))
      .jcall(jtd,"V","setW",as.integer(td[5]))
    }

    .jcall(jtd,"V","setLeapYear",as.logical(td[4]))
  }
  .jcall(jtd,"V","setRegressionTestType", as.character(td[6]))

  .jcall(jeaster,"V","setJulian",as.logical(easter[2]))
  .jcall(jeaster,"V","setDuration",as.integer(easter[3]))
  .jcall(jeaster,"V","setTest",as.logical(easter[4]))
  .jcall(jeaster,"V","setOption",as.character(easter[1]))

  #user-defined specification
  if (usrspc[3]==TRUE)
    preVar_r2jd(jsobjct = jregression, jsdict = jdictionary, coefEna = usrspc[4],
                prevar_spec = varF, jtd = jtd)

  #Outlier
  joutlier<-.jcall(jdspec,"Ljdr/spec/tramoseats/OutlierSpec;","getOutlier")
  joutlier.span <-.jcall(joutlier,"Ljdr/spec/ts/SpanSelector;","getSpan")

  span_r2jd(jsobjct = joutlier.span, type = span[2,1], d0=as.character(span[2,2]), d1=as.character(span[2,3]),
            n0 = as.integer(span[2,4]), n1=as.integer(span[2,5]))

  .jcall(joutlier,"V","setOutliersDetectionEnabled", as.logical(outliers[1]))
  .jcall(joutlier,"V","setAO", as.logical(outliers[3]))
  .jcall(joutlier,"V","setTC", as.logical(outliers[4]))
  .jcall(joutlier,"V","setLS", as.logical(outliers[5]))
  .jcall(joutlier,"V","setSO", as.logical(outliers[6]))
  # Default critical value
  if (outliers[7] == TRUE){
    .jcall(joutlier,"V","setAutoVa", as.logical(outliers[7]))
  }else{
    .jcall(joutlier,"V","setVa", as.numeric(outliers[8]))
  }
  .jcall(joutlier,"V","setEML", as.logical(outliers[9]))
  .jcall(joutlier,"V","setTCRate", as.numeric(outliers[10]))

  #Arima
  jarima<-.jcall(jdspec,"Ljdr/spec/tramoseats/ArimaSpec;","getArima")

  .jcall(jarima,"V","setEnabled", as.logical(arimaspc[1]))
  .jcall(jarima,"V","setAcceptDefault", as.logical(arimaspc[2]))
  .jcall(jarima,"V","setCancel", as.numeric(arimaspc[3]))
  .jcall(jarima,"V","setUb1", as.numeric(arimaspc[4]))
  .jcall(jarima,"V","setUb2", as.numeric(arimaspc[5]))
  .jcall(jarima,"V","setTsig", as.numeric(arimaspc[6]))
  .jcall(jarima,"V","setPc", as.numeric(arimaspc[7]))
  .jcall(jarima,"V","setPcr", as.numeric(arimaspc[8]))
  .jcall(jarima,"V","setAmiCompare", as.logical(arimaspc[9]))
  .jcall(jarima,"V","setMean", as.logical(arimaspc[10]))
  .jcall(jarima,"V","setP", as.integer(arimaspc[11]))
  .jcall(jarima,"V","setD", as.integer(arimaspc[12]))
  .jcall(jarima,"V","setQ", as.integer(arimaspc[13]))
  .jcall(jarima,"V","setBP", as.integer(arimaspc[14]))
  .jcall(jarima,"V","setBD", as.integer(arimaspc[15]))
  .jcall(jarima,"V","setBQ", as.integer(arimaspc[16]))

  # Fixed ARIMA coefficients
  if (arimaspc[17]==TRUE)
    arimaCoef_r2jd(jsobjct = jarima, acoef = arimacoF, p = as.numeric(arimaspc[11]) , q = as.numeric(arimaspc[13]),
                   bp = as.numeric(arimaspc[14]), bq = as.numeric(arimaspc[16]))

  return(jdictionary)
}

specX11_r2jd <- function(rspec = NA, jdspec = NA , freq = NA){
  x11 <- s_x11(rspec)
  jx11 <- .jcall(jdspec,"Ljdr/spec/x13/X11Spec;","getX11")

  seasonalma <- unlist(strsplit(as.character(x11[["x11.seasonalma"]]),
                                split = ", "))
  len.ma <- length(seasonalma)

  .jcall(jx11,"V","setMode",as.character(x11[["x11.mode"]]))
  .jcall(jx11,"V","setSeasonal",as.logical(x11[["x11.seasonalComp"]]))
  .jcall(jx11,"V","setLSigma",as.numeric(x11[["x11.lsigma"]]))
  .jcall(jx11,"V","setUSigma",as.numeric(x11[["x11.usigma"]]))
  if (x11[["x11.trendAuto"]]) {
    .jcall(jx11,"V","setTrendMA",as.integer(x11[["x11.trendma"]]))
  }else{
    .jcall(jx11,"V","setAutoTrendMA",as.logical(x11[["x11.trendAuto"]]))
    .jcall(jx11,"V","setTrendMA",as.integer(x11[["x11.trendma"]]))
  }
  .jcall(jx11,"V","setFreq", as.integer(freq))
  if (len.ma == 1) {
    .jcall(jx11, "V", "setSeasonalMA", seasonalma)
    seasma <- seasonalma
  }else if (len.ma != freq) {
    .jcall(jx11,"V","setSeasonalMA","Msr")
    warning(paste0("wrong frequency of the x11.seasonalma (",
                   len.ma, " instead of ", freq, ").",
            "\nPre-specified seasonal filters will be ignored (x11.seasonalma=\"Msr\")."),
            call. = FALSE)
    seasma <- "Msr"
  } else {

    .jcall(jx11,"V","setFullSeasonalMA",seasonalma)
    seasma <- as.character(x11[["x11.seasonalma"]])
  }
  .jcall(jx11, "V", "setForecastHorizon", as.integer(x11[["x11.fcasts"]]))
  .jcall(jx11, "V", "setBackcastHorizon", as.integer(x11[["x11.bcasts"]]))

  .jcall(jx11, "V", "setCalendarSigma", x11[["x11.calendarSigma"]])

  if (x11[["x11.calendarSigma"]] == "Select" &&
      !identical_na(x11[["x11.sigmaVector"]])) {
    # sigmaVector is change only if x11.calendarSigma is set to "Select"
    sigmaVector <- unlist(strsplit(as.character(x11[["x11.sigmaVector"]]),
                                  split = ", "))
    if (length(sigmaVector) != freq) {
      warning(paste0("Wrong frequency of the x11.sigmaVector (",
                     length(sigmaVector), " instead of ", freq, ").",
                     "\nThis parameter will be ignored."), call. = FALSE)
    } else {
      .jcall(jx11, "V", "setSigmavec", sigmaVector)
    }

  }
  .jcall(jx11, "V", "setExcludefcst", as.logical(x11[["x11.excludeFcasts"]]))

  return(seasma)
}

specSeats_r2jd <- function(rspec = NA, jdspec = NA){

  seats <- s_seats(rspec)
  jseats <-.jcall(jdspec,"Ljdr/spec/tramoseats/SeatsSpec;","getSeats")

  .jcall(jseats,"V","setPredictionLength", as.integer(seats[["seats.predictionLength"]]))
  .jcall(jseats,"V","setApproximationMode", as.character(seats[["seats.approx"]]))
  .jcall(jseats,"V","setXl", as.numeric(seats[["seats.maBoundary"]]))
  .jcall(jseats,"V","setRMod", as.numeric(seats[["seats.trendBoundary"]]))
  .jcall(jseats,"V","setSMod", as.numeric(seats[["seats.seasdBoundary"]]))
  .jcall(jseats,"V","setSMod1", as.numeric(seats[["seats.seasdBoundary1"]]))
  .jcall(jseats,"V","setEpsPhi",as.numeric(seats[["seats.seasTol"]]))
  .jcall(jseats,"V","setMethod", as.character(seats[["seats.method"]]))

}

