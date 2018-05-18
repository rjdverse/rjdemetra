# Function to extract results from the RegARIMA S4 java object
regarima_rslts <- function(jrobj, fcsth){

  # ARIMA model
  arma <- c(result(jrobj,"arima.p"),result(jrobj,"arima.d"),result(jrobj,"arima.q"),result(jrobj,"arima.bp"),result(jrobj,"arima.bd"),result(jrobj,"arima.bq"))

  # ARIMA coefficients
  if (!is.null(result(jrobj,"arima.parameters"))){
    arima.est <-  result(jrobj,"arima.parameters")
    arima.se  <- sqrt(diag(result(jrobj,"model.pcovar")))
    if (sum(arima.se)==0)
      arima.se <- rep(0,length(arima.est))
    arima.tstat <- arima.est/arima.se
    arima.tstat[arima.se==0] <- NA
    arima.coefficients <- cbind(arima.est,arima.se,arima.tstat)

    arma.descritpion <- c()
    if (arma[1]!=0){
      for (i in 1:arma[1]){arma.descritpion <- c(arma.descritpion,paste("Phi(",as.character(i),")",sep=""))}
    }
    if (arma[3]!=0){
      for (i in 1:arma[3]){arma.descritpion <- c(arma.descritpion,paste("Theta(",as.character(i),")",sep=""))}
    }
    if (arma[4]!=0){
      for (i in 1:arma[4]){arma.descritpion <- c(arma.descritpion,paste("BPhi(",as.character(i),")",sep=""))}
    }
    if (arma[6]!=0){
      for (i in 1:arma[6]){arma.descritpion<-c(arma.descritpion,paste("BTheta(",as.character(i),")",sep=""))}
    }
    rownames(arima.coefficients) <- arma.descritpion
    colnames(arima.coefficients) <- c("Estimate","Std. Error","T-stat")
  }else{
    arima.coefficients = NULL
  }

  # Regression coefficients
  regression.coefficients <- result(jrobj,"model.coefficients")

  if (!is.null(regression.coefficients)){
    regression.tstat <- regression.coefficients[,1]/regression.coefficients[,2]
    regression.coefficients <- cbind(regression.coefficients,regression.tstat)
    rownames(regression.coefficients) <- result(jrobj,"model.description")
    colnames(regression.coefficients) <- c("Estimate","Std. Error","T-stat")
  }

  # Loglik
  loglik_names <- paste0("likelihood.",c("logvalue","np","neffectiveobs",
                                         "aic","aicc","bic","bicc"))
  loglik <- lapply(loglik_names,
                   function(diag) {
                    res <- result(jrobj,diag)})
  loglik <- do.call(rbind, loglik)
  rownames(loglik) <- gsub("likelihood.","",loglik_names)
  colnames(loglik) <- ""

  # Model specification after estimation & components
  model<- if (inherits(jrobj,"JD2_RegArima_java")) {"RegARIMA - X13"} else if (inherits(jrobj,"JD2_TRAMO_java")) {"RegARIMA - TRAMO/SEATS"} else {""}
  t.span <- paste("from",result(jrobj,"model.espan.start"),"to",result(jrobj,"model.espan.end"), sep=" ")
  transformed <- as.logical(result(jrobj,"model.log"))
  if (sum(match(result(jrobj,"model.description"),"Mean"), na.rm = TRUE)!=0) {mean=TRUE} else {mean=FALSE}
  ntd <- result(jrobj,"model.ntd")
  if (is.null(result(jrobj,"model.lp"))){leap.year=FALSE} else {leap.year=TRUE}
  if (is.null(result(jrobj,"model.easter"))){easter=FALSE} else {easter=TRUE}
  nout <- result(jrobj,"model.nout")

  spec_rslt <- c(model,t.span,transformed,mean,ntd,leap.year,easter,nout)
  names(spec_rslt) <- c("Model","T.span", "Log transformation", "Mean","Trading days","Leap year","Easter","Outliers")

  #if (transformed=="true"){y_lin=result(jrobj,"model.l")} else {y_lin=result(jrobj,"model.y_lin")}
  #td.effect<-result(jrobj,"model.tde")
  #easter.effect<-result(jrobj,"model.ee")
  #omh.effect<-result(jrobj,"model.omhe")
  #out.effect<-result(jrobj,"model.out_t")+result(jrobj,"model.out_s")+result(jrobj,"model.out_i")

  decomp_names <-paste0("model.",c("y_lin","tde","ee","omhe","out_t","out_s","out_i"))
  decomp <- do.call(ts.union,
                       lapply(decomp_names,
                              function(series) result(jrobj, series)))
  decomp <- ts.union(decomp,rowSums(decomp[,5:7], na.rm = TRUE))
  colnames(decomp) <- c(gsub("model.","",decomp_names),"out")

  #model <- list(specification_rst = specification_rst, y_lin = y_lin, td.effect = td.effect, easter.effect = easter.effect,
  #              omh.effect = omh.effect, out.effect = out.effect)
  model <- list(spec_rslt = spec_rslt, effects = decomp)

  # Residuals
  residuals <- result(jrobj,"model.fullresiduals")

  # Statistics on residuals
  st.error<-result(jrobj,"likelihood.ser-ml")

  dtmean<-result(jrobj,"residuals.mean")
  dtskewness<-result(jrobj,"residuals.skewness")
  dtkurtosis<-result(jrobj,"residuals.kurtosis")
  dtljung.box<-result(jrobj,"residuals.lb")
  dtljung.boxSa<-result(jrobj,"residuals.seaslb")
  dtljung.box2<-result(jrobj,"residuals.lb2")
  dscmean<-as.character(attributes(dtmean))
  dscskewness<-as.character(attributes(dtskewness))
  dsckurtosis<-as.character(attributes(dtkurtosis))
  dscljung.box<-as.character(attributes(dtljung.box))
  dscljung.boxSa<-as.character(attributes(dtljung.boxSa))
  dscljung.box2<-as.character(attributes(dtljung.box2))

  dtstat<-rbind(dtmean,dtskewness,dtkurtosis,dtljung.box,dtljung.boxSa,dtljung.box2)
  dscstat<-rbind(dscmean,dscskewness,dsckurtosis,dscljung.box,dscljung.boxSa,dscljung.box2)
  tabstat<-data.frame(dtstat,dscstat)
  rownames(tabstat)<-c("mean","skewness","kurtosis","ljung.box","ljung.boxSa","ljung.box2")
  colnames(tabstat)<-c("Statistic","P.value","Description")

  residuals.stat<-list(st.error=st.error, tests=tabstat)

  # Forecast
  fcst <- result(jrobj,paste("model.fcasts(",fcsth,")", sep=""))
  fcsterr <- result(jrobj,paste("model.efcasts(",fcsth,")", sep=""))

  forecast <- list(forecast = fcst, st.error= fcsterr)

  z<- list( arma=arma,
            arima.coefficients =arima.coefficients,
            regression.coefficients = regression.coefficients,
            loglik=loglik,
            model=model,
            residuals = residuals,
            residuals.stat = residuals.stat,
            forecast = forecast)
}





