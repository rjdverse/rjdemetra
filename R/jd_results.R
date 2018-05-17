# Function to extract results from the RegARIMA S4 java object
regarima_rslts <- function(jdobj, jrobj, fcsth){

  # ARIMA model
  arma <- c(result(jdobj,jrobj,"arima.p"),result(jdobj,jrobj,"arima.d"),result(jdobj,jrobj,"arima.q"),result(jdobj,jrobj,"arima.bp"),result(jdobj,jrobj,"arima.bd"),result(jdobj,jrobj,"arima.bq"))

  # ARIMA coefficients
  if (!is.null(result(jdobj,jrobj,"arima.parameters"))){
    arima.est <-  result(jdobj,jrobj,"arima.parameters")
    arima.se  <- sqrt(diag(result(jdobj,jrobj,"model.pcovar")))
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
  regression.coefficients <- result(jdobj,jrobj,"model.coefficients")

  if (!is.null(regression.coefficients)){
    regression.tstat <- regression.coefficients[,1]/regression.coefficients[,2]
    regression.coefficients <- cbind(regression.coefficients,regression.tstat)
    rownames(regression.coefficients) <- result(jdobj,jrobj,"model.description")
    colnames(regression.coefficients) <- c("Estimate","Std. Error","T-stat")
  }

  # Loglik
  loglikelihood <- result(jdobj,jrobj,"likelihood.logvalue")
  n.parameters <- result(jdobj,jrobj,"likelihood.np")
  eff.obs <- result(jdobj,jrobj,"likelihood.neffectiveobs")
  aic <- result(jdobj,jrobj,"likelihood.aic")
  aicc <- result(jdobj,jrobj,"likelihood.aicc")
  bic <- result(jdobj,jrobj,"likelihood.bic")
  bicc <- result(jdobj,jrobj,"likelihood.bicc")

  loglik <- list(loglikelihood=loglikelihood,n.parameters=n.parameters,eff.obs=eff.obs,aic=aic,aicc=aicc,bic=bic,bicc=bicc)

  # Model specification after estimation & components
  model<- if (inherits(jrobj,"JD2_RegArima_java")) {"RegARIMA - X13"} else if (inherits(jrobj,"JD2_TRAMO_java")) {"RegARIMA - TRAMO/SEATS"} else {""}
  t.span <- paste("from",result(jdobj,jrobj,"model.espan.start"),"to",result(jdobj,jrobj,"model.espan.end"), sep=" ")
  transformed <- as.logical(result(jdobj,jrobj,"model.log"))
  if (sum(match(result(jdobj,jrobj,"model.description"),"Mean"), na.rm = TRUE)!=0) {mean=TRUE} else {mean=FALSE}
  ntd <- result(jdobj,jrobj,"model.ntd")
  if (is.null(result(jdobj,jrobj,"model.lp"))){leap.year=FALSE} else {leap.year=TRUE}
  if (is.null(result(jdobj,jrobj,"model.easter"))){easter=FALSE} else {easter=TRUE}
  nout <- result(jdobj,jrobj,"model.nout")

  if (transformed=="true"){y_lin=result(jdobj,jrobj,"model.l")} else {y_lin=result(jdobj,jrobj,"model.y_lin")}
  td.effect<-result(jdobj,jrobj,"model.tde")
  easter.effect<-result(jdobj,jrobj,"model.ee")
  omh.effect<-result(jdobj,jrobj,"model.omhe")
  out.effect<-result(jdobj,jrobj,"model.out_t")+result(jdobj,jrobj,"model.out_s")+result(jdobj,jrobj,"model.out_i")

  specification_rst <- c(model,t.span,transformed,mean,ntd,leap.year,easter,nout)
  names(specification_rst) <- c("Model","T.span", "Log transformation", "Mean","Trading days","Leap year","Easter","Outliers")

  model <- list(specification_rst = specification_rst, y_lin = y_lin, td.effect = td.effect, easter.effect = easter.effect,
                omh.effect = omh.effect, out.effect = out.effect)

  # Residuals
  residuals <- result(jdobj,jrobj,"model.fullresiduals")

  # Statistics on residuals
  st.error<-result(jdobj,jrobj,"likelihood.ser-ml")

  dtmean<-result(jdobj,jrobj,"residuals.mean")
  dtskewness<-result(jdobj,jrobj,"residuals.skewness")
  dtkurtosis<-result(jdobj,jrobj,"residuals.kurtosis")
  dtljung.box<-result(jdobj,jrobj,"residuals.lb")
  dtljung.boxSa<-result(jdobj,jrobj,"residuals.seaslb")
  dtljung.box2<-result(jdobj,jrobj,"residuals.lb2")
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
  fcst <- result(jdobj,jrobj,paste("model.fcasts(",fcsth,")", sep=""))
  fcsterr <- result(jdobj,jrobj,paste("model.efcasts(",fcsth,")", sep=""))

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





