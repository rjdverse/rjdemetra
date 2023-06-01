# Function to extract results from the RegARIMA S4 java object
regarima_rslts <- function(jrobj, fcsth){
  if (is.jnull(jrobj@internal) || is.null(jrobj@internal$getModel()))
    return(list(arma = NULL,
                arima.coefficients = NULL,
                regression.coefficients = NULL,
                loglik = NULL,
                model = NULL,
                residuals = NULL,
                residuals.stat = NULL,
                forecast = NULL))

  # ARIMA model
  arma_names <- paste0("arima.",c("p","d","q","bp","bd","bq"))
  arma <- sapply(arma_names,
                function(diag) {
                result(jrobj, diag)})
  names(arma) <- gsub("arima.","",arma_names)

  arima.est <- result(jrobj,"arima.parameters")

  # ARIMA coefficients
  if (!is.null(arima.est)){
    arima.se  <- sqrt(diag(result(jrobj,"model.pcovar")))
    if (sum(arima.se)==0)
      arima.se <- rep(0,length(arima.est))
    arima.tstat <- arima.est/arima.se
    arima.tstat[arima.se==0] <- NA
    arima.coefficients <- cbind(arima.est,arima.se,arima.tstat)

    arma.description <- c()

    if (arma[1]!=0){
      arma.description <- c(arma.description,paste0("Phi(",1:arma[1],")"))
    }
    if (arma[4]!=0){
      arma.description <- c(arma.description, paste0("BPhi(",1:arma[4],")"))
    }
    if (arma[3]!=0){
      arma.description <- c(arma.description, paste0("Theta(",1:arma[3],")"))
    }
    if (arma[6]!=0){
      arma.description <- c(arma.description, paste0("BTheta(",1:arma[6],")"))
    }
    rownames(arima.coefficients) <- arma.description
    colnames(arima.coefficients) <- c("Estimate","Std. Error","T-stat")
  }else{
    arima.coefficients = NULL
  }

  # Regression coefficients
  regression.coefficients <- result(jrobj, "model.coefficients")

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
                   function(diag)
                    result(jrobj,diag))
  loglik <- matrix(unlist(loglik),ncol = 1)
  rownames(loglik) <- gsub("likelihood.","",loglik_names)
  colnames(loglik) <- ""

  # Model specification after estimation & components
  model <- if(inherits(jrobj,"RegArima_java")){
    "RegARIMA - X13"
  }else if (inherits(jrobj,"TRAMO_java")){
    "RegARIMA - TRAMO/SEATS"
  }else{
    ""
  }

  t.span <- paste("from",result(jrobj,"model.espan.start"),"to",
                  result(jrobj,"model.espan.end"), sep=" ")
  transformed <- as.logical(result(jrobj,"model.log"))
  mean <- "Mean" %in% rownames(regression.coefficients)
  ntd <- result(jrobj,"model.ntd")
  if (is.null(result(jrobj,"model.lp"))){leap.year=FALSE} else {leap.year=TRUE}
  if (is.null(result(jrobj,"model.easter"))){easter=FALSE} else {easter=TRUE}
  nout <- result(jrobj,"model.nout")

  spec_rslt <- data.frame(model, t.span, transformed, mean, ntd, leap.year, easter, nout,
                          stringsAsFactors = FALSE)
  names(spec_rslt) <- c("Model", "T.span", "Log transformation", "Mean", "Trading days", "Leap year", "Easter", "Outliers")
  rownames(spec_rslt) <- NULL

  decomp_names <- c("y_lin","tde","ee","omhe","out_t","out_s","out_i")
  decomp <- lapply(paste0("model.", decomp_names),
                        function(series) result(jrobj, series))
  decomp <- ts(simplify2array(decomp),
               start = start(decomp[[1]]), frequency = frequency(decomp[[1]]))
  if (transformed){
    decomp[,2:7]<-log(decomp[,2:7])
  }
  decomp <- ts.union(decomp,rowSums(decomp[,5:7], na.rm = TRUE))
  colnames(decomp) <- c(decomp_names, "out")

  model <- list(spec_rslt = spec_rslt, effects = decomp)

  # Residuals
  residuals <- result(jrobj,"model.fullresiduals")

  # Statistics on residuals
  st.error<-result(jrobj,"likelihood.ser-ml")

  tests_names <- paste0("residuals.",c("mean","skewness","kurtosis","lb","seaslb","lb2"))
  tests <- lapply(tests_names,
                  function(diag) {
                    res <- result(jrobj, diag)
                    c(res[1], res[2],
                      attr(res, "description")
                    )
                  })
  tests <- data.frame(matrix(unlist(tests), ncol = 3, byrow=T),
                      stringsAsFactors=FALSE)
  tests[,1] <- as.numeric(tests[,1])
  tests[,2] <- as.numeric(tests[,2])

  colnames(tests) <- c("Statistic","P.value","Description")
  rownames(tests)<-c("mean","skewness","kurtosis","ljung box",
    "ljung box (residuals at seasonal lags)","ljung box (squared residuals)")
  class(tests) <- c("regarima_rtests","data.frame")

  residuals.stat<-list(st.error=st.error, tests=tests)

  # Forecast
  if(fcsth == 0){
    forecast <- NULL
  }else{
    fcst <- result(jrobj,paste("model.fcasts(",fcsth,")", sep=""))
    fcsterr <- result(jrobj,paste("model.efcasts(",fcsth,")", sep=""))

    forecast <- ts.union(fcst,fcsterr)
  }

  list(arma = arma,
       arima.coefficients = arima.coefficients,
       regression.coefficients = regression.coefficients,
       loglik = loglik,
       model = model,
       residuals = residuals,
       residuals.stat = residuals.stat,
       forecast = forecast)
}





