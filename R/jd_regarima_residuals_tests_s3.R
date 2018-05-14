# The function creates a S3 object of class "JD_RegArima_rtest"
jd_regarima_rtest = function(x){
  if (!inherits(x, "JD_RegArima"))
    stop("use only with \"JD_RegArima\" objects")

  s <- x$residuals.stat$tests[,1]; names(s) <-rownames(x$residuals.stat$tests)
  pv <- x$residuals.stat$tests[,2]; names(pv) <-rownames(x$residuals.stat$tests)
  param <- as.vector(x$residuals.stat$tests[,3]); names(param) <-rownames(x$residuals.stat$tests)
  mth <- c("Mean","Skewness","Kurtosis","Ljung box","Ljung box (residuals at seasonal lags)","Ljung box (squared residuals)"); names(mth) <-rownames(x$residuals.stat$tests)
  dname <- c("residuals","residuals","residuals","resdiuals","residuals at seasonal lags","squared residuals"); names(dname) <-rownames(x$residuals.stat$tests)
  result <- list(statistic = s, p.value = pv , parameter = param,
                 method = mth, data.name = dname)
  class(result) <- "JD_RegArima_rtest"
  return(result)
}

# Method: "JD_RegArima_rtest" for the print
print.JD_RegArima_rtest=function (x, digits = max(3L, getOption("digits") - 3L), parameter_dsc = FALSE, ...){

  doublestar<-paste0("\u002A","\u002A")
  triplestar<-paste0("\u002A","\u002A","\u002A")

  sigcode=vector(mode = "character", length = 6)
  sigcode[x$p.value >=0.1] = triplestar
  sigcode[x$p.value < 0.1  & x$p.value >= 0.05] = doublestar
  sigcode[x$p.value < 0.05] = " "
  tabstat=data.frame(x$statistic,x$p.value,sigcode)
  rownames(tabstat)=x$method
  colnames(tabstat)=c("Statistic","P.value","")
  tabstat[,1]=format(tabstat[,1], digits = digits)
  tabstat[,2]=format(round(tabstat[,2],max(4,digits)))

  cat("\n","--------------","\n",sep="")
  cat("Normality")
  cat("\n","--------------","\n",sep="")
  print (tabstat[1:3,])
  cat("\n")
  cat("Signif. codes:  H0 (normality of residuals) is not rejected at","\n")
  usestring<-paste0("significance levels: 0.1 ",triplestar,"0.05 ", doublestar,"\n")
  cat(usestring)

  cat("\n","--------------","\n",sep="")
  cat("Independence")
  cat("\n","--------------","\n",sep="")
  print(tabstat[c(4,5),])
  cat("\n")
  cat("Signif. codes: H0 (independence of residuals) is not rejected at","\n")
  cat(usestring)

  cat("\n","--------------","\n",sep="")
  cat("Linearity")
  cat("\n","--------------","\n",sep="")
  print(tabstat[6,])
  cat("\n")
  cat("Signif. codes:  H0 (no conditional heteroscedasticity of residuals) is not rejected at","\n")
  cat(usestring)

  cat("\n")
  if (parameter_dsc==TRUE) {
  cat("\n","--------------","\n",sep="")
  cat("Statistics parameters")
  cat("\n","--------------","\n",sep="")
  tabdsc=matrix(x$parameter,ncol=1)
  rownames(tabdsc)=x$method
  colnames(tabdsc)=""
  print(tabdsc)
  cat("\n")
  }
}
