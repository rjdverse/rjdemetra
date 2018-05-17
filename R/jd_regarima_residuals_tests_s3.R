#' Tests on RegARIMA residuals, pre-adjustment in X13 and TRAMO/SEATS
#' @description
#' \code{jd_regarima_rtest} provides tests on the normality, independence and linearity of the RegARIMA model residuals.
#'
#' @param x object of class \code{c("JD_RegArima","X13")} or of class \code{c("JD_RegArima","TRAMO_SEATS")}
#'
#' @details
#' \code{jd_regarima_rtest} provides the following tests on the RegARIMA model residuals:
#' \itemize{
#' \item Normality - Mean, Skewness, Kurtosis;
#'
#' \item Independence - Ljung-Box, Ljung-Box for residuals at seasonal lags;
#'
#' \item Linearity - Ljung-Box for squared residuals.
#' }
#' In all tests the H0 (normality, independence and linearity of residuals) shall not be rejected.
#'
#' @return
#' A list of class \code{"JD_RegArima_rtest"}  with components:
#'
#' \item{statistic}{vector with the values of the test statistics.}
#'
#' \item{p.value}{vector with the p-values of the tests.}
#'
#' \item{parameter}{vector with the parameters of the test statistics.}
#'
#' \item{method}{vector with character strings indicating what type of tests were performed.}
#'
#' \item{data.name}{vector with character strings giving the names of the data on which the tests were performed.}
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#'
#' @examples
#'   #Create JD_RegArima object
#'   myreg1 <- jd_regarima_defX13(myseries, spec=c("RG5c"))
#'   myreg2 <-jd_regarima_defTS(myseries, spec=c("TRfull"))
#'
#'   test1<-jd_regarima_rtest(myreg1)
#'   test2<-jd_regarima_rtest(myreg2)
#'   #print the results
#'   test1
#'   test2
#'   # print the results with the information on the statistics' parameters
#'   print(test1,parameter_dsc=TRUE)
#'   print(test2,parameter_dsc=TRUE)
#' @export
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
#' @export
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
