# Method: JD_RegArima for the function summary
#' @export
summary.JD_RegArima=function (object, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...){

  arima_coef <- object$arima.coefficients
  reg_coef <- object$regression.coefficients
  rslt_spec <- object$model$specification_rst
  loglik<- object$loglik
  res_err <- object$residuals.stat$st.error
  usr_spec <- object$specification$regression$userdef$specification
  out <- s_preOut(object)
  var <- s_preVar(object)$description

  cat("\n")
  cat("y = regression model + arima ",gsub("c","",deparse(object$arma)),sep="")
  cat("\n\n")
  cat("Model:",rslt_spec[1],sep=" ")
  cat("\n")
  cat("Estimation span:",rslt_spec[2],sep=" ")
  cat("\n")
  cat("Log-transformation:",if(rslt_spec[3]==TRUE) {"yes"} else {"no"},sep=" ")
  cat("\n")
  cat("Regression model:",if(rslt_spec[4]==TRUE) {"mean"} else {"no mean"},sep=" ")
  if(rslt_spec[5]==0) {cat(", no trading days effect")} else {cat(", trading days effect(",rslt_spec[5],")",sep="")}
  cat(if(rslt_spec[6]==TRUE) {", leap year effect"} else {", no leap year effect"},sep="")
  cat(if(rslt_spec[7]==TRUE) {", Easter effect"} else {", no Easter effect"},sep="")
  if(rslt_spec[8]==0) {cat(", no outliers")} else {cat(", outliers(",rslt_spec[8],")",sep="")}
  cat("\n\n")
  cat("Coefficients:")
  if (!is.null(arima_coef)){
    cat("\n")
    cat("ARIMA:","\n")
    a_tvalues=matrix(2*(1 - pt(abs(arima_coef[,3]), loglik$eff.obs)),ncol=1)
    colnames(a_tvalues)=c("Pr(>|t|)")
    printCoefmat(cbind(arima_coef,a_tvalues), digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  if (!is.null(reg_coef)){
    cat("\n")
    cat("Regression model:","\n")
    r_tvalues=matrix(2*(1 - pt(abs(reg_coef[,3]), loglik$eff.obs)),ncol=1)
    colnames(r_tvalues)=c("Pr(>|t|)")
    printCoefmat(cbind(reg_coef,r_tvalues), digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  if (usr_spec[1]==TRUE & usr_spec[2]==TRUE){
    out_t <- as.character(out[,1])
    out_y <- substr(out[,2],1,4)
    out_m <- as.character(as.numeric(substr(out[,2],6,7)))
    out_dsc <- paste(out_t," (",out_m,"-",out_y,")",sep = "")
    colnames(out) <- c("","","Coefficients")
    rownames(out) <- out_dsc
    fout <- out[3]
    cat("\n")
    cat("Fixed outliers:","\n")
    printCoefmat(fout, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  if (usr_spec[3]==TRUE & usr_spec[4]==TRUE){
    var_dsc <- if (dim(var)[1]==1){c("r.userdef")} else {paste("r.userdef",seq(1,dim(var)[1],1),sep="_")}
    colnames(var) <- c("","Coefficients")
    rownames(var) <- var_dsc
    fvar <- var[2]
    cat("\n")
    cat("Fixed other regression effects:","\n")
    printCoefmat(fvar, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  cat("\n\n")
  cat("Residual standard error:",formatC(res_err,digits = digits),"on",loglik$eff.obs,"degrees of freedom", sep = " ")
  cat("\n")
  cat("Log likelihood = ",formatC(loglik$loglikelihood,digits = digits),", aic = ",formatC(loglik$aic,digits = digits)," aicc = ",formatC(loglik$aicc,digits = digits),", bic(corrected for length) = ",formatC(loglik$bicc,digits = digits), sep = "")
  cat("\n\n")
}

# Method: JD_RegArima for the function print
#' @export
print.JD_RegArima=function (x, digits = max(3L, getOption("digits") - 3L), ...){

  arima_coef <- x$arima.coefficients
  reg_coef <- x$regression.coefficients
  loglik<- x$loglik
  res_err <- x$residuals.stat$st.error
  usr_spec <- x$specification$regression$userdef$specification
  out <- s_preOut(x)
  var <- s_preVar(x)$description

  cat("\n")
  cat("y = regression model + arima ",gsub("c","",deparse(x$arma)),sep="")
  cat("\n\n")
  cat("Coefficients:")
  if (!is.null(arima_coef)){
    if (!is.matrix(arima_coef[,-3])){
      tab.arima=t(as.matrix(arima_coef[,-3]))
      rownames(tab.arima)=rownames(arima_coef)
    }else{
      tab.arima=arima_coef[,-3]
    }
  cat("\n")
  printCoefmat(tab.arima, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  if (!is.null(reg_coef)){
    if (!is.matrix(reg_coef[,-3])){
      tab.reg=t(as.matrix(reg_coef[,-3]))
      rownames(tab.reg)=rownames(reg_coef)
    }else{
      tab.reg=reg_coef[,-3]
    }
    cat("\n")
    printCoefmat(tab.reg, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  if (usr_spec[1]==TRUE & usr_spec[2]==TRUE){
    out_t <- as.character(out[,1])
    out_y <- substr(out[,2],1,4)
    out_m <- as.character(as.numeric(substr(out[,2],6,7)))
    out_dsc <- paste(out_t," (",out_m,"-",out_y,")",sep = "")
    fout <- out[3]
    colnames(fout) <- "Coefficients"
    rownames(fout) <- out_dsc
    cat("\n")
    cat("Fixed outliers:","\n")
    printCoefmat(fout, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  if (usr_spec[3]==TRUE & usr_spec[4]==TRUE){
    var_dsc <- if (dim(var)[1]==1){c("r.userdef")} else {paste("r.userdef",seq(1,dim(var)[1],1),sep="_")}
    colnames(var) <- c("","Coefficients")
    rownames(var) <- var_dsc
    fvar <- var[2]
    cat("\n")
    cat("Fixed other regression effects:","\n")
    printCoefmat(fvar, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  cat("\n\n")
  cat("Residual standard error:",formatC(res_err,digits = digits),"on",loglik$eff.obs,"degrees of freedom", sep = " ")
  cat("\n")
  cat("Log likelihood = ",formatC(loglik$loglikelihood,digits = digits),", aic = ",formatC(loglik$aic,digits = digits)," aicc = ",formatC(loglik$aicc,digits = digits),", bic(corrected for length) = ",formatC(loglik$bicc,digits = digits), sep = "")
  cat("\n\n")
}




