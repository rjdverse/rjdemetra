# Method: "regarima" for the function summary
#' @export
#' @export
summary.regarima <- function (object, ...){
  
  arma <- object$arma
  arima_coef <- object$arima.coefficients
  reg_coef <- object$regression.coefficients
  rslt_spec <- object$model$spec_rslt
  loglik<- object$loglik
  res_err <- object$residuals.stat$st.error
  usr_spec <- object$specification$regression$userdef$specification
  out <- s_preOut(object)
  var <- s_preVar(object)$description
  
  fvar <- fout <- NULL
  
  if (!is.null(arima_coef)){
    a_tvalues=matrix(2*(1 - pt(abs(arima_coef[,3]), loglik[3])),ncol=1)
    colnames(a_tvalues)=c("Pr(>|t|)")
    arima_coef <- cbind(arima_coef,a_tvalues)
  }
  if (!is.null(reg_coef)){
    r_tvalues=matrix(2*(1 - pt(abs(reg_coef[,3]), loglik[3])),ncol=1)
    colnames(r_tvalues)=c("Pr(>|t|)")
    reg_coef <- cbind(reg_coef, r_tvalues)
  }
  if (usr_spec[1]==TRUE & usr_spec[2]==TRUE){
    out <- out[out[,3]!=0,]
    if (dim(out)[1]!=0){
      out_t <- as.character(out[,1])
      out_y <- substr(out[,2],1,4)
      out_m <- as.character(as.numeric(substr(out[,2],6,7)))
      out_dsc <- paste(out_t," (",out_m,"-",out_y,")",sep = "")
      colnames(out) <- c("","","Coefficients")
      rownames(out) <- out_dsc
      fout <- out[3]
      fout <- cbind(fout, NA)
      colnames(fout)[ncol(fout)] <- "Pr(>|t|)"
    }
  }
  if (usr_spec[3]==TRUE & usr_spec[4]==TRUE){
    nvar0 <-dim(var)[1]
    var <- cbind(var,c(1:nvar0))
    var <- var[var[,2]!=0,]
    nvar <- dim(var)[1]
    if (nvar!=0){
      colnames(var) <- c("","Coefficients")
      fvar <- var[2]
      rownames(fvar) <- sprintf("r.%s", rownames(fvar))
      fvar <- cbind(fout, NA)
      colnames(fvar)[ncol(fvar)] <- "Pr(>|t|)"
    }
  }
  
  result <- list(arma_orders = arma,
                 results_spec = rslt_spec,
                 coefficients = list(arima = arima_coef,
                                     regression = reg_coef,
                                     fixed_out = fout,
                                     fixed_var = fvar),
                 loglik = loglik,
                 residuals_st_err = res_err)
  class(result) <- "summary.regarima"
  result
}
#' @export
print.summary.regarima <- function (x, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...){
  
  cat("y = regression model + arima ",gsub("c","",deparse(as.numeric(x$arma_orders))),sep="")
  cat("\n\n")
  cat("Model:",x$results_spec["Model"],sep=" ")
  cat("\n")
  cat("Estimation span:",x$results_spec["T.span"],sep=" ")
  cat("\n")
  cat("Log-transformation:",if(x$results_spec["Log transformation"]==TRUE) {"yes"} else {"no"},sep=" ")
  cat("\n")
  cat("Regression model:",if(x$results_spec["Mean"]==TRUE) {"mean"} else {"no mean"},sep=" ")
  if(x$results_spec["Trading days"]==0) {cat(", no trading days effect")} else {cat(", trading days effect(",x$results_spec["Trading days"],")",sep="")}
  cat(if(x$results_spec["Leap year"]==TRUE) {", leap year effect"} else {", no leap year effect"},sep="")
  cat(if(x$results_spec["Easter"]==TRUE) {", Easter effect"} else {", no Easter effect"},sep="")
  if(x$results_spec["Outliers"]==0) {cat(", no outliers")} else {cat(", outliers(",x$results_spec["Outliers"],")",sep="")}
  cat("\n\n")
  cat("Coefficients:")
  
  
  if (!is.null(x$coefficients$arima)){
    cat("\n")
    cat("ARIMA:","\n")
    printCoefmat(x$coefficients$arima, digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  if (!is.null(x$coefficients$regression)){
    cat("\n")
    cat("Regression model:","\n")
    
    printCoefmat(x$coefficients$regression, digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  if (!is.null(x$coefficients$fixed_out)){
    out <- out[out[,3]!=0,]
    if (dim(out)[1]!=0){
      printCoefmat(x$coefficients$fixed_out[, -ncol(x$coefficients$fixed_out)],
                   digits = digits, P.values= FALSE, na.print = "NA", ...)
    }
  }
  if (!is.null(x$coefficients$fixed_var)){
    cat("\n")
    cat("Fixed other regression effects:","\n")
    printCoefmat(x$coefficients$fixed_var[,-ncol(x$coefficients$fixed_var)],
                 digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  
  loglik <- x$loglik
  class(result) <- "summary.regarima"
  cat("\n\n")
  cat("Residual standard error:",
      formatC(x$residuals_st_err,digits = digits),
      "on",
      loglik["np",], "degrees of freedom", sep = " ")
  cat("\n")
  cat("Log likelihood = ", formatC(loglik["logvalue"], digits = digits),
      ", aic = ",formatC(loglik["aic", ], digits = digits),
      ", aicc = ", formatC(loglik["aicc", ], digits = digits),
      ", bic(corrected for length) = ", formatC(loglik["bicc", ],digits = digits),
      sep = "")
  cat("\n\n")
  invisible(x)
}

# Method: "regarima" for the function print
#' @export
print.regarima=function (x, digits = max(3L, getOption("digits") - 3L), ...){
  
  arma <- x$arma
  arima_coef <- x$arima.coefficients
  reg_coef <- x$regression.coefficients
  loglik<- x$loglik
  res_err <- x$residuals.stat$st.error
  usr_spec <- x$specification$regression$userdef$specification
  out <- s_preOut(x)
  var <- s_preVar(x)$description
  rslt_spec <- x$model$spec_rslt
  
  cat("y = regression model + arima ",gsub("c","",deparse(as.numeric(arma))),sep="")
  cat("\n")
  cat("Log-transformation:",if(rslt_spec[3]==TRUE) {"yes"} else {"no"},sep=" ")
  
  cat("\n")
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
    out <- out[out[,3]!=0,]
    if (dim(out)[1]!=0){
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
  }
  if (usr_spec[3]==TRUE & usr_spec[4]==TRUE){
    nvar0 <-dim(var)[1]
    var <- cbind(var,c(1:nvar0))
    var <- var[var[,2]!=0,]
    nvar <- dim(var)[1]
    if (nvar!=0){
      var_dsc <- if (nvar0==1){c("r.userdef")} else {paste("r.userdef",var[,3],sep="_")}
      colnames(var) <- c("","Coefficients")
      # rownames(var) <- var_dsc
      fvar <- var[2]
      rownames(fvar) <- sprintf("r.%s", rownames(fvar))
      cat("\n")
      cat("Fixed other regression effects:","\n")
      printCoefmat(fvar, digits = digits, P.values= FALSE, na.print = "NA", ...)
    }
  }
  cat("\n\n")
  cat("Residual standard error:",formatC(res_err,digits = digits),"on",loglik[3],"degrees of freedom", sep = " ")
  cat("\n")
  cat("Log likelihood = ",formatC(loglik[1],digits = digits),", aic = ",formatC(loglik[4],digits = digits)," aicc = ",
      formatC(loglik[5],digits = digits),", bic(corrected for length) = ",formatC(loglik[7],digits = digits), sep = "")
  cat("\n\n")
  invisible(x)
}
# Method: "regarima_rtest" for the print
#' @export
print.regarima_rtests=function (x, digits = max(3L, getOption("digits") - 3L),...){

  doublestar<-paste0("\u002A","\u002A")
  triplestar<-paste0("\u002A","\u002A","\u002A")

  stat <- x[,1]
  pval <- x[,2]

  sigcode=vector(mode = "character", length = 6)
  sigcode[pval >=0.1] = triplestar
  sigcode[pval < 0.1  & pval >= 0.05] = doublestar
  sigcode[pval < 0.05] = " "
  tabstat=data.frame(stat,pval,sigcode)
  rownames(tabstat)=rownames(x)
  colnames(tabstat)=c("Statistic","P.value","")
  tabstat[,1]=format(tabstat[,1], digits = digits)
  tabstat[,2]=format(round(tabstat[,2],max(4,digits)))

  cat("\n")
  cat("\033[1mNormality\033[22m")
  cat("\n")
  print (tabstat[1:3,])
  cat("\n")
  cat("Signif. codes:  H0 (normality of residuals) is not rejected at","\n")
  usestring<-paste0("significance levels: 0.1 ",triplestar,"0.05 ", doublestar,"\n")
  cat(usestring)

  cat("\n")
  cat("\033[1mIndependence\033[22m")
  cat("\n")
  print(tabstat[c(4,5),])
  cat("\n")
  cat("Signif. codes: H0 (independence of residuals) is not rejected at","\n")
  cat(usestring)

  cat("\n")
  cat("\033[1mLinearity\033[22m")
  cat("\n")
  print(tabstat[6,])
  cat("\n")
  cat("Signif. codes:  H0 (no conditional heteroscedasticity of residuals) is not rejected at","\n")
  cat(usestring)
  invisible(x)
}




