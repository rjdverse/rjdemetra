print.Decomp_X13=function (x, digits = max(3L, getOption("digits") - 3L), ...){
  m <- x$mstats
  s_flt <- x$s_filter
  t_flt <- x$t_filter
  if (!is.null(m)){
    cat("Monitoring and Quality Assessment Statistics:","\n")
    printCoefmat(m, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  cat("\n")
  cat("Final filters:","\n")
  cat("Seasonal filter: ",s_flt)
  cat("\n")
  cat("Trend filter: ",t_flt)
  cat("\n")
}

print.Decomp_TS=function (x, digits = max(3L, getOption("digits") - 3L), ...){

  model <-x$model$model
  sa <- x$model$sa
  t <- x$model$trend
  s <- x$model$seasonal
  trans <- x$model$transitory
  i <- x$model$irregular

  var <- list(model, sa, t, s, trans, i)
  var_names <- c("Model","SA","Trend","Seasonal","Transitory","Irregular")

  for (ii in 1:length(var_names)){
    cat(var_names[ii],"\n")
    print_formula(var[[ii]][1,-1],"AR")
    print_formula(var[[ii]][2,-1],"D")
    print_formula(var[[ii]][3,-1],"MA")
    if (var[[ii]][4,1]==1) {
      cat("\n\n")
    }else{
      cat("Innovation variance: ",var[[ii]][4,1], "\n\n")
    }
  }
}

print_formula <- function(x, var){
  non_0 <- which(x != 0 )
  if(length(non_0) == 0)
    return(NULL)
  polynome_degre <- paste0("B^", non_0)
  polynome_degre[non_0 == 1] <- "B"

  polynome_coef <- sprintf("%+f", x[non_0], polynome_degre)
  polynome_coef <- gsub("-","- ", polynome_coef)
  polynome_coef <- gsub("\\+","+ ", polynome_coef)
  polynome_coef[x[non_0] == -1] <- "-"
  polynome_coef[x[non_0] == 1] <- "+"

  polynome_formula <- paste(polynome_coef, polynome_degre, collapse = " ")
  polynome_formula <- paste("1", polynome_formula)
  cat(var,": ",polynome_formula,"\n")
}

