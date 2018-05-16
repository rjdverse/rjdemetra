print.Decomp_X13=function (x, digits = max(3L, getOption("digits") - 3L), ...){
  m <- x$m_stat
  s_flt <- x$s_filter
  t_flt <- x$t_filter
  if (!is.null(m)){
    cat("Monitoring and Quality Assessment Statistics:","\n")
    printCoefmat(m, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  cat("\n\n")
  cat("Final filters:","\n")
  cat("Seasonal filter: ",s_flt)
  cat("\n")
  cat("Trend filter: ",t_flt)
  cat("\n")
}

#print.Decomp_TS=function (x, digits = max(3L, getOption("digits") - 3L), ...){}
  