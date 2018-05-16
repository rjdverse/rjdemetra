setClass(
  Class="JD2_X13_java",
  contains = "JD2_ProcResults"
)
jd_defX13 <-function(series, spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c")){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)
  # create the java objects
  if (exists("jd_clobj"))
    rm(jd_clobj)
  if (exists("jrobct"))
    rm(jrobct)
  jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)
  jrobct <- new (Class = "JD2_X13_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- list #regarima_defX13(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    deco <- decomp_defX13(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    fin <- list #final_X13(jdobj = jd_clobj, jrobj = jrobct)
    q <- list #quality_X13(jdobj = jd_clobj, jrobj = jrobct)

    rm(jd_clobj)
    rm(jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = q)
    class(z) <- c("SA","X13")
    return(z)
  }
}

decomp_defX13 <- function(jdobj,jrobj,spec){
  # extract model specification from the java object
#  rspec <- specDecompX13_jd2r( spec = spec)
  # specification
  specification <- list
  # results
  jd_results <- decomp_rsltsX13(jdobj,jrobj)
  # new S3 class ("Decomp","X13")
  z<- list(specification = specification,
           m_stat =  jd_results$m_stat,
           si_ratio = jd_results$si_ratio,
           s_filter = jd_results$s_filter,
           t_filter = jd_results$t_filter)
  class(z) <- c("Decomp_X13")
  return(z)
}

