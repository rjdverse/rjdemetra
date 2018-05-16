setClass(
  Class="JD2_TramoSeats_java",
  contains = "JD2_ProcResults"
)

jd_defTS <-function(series, spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA", "RSA4", "RSA5")){
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
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )
  jrobct <- new (Class = "JD2_TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- list #regarima_defTS(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    deco <- decomp_defTS(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    fin <- list #final_TS(jdobj = jd_clobj, jrobj = jrobct)
    q <- list #quality_TS(jdobj = jd_clobj, jrobj = jrobct)

    rm(jd_clobj)
    rm(jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = q)
    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}

decomp_defTS <- function(jdobj,jrobj,spec){
  # extract model specification from the java object
#  rspec <- specDecompTS_jd2r( spec = spec)
  # specification
  specification <- list
  # results
  jd_results <- decomp_rsltsTS(jdobj, jrobj)
  # new S3 class ("Decomp","TRAMO_SEATS")
  z<- list(specification = specification,
           linearized=jd_results$lin,
           components=jd_results$cmp)
  class(z) <- c("Decomp_TS")
  return(z)
}

