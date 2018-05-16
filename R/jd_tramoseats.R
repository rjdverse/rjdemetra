setClass(
  Class="JD2_TramoSeats_java",
  contains = "JD2_ProcResults"
)

#' SA, TRAMO/SEATS
#' 
#' @description
#' .
#'
#' @param series a univariate time series
#' @param spec model specification
#'
#' @details  .
#' 
#' @return
#' .
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' 
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#' 
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @examples
#' mysa <- jd_defTS(myseries, "RSAfull")
#'
#' @export
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
    reg <- list#regarima_defTS(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    deco <- list #decomp_defTS(jdobj = jd_clobj, jrobj = jrobct, spec = jrspec)
    fin <- list #final_TS(jdobj = jd_clobj, jrobj = jrobct)
    q <- list #quality_TS(jdobj = jd_clobj, jrobj = jrobct)
    
    rm(jd_clobj)
    rm(jrobct)
    
    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = q)
    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}

