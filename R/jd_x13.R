setClass(
  Class="JD2_X13_java",
  contains = "JD2_ProcResults"
)
#' SA, X13
#'
#' @description
#' .
#'
#' @param series a univariate time series
#' @param spec model specification
#' @param userdefined vector with characters for additional output variables
#'
#' @details
#' .
#' @return
#' .
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @examples
#' mysa <- jd_defX13(myseries, "RSA5c")
#'
#' @export
jd_defX13 <-function(series, spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c"), userdefined){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)
  # create the java objects
  jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_RegArima_java",internal = jrarima)
  jrobct <- new (Class = "JD2_X13_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- regarima_defX13(jrobj = jrobct_arima, spec = jrspec)
    deco <- decomp_defX13(jrobj = jrobct, spec = jrspec)
    fin <- final(jrobj = jrobct)
    diagn <- diagnostics(jrobj = jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = diagn)

    if (!missing(userdefined))
      z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","X13")
    return(z)
  }
}
#' SA, X13
#'
#' @description
#' .
#'
#' @param series a univariate time series
#' @param spec user-defined model specification
#' @param userdefined vector with characters for additional output variables
#'
#' @details
#' .
#' @return
#' .
#'
#' @references
#' Info on JDemtra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @examples
#' mysa <- myseries
#'
#' @export
jd_x13 <-function(series, spec, userdefined){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  if (!inherits(spec, "SA_Spec") | !inherits(spec, "X13"))
    stop("use only with c(\"SA_Spec\",\"X13\") class object")

  # create the java objects
  jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  jdictionary <- specX13_r2jd(spec,jrspec)
  seasma <- specX11_r2jd(spec,jrspec, freq = frequency(series))
  jspec<-.jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_RegArima_java",internal = jrarima)
  jrobct <- new (Class = "JD2_X13_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- regarima_X13(jrobj = jrobct_arima, spec = spec$regarima)
    deco <- decomp_X13(jrobj = jrobct, spec = spec$x11, seasma=seasma)
    fin <- final(jrobj = jrobct)
    diagn <- diagnostics(jrobj = jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = diagn)

    if (!missing(userdefined))
      z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","X13")
    return(z)
  }
}
