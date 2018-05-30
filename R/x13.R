setClass(
  Class="JD2_X13_java",
  contains = "JD2_ProcResults"
)
#' SA, X13
#'
#' @description
#' \code{x13}/\code{x13Def} estimates the seasonally adjusted series (sa) with the X13 method.
#' This is achieved by decomposing the time series (y) into the: trend-cycle (t), seasonal component (s) and irregular component (i).
#' The final seasonally adjusted series shall be free of seasonal and calendar-related movements.
#'
#' @param series a univariate time series
#' @param spec model specification X13. For the function:
#' \itemize{
#' \item \code{x13}, object of class \code{c("SA_spec","X13")}
#' \item \code{x13Def}, predefined X13 \emph{JDemetra+} model specification (see \emph{Details}). The default is "RSA5c".
#' }
#' @param userdefined vector with characters for additional output variables
#'
#' @details
#' The first step of the seasonal adjustment consist of pre-adjusting the time series by removing from it the deterministic effects by means of a regression model with ARIMA noise (RegARIMA, see: \code{\link{regarima}}).
#' In the second part the pre-adjusted series is decomposed into the following components: trend-cycle (t), seasonal component (s) and irregular component (i). The decomposition can be: additive  (\eqn{y = t + s + i}), multiplicative (\eqn{y = t * s * i}), log-additive (\eqn{log(y) = log(t)+log(s)+log(i)}) or pseudo-additive (\eqn{y = t*(s+i-1)}).
#' The final seasonally adjusted series (sa) shall be free of seasonal and calendar-related movements.
#'
#' In the X13 method, the X11 algorithm decomposes the series by means of linear filters. More information on the method can be found on the U.S. Census Bureau website.
#'
#' As regards the available predefined \emph{JDemetra+} X13 model specifications (for the function \code{x13Def}), they are described in the table below.
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calender effects} |\tab \strong{ARIMA}\cr
#' RSA0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
#' }
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
#' myspec <- x13_specDef("RSA5c")
#' mysa <- x13(myseries, myspec)
#' mysa
#'
#' mysa1 <- x13Def(myseries,"RSA3")
#' mysa1
#'
#' mysa2 <- x13Def(myseries,"RSA5c",
#' userdefined = c("decomposition.d18","decomposition.d19"))
#' mysa2
#'
#' @export
x13 <-function(series, spec, userdefined){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  if (!inherits(spec, "SA_spec") | !inherits(spec, "X13"))
    stop("use only with c(\"SA_spec\",\"X13\") class object")

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
#' @rdname x13
#' @name x13
#' @export
x13Def <-function(series, spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c"), userdefined){
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

