setClass(
  Class="JD2_TramoSeats_java",
  contains = "JD2_ProcResults"
)

#' SA, TRAMO-SEATS
#'
#' @description
#' \code{tramoseats}/\code{tramoseatsDef} estimates the seasonally adjusted series (sa) with the TRAMO-SEATS method.
#' This is achieved by decomposing the time series (y) into the: trend-cycle (t), seasonal component (s) and irregular component (i).
#' The final seasonally adjusted series shall be free of seasonal and calendar-related movements.
#'
#' @param series a univariate time series
#' @param spec model specification TRAMO-SEATS. For the function:
#' \itemize{
#' \item \code{tramoseats}, object of class \code{c("SA_spec","TRAMO_SEATS")}
#' \item \code{tramoseatsDef}, predefined TRAMO-SEATS \emph{JDemetra+} model specification (see \emph{Details}). The default is "RSAfull".
#' }
#' @param userdefined vector with characters for additional output variables
#'
#' @details
#' The first step of the seasonal adjustment consist of pre-adjusting the time series by removing from it the deterministic effects by means of a regression model with ARIMA noise (RegARIMA, see: \code{\link{regarima}}).
#' In the second part the pre-adjusted series is decomposed into the following components: trend-cycle (t), seasonal component (s) and irregular component (i). The decomposition can be: additive  (\eqn{y = t + s + i}) or multiplicative (\eqn{y = t * s * i}). The final seasonally adjusted series (sa) shall be free of seasonal and calendar-related movements.
#'
#' In the TRAMO-SEATS method, the second step - SEATS ("Signal Extraction in ARIMA Time Series") - performs an ARIMA-based decomposition of an observed time series into unobserved components. More information on the method can be found on the Bank of Spian website (\url{www.bde.es}).
#'
#' As regards the available predefined \emph{JDemetra+} TRAMO-SEATS model specifications (for the function \code{tramoseatsDef}), they are described in the table below.
#' \tabular{rrrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calender effects} |\tab \strong{ARIMA}\cr
#' RSA0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5 |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
#' RSAfull |\tab automatic |\tab AO/LS/TC |\tab automatic |\tab automatic
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
#' myspec <- tramoseats_specDef("RSAfull", seats.approx = "Noisy")
#' mysa <- tramoseats(myseries, myspec)
#' mysa
#'
#' mysa1 <- tramoseatsDef(myseries,"RSA2")
#' mysa1
#'
#' mysa2 <- tramoseatsDef(myseries,"RSAfull",
#' userdefined = c("decomposition.sa_lin_f","decomposition.sa_lin_e"))
#' mysa2
#' @export
tramoseats <-function(series, spec, userdefined){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  if (!inherits(spec, "SA_spec") | !inherits(spec, "TRAMO_SEATS"))
    stop("use only with c(\"SA_spec\",\"TRAMO_SEATS\") class object")
  # create the java objects
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- specTS_r2jd(spec,jrspec)
  specSeats_r2jd(spec,jrspec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_TRAMO_java",internal = jrarima)
  jrobct <- new (Class = "JD2_TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- regarima_TS(jrobj = jrobct_arima, spec = spec$regarima)
    deco <- decomp_TS(jrobj = jrobct, spec = spec$seats)
    fin <- final(jrobj = jrobct)
    diagn <- diagnostics(jrobj = jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = diagn)

    if (!missing(userdefined))
      z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}
#' @rdname tramoseats
#' @name tramoseats
#' @export
tramoseatsDef <-function(series, spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA", "RSA4", "RSA5"), userdefined){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)
  # create the java objects
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_TRAMO_java",internal = jrarima)
  jrobct <- new (Class = "JD2_TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{
    reg <- regarima_defTS(jrobj = jrobct_arima, spec = jrspec)
    deco <- decomp_defTS(jrobj = jrobct, spec = jrspec)
    fin <- final(jrobj = jrobct)
    diagn <- diagnostics(jrobj = jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin, diagnostics = diagn)

    if (!missing(userdefined))
      z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}


