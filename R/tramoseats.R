setClass(
  Class="JD2_TramoSeats_java",
  contains = "JD2_ProcResults"
)

#' SA, TRAMO-SEATS
#'
#' @description
#' .
#'
#' @inheritParams x13Def
#' @param spec model specification TRAMO-SEATS
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
#' mysa <- tramoseatsDef(myseries, "RSAfull")
#'
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

#' SA, TRAMO-SEATS
#'
#' @description
#' .
#'
#' @inheritParams tramoseatsDef
#' @param spec user-defined model specification
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
#' myspec <- tramoseats_specDef("RSAfull", seats.approx = "Noisy")
#' mysa <- tramoseats(myseries, myspec)
#'
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


