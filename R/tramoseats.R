setClass(
  Class="JD2_TramoSeats_java",
  contains = "JD2_ProcResults"
)

#' Seasonal Adjustment with TRAMO-SEATS
#'
#' @description
#' \code{tramoseats}/\code{tramoseats_def} estimates the seasonally adjusted series (sa) with the TRAMO-SEATS method.
#' This is achieved by decomposing the time series (y) into the: trend-cycle (t), seasonal component (s) and irregular component (i).
#' The final seasonally adjusted series shall be free of seasonal and calendar-related movements.
#'
#' @param series a univariate time series
#' @param spec model specification TRAMO-SEATS. For the function:
#' \itemize{
#' \item \code{tramoseats}, object of class \code{c("SA_spec","TRAMO_SEATS")}
#' \item \code{tramoseats_def}, predefined TRAMO-SEATS \emph{JDemetra+} model specification (see \emph{Details}). The default is "RSAfull".
#' }
#' @param userdefined vector with characters for additional output variables.
#'
#' @details
#' The first step of the seasonal adjustment consist of pre-adjusting the time series by removing from it the deterministic effects by means of a regression model with ARIMA noise (RegARIMA, see: \code{\link{regarima}}).
#' In the second part, the pre-adjusted series is decomposed into the following components: trend-cycle (t), seasonal component (s) and irregular component (i). The decomposition can be: additive  (\eqn{y = t + s + i}) or multiplicative (\eqn{y = t * s * i}). The final seasonally adjusted series (sa) shall be free of seasonal and calendar-related movements.
#'
#' In the TRAMO-SEATS method, the second step - SEATS ("Signal Extraction in ARIMA Time Series") - performs an ARIMA-based decomposition of an observed time series into unobserved components. More information on the method can be found on the Bank of Spian website (\url{www.bde.es}).
#'
#' As regards the available predefined \emph{JDemetra+} TRAMO-SEATS model specifications (for the function \code{tramoseats_def}), they are described in the table below.
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
#' \code{tramoseats}/\code{tramoseats_def} returns an object of class \code{c("SA","TRAMO_SEATS")}, a list containing the following components:
#'
#' \item{regarima}{object of class \code{c("regarima","TRAMO_SEATS")}. See \emph{Value} of the function \code{\link{regarima}}.}
#'
#' \item{decomposition}{object of class \code{"decomposition_SEATS"}, five elements list:
#' \itemize{
#' \item \code{specification} list with the SEATS algorithm specification. See also function \code{\link{tramoseats_spec}}
#' \item \code{mode} decomposition mode
#' \item \code{model} list with the SEATS models: \code{model, sa, trend, seasonal, transitory, irregular}. Each of them is a matrix with the estimated coefficients.
#' \item \code{linearized} time series matrix (mts) with the stochastic series decomposition (input series \code{y_lin}, seasonally adjusted \code{sa_lin}, trend \code{t_lin}, seasonal \code{s_lin}, irregular \code{i_lin})
#' \item \code{components} time series matrix (mts) with the decomposition components (input series \code{y_cmp}, seasonally adjusted \code{sa_cmp}, trend \code{t_cmp}, seasonal \code{s_cmp}, irregular \code{i_cmp})
#' }
#' }
#'
#' \item{final}{object of class \code{c("final","mts","ts","matrix")}. Matrix with the final results of the seasonal adjustment.
#' It includes time series: original time series (\code{y}), forecast of the original series (\code{y_f}), trend (\code{t}), forecast of the trend (\code{t_f}),
#' seasonally adjusted series (\code{sa}), forecast of the seasonally adjusted series (\code{sa_f}),
#' seasonal component (\code{s}), forecast of the seasonal component (\code{s_f}), irregular component (\code{i}) and the forecast of the irregular component (\code{i_f}).}
#'
#' \item{diagnostics}{object of class \code{"diagnostics"}, list with four type of diagnostics tests:
#' \itemize{
#' \item \code{variance_decomposition} data.frame with the tests on the relative contribution of the components to the stationary portion of the variance in the original series, after the removal of the long term trend.
#' \item \code{residuals_test} data.frame with the tests on the presence of seasonality in the residuals (includes the statistic, p-value and parameters description)
#' \item \code{combined_test_all}  combined tests for stable seasonality in the entire series. Two elements list with: \code{tests_for_stable_seasonality} - data.frame with the tests (includes the statistic, p-value and parameters description) and \code{combined_seasonality_test} - the summary.
#' \item \code{combined_test_end} combined tests for stable seasonality in the last 3 years.  Two elements list with: \code{tests_for_stable_seasonality} - data.frame with the tests (includes the statistic, p-value and parameters description) and \code{combined_seasonality_test} - the summary.
#' }
#' }
#'
#' @references
#' Info on JDemetra+, usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @seealso \code{\link{tramoseats_spec}}, \code{\link{x13}}
#'
#' @examples
#'
#'   myspec<-tramoseats_spec_def("RSAfull")
#'   mysa <-tramoseats(myseries,myspec)
#'   mysa1 <- tramoseats_def(myseries, spec = "RSAfull")
#'   mysa
#'   mysa1
#'
#'   myspec2 <- tramoseats_spec(myspec, tradingdays.mauto = "Unused",
#'                              tradingdays.option = "WorkingDays",
#'                              easter.type = "Standard",
#'                              automdl.enabled = FALSE, arima.mu = TRUE)
#'   mysa2 <- tramoseats(myseries, myspec2)
#'
#'   var1 <- ts(rnorm(length(myseries))*10,start = c(2001, 12), frequency = 12)
#'   var2 <- ts(rnorm(length(myseries))*100,start = c(2001, 12), frequency = 12)
#'   var<-ts.union(var1,var2)
#'   myspec3 <- tramoseats_spec(myspec,
#'                               usrdef.varEnabled = TRUE, usrdef.var = var)
#'   s_preVar(myspec3)
#'   mysa3 <- tramoseats(myseries,myspec3)
#'   plot(mysa3)
#'   plot(mysa3$regarima)
#'   plot(mysa3$decomposition)
#'
#'   mysa2 <- tramoseats_def(myseries,"RSAfull",
#'   userdefined = c("decomposition.sa_lin_f","decomposition.sa_lin_e"))
#'   mysa2
#'
#' @export
tramoseats <-function(series, spec, userdefined = NULL){
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

  # Or, using the fonction x13JavaResults :
  # return(tramoseatsJavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))

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

    z <- list(regarima = reg, decomposition = deco, final = fin,
              diagnostics = diagn, user_defined = user_defined(userdefined,jrobct))

    if (!missing(userdefined))
      z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}
#' @rdname tramoseats
#' @name tramoseats
#' @export
tramoseats_def <-function(series, spec=c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA", "RSA4", "RSA5"),
                         userdefined = NULL){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)
  # create the java objects
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )

  return(tramoseatsJavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))
}

tramoseatsJavaResults <- function(jrslt, spec, userdefined = NULL){
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_TRAMO_java",internal = jrarima)
  jrobct <- new (Class = "JD2_TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal))
    return (NaN)

  reg <- regarima_defTS(jrobj = jrobct_arima, spec = spec)
  deco <- decomp_defTS(jrobj = jrobct, spec = spec)
  fin <- final(jrobj = jrobct)
  diagn <- diagnostics(jrobj = jrobct)

  z <- list(regarima = reg, decomposition = deco, final = fin,
            diagnostics = diagn, user_defined = user_defined(userdefined,jrobct))

  class(z) <- c("SA","TRAMO_SEATS")
  return(z)

}
