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
#' In the second part, the pre-adjusted series is decomposed into the following components: trend-cycle (t), seasonal component (s) and irregular component (i). The decomposition can be: additive  (\eqn{y = t + s + i}), multiplicative (\eqn{y = t * s * i}), log-additive (\eqn{log(y) = log(t)+log(s)+log(i)}) or pseudo-additive (\eqn{y = t*(s+i-1)}).
#' The final seasonally adjusted series (sa) shall be free of seasonal and calendar-related movements.
#'
#' In the X13 method, the X11 algorithm (second step) decomposes the time series by means of linear filters. More information on the method can be found on the U.S. Census Bureau website.
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
#' \code{x13}/\code{x13Def} returns an object of class \code{c("SA","X13")}, a list containing the following components:
#'
#' \item{regarima}{object of class \code{c("regarima","X13")}. See \emph{Value} of the function \code{\link{regarima}}.}
#'
#' \item{decomposition}{object of class \code{"decomposition_X11"}, six elements list:
#' \itemize{
#' \item \code{specification} list with the X11 algorithm specification. See also function \code{\link{x13_spec}}
#' \item \code{mode} decomposition mode
#' \item \code{mstats} matrix with the  M statistics
#' \item \code{si_ratio} time series matrix (mts) with the \code{d8} and \code{d10} series
#' \item \code{s_filter} seasonal filters
#' \item \code{t_filter} trend filter
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
#'   mysa <- x13Def(myseries, spec=c("RSA5c"))
#'
#'   myspec1 <- x13_spec(mysa,tradingdays.option = "WorkingDays")
#'   mysa1 <- x13(myseries, myspec1)
#'   mysa1
#'   summary(mysa1$regarima)
#'
#'   myspec2<-x13_spec(mysa, usrdef.outliersEnabled = TRUE,
#'                              usrdef.outliersType = c("LS","AO"),
#'                              usrdef.outliersDate=c("2008-10-01","2002-01-01"),
#'                              usrdef.outliersCoef = c(36000,14000),
#'                              transform.function = "None")
#'   mysa2 <- x13(myseries, myspec2)
#'   mysa2
#'
#'   myspec3 <- x13_spec(mysa, automdl.enabled =FALSE,
#'                                arima.p=1,arima.q=1, arima.bp=0, arima.bq=1,
#'                                arima.coefEnabled = TRUE,
#'                                arima.coef = c(-0.8,-0.6,0),
#'                                arima.coefType = c(rep("Fixed",2),"Undefined"))
#'   s_arimaCoef(myspec3)
#'   mysa3 <- x13(myseries, myspec3)
#'   plot(mysa3)
#'   plot(mysa3$regarima)
#'   plot(mysa3$decomposition)
#'
#'   mysa4 <- x13Def(myseries,"RSA5c",
#'   userdefined = c("decomposition.d18","decomposition.d19"))
#'   mysa4
#'
#' @export
x13 <-function(series, spec, userdefined = NULL){
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

  # Or, using the fonction x13JavaResults :
  # return(x13JavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))

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

    z[["user_defined"]] <- user_defined(userdefined,jrobct)

    class(z) <- c("SA","X13")
    return(z)
  }
}

#' @rdname x13
#' @name x13
#' @export
x13Def <-function(series, spec=c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c"),
                  userdefined = NULL){
  if (!is.ts(series)){
    stop("series must be a time series")
  }
  spec<-match.arg(spec)
  # create the java objects
  jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  jspec<-.jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)

  return(x13JavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))
}

#Extract the results of the SA of a X13 object
x13JavaResults <- function(jrslt, spec, userdefined){

  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "JD2_RegArima_java",internal = jrarima)
  jrobct <- new (Class = "JD2_X13_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }

  reg <- regarima_defX13(jrobj = jrobct_arima, spec = spec)
  deco <- decomp_defX13(jrobj = jrobct, spec = spec)
  fin <-final(jrobj = jrobct)
  diagn <- diagnostics(jrobj = jrobct)

  z <- list(regarima = reg, decomposition = deco, final = fin,
            diagnostics = diagn, user_defined = user_defined(userdefined,jrobct))
  z[["user_defined"]] <- user_defined(userdefined,jrobct)

  class(z) <- c("SA","X13")
  return(z)
}
sa_jd2r <- function(jrslt, spec, userdefined = NULL, ...){
  if(is.null(jresult))
    return(NULL)

  if(.jinstanceof(spec, "jdr/spec/tramoseats/TramoSeatsSpec")){
    tramoseatsJavaResults(jrslt = jrslt, spec = spec, userdefined = userdefined)
  }else{
    if(.jinstanceof(spec, "jdr/spec/x13/X13Spec")){
      x13JavaResults(jrslt = jrslt, spec = spec, userdefined = userdefined)
    }else{
      stop("Wrong spec argument")
    }

  }
}

