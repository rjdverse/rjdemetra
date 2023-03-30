setClass(
  Class="TramoSeats_java",
  contains = "ProcResults"
)

#' Seasonal Adjustment with TRAMO-SEATS
#'
#' @description
#' Functions to estimate the seasonally adjusted series (sa) with the TRAMO-SEATS method.
#' This is achieved by decomposing the time series (y) into the trend-cycle (t), the seasonal component (s) and the irregular component (i).
#' Calendar-related movements can be corrected in the pre-treatment (TRAMO) step.

#' \code{tramoseats} returns a preformatted result while \code{jtramoseats} returns the Java objects of the seasonal adjustment.
#'
#' @param series an univariate time series
#' @param spec a TRAMO-SEATS model specification. It can be the name (\code{character}) of a pre-defined
#' TRAMO-SEATS 'JDemetra+' model specification (see \emph{Details}), or an object of class
#' \code{c("SA_spec","TRAMO_SEATS")}. The default value is \code{"RSAfull"}.
#' @param userdefined a \code{character} vector containing the additional output variables (see \code{\link{user_defined_variables}}).
#'
#' @details
#'
#' The first step of a seasonal adjustment consists in pre-adjusting the time series with TRAMO. This is done by removing
#' its deterministic effects (calendar and outliers), using a regression model with ARIMA noise (RegARIMA, see: \code{\link{regarima}}).
#' In the second part, the pre-adjusted series is decomposed by the SEATS algorithm into the following components:
#' trend-cycle (t), seasonal component (s) and irregular component (i). The decomposition can be:
#' additive  (\eqn{y = t + s + i}) or multiplicative (\eqn{y = t * s * i}, in the latter case pre-adjustment and decomposition are performed
#' on (\eqn{log(y) = log(t) + log(s) + log(i)}).
#'
#' In the TRAMO-SEATS method, the second step - SEATS ("Signal Extraction in ARIMA Time Series") - performs
#' an ARIMA-based decomposition of an observed time series into unobserved components.
#' More information on this method at \url{https://jdemetra-new-documentation.netlify.app/m-seats-decomposition}.
#'
#' The available predefined 'JDemetra+' TRAMO-SEATS model specifications are described in the table below:

#' \tabular{rrrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
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
#'
#' \code{jtramoseats} returns a \code{\link{jSA}} object that contains the results of the seasonal adjustment without
#' any formatting. Therefore, the computation is faster than with the function \code{tramoseats}. The results of the seasonal
#' adjustment can be extracted with the function \code{\link{get_indicators}}.
#'
#' \code{tramoseats} returns an object of class \code{c("SA","TRAMO_SEATS")}, that is, a list containing :
#'
#' \item{regarima}{an object of class \code{c("regarima","TRAMO_SEATS")}. More info in the \emph{Value} section of the function \code{\link{regarima}}.}
#'
#' \item{decomposition}{an object of class \code{"decomposition_SEATS"}, that is a five-element list:
#' \itemize{
#' \item \code{specification} a list with the SEATS algorithm specification. See also the function \code{\link{tramoseats_spec}}.
#' \item \code{mode} the decomposition mode
#' \item \code{model} the SEATS model list: \code{model, sa, trend, seasonal, transitory, irregular},
#' each element being a matrix of estimated coefficients.
#' \item \code{linearized} the time series matrix (mts) with the stochastic series decomposition (input series \code{y_lin},
#' seasonally adjusted series \code{sa_lin}, trend \code{t_lin}, seasonal \code{s_lin}, irregular \code{i_lin})
#' \item \code{components} the time series matrix (mts) with the decomposition components (input series \code{y_cmp},
#' seasonally adjusted series \code{sa_cmp}, trend \code{t_cmp}, seasonal component \code{s_cmp}, irregular \code{i_cmp})
#' }
#' }
#'
#' \item{final}{an object of class \code{c("final","mts","ts","matrix")}. The matrix contains the final results of the seasonal adjustment:
#' the original time series (\code{y})and its forecast (\code{y_f}), the trend (\code{t}) and its forecast (\code{t_f}),
#' the seasonally adjusted series (\code{sa}) and its forecast (\code{sa_f}), the seasonal component (\code{s})and its forecast (\code{s_f}),
#' and the irregular component (\code{i}) and its forecast (\code{i_f}).}
#'
#' \item{diagnostics}{an object of class \code{"diagnostics"}, that is a list containing three types of tests results:
#' \itemize{
#' \item \code{variance_decomposition} a data.frame with the tests results on the relative contribution of the components to the stationary
#' portion of the variance in the original series, after the removal of the long term trend;
#' \item \code{residuals_test} a data.frame with the tests results of the presence of seasonality in the residuals
#' (including the statistic test values, the corresponding p-values and the parameters description);
#' \item \code{combined_test} the combined tests for stable seasonality in the entire series. The format is a two-element list with:
#' \code{tests_for_stable_seasonality}, a data.frame containing the tests results (including the statistic test value, its p-value and the parameters
#' description), and \code{combined_seasonality_test}, the summary.
#' }}
#' \item{user_defined}{an object of class \code{"user_defined"}: a list containing the additional userdefined variables.}
#'
#' @references
#' More information and examples related to 'JDemetra+' features in the online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#' @seealso \code{\link{tramoseats_spec}}, \code{\link{x13}}
#'
#' @examples \donttest{
#' #Example 1
#' myseries <- ipi_c_eu[, "FR"]
#' myspec <- tramoseats_spec("RSAfull")
#' mysa <- tramoseats(myseries, myspec)
#' mysa
#'
#' # Equivalent to:
#' mysa1 <- tramoseats(myseries, spec = "RSAfull")
#' mysa1
#'
#' #Example 2
#' var1 <- ts(rnorm(length(myseries))*10, start = start(myseries), frequency = 12)
#' var2 <- ts(rnorm(length(myseries))*100, start = start(myseries), frequency = 12)
#' var <- ts.union(var1, var2)
#' myspec2 <- tramoseats_spec(myspec, tradingdays.mauto = "Unused",
#'                            tradingdays.option = "WorkingDays",
#'                            easter.type = "Standard",
#'                            automdl.enabled = FALSE, arima.mu = TRUE,
#'                            usrdef.varEnabled = TRUE, usrdef.var = var)
#' s_preVar(myspec2)
#' mysa2 <- tramoseats(myseries, myspec2,
#'                     userdefined = c("decomposition.sa_lin_f",
#'                                     "decomposition.sa_lin_e"))
#' mysa2
#' plot(mysa2)
#' plot(mysa2$regarima)
#' plot(mysa2$decomposition)
#' }
#' @export
tramoseats <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                       userdefined = NULL){
  if (!is.ts(series)) {
    stop("The series must be a time series!")
  }
  UseMethod("tramoseats", spec)
}
#' @export
tramoseats.SA_spec <- function(series, spec,
                      userdefined = NULL){
  jsa_obj <- jtramoseats.SA_spec(series, spec)
  jrslt <- jsa_obj[["result"]]@internal
  jrspec <- jsa_obj[["spec"]]

  # Or, using the fonction x13JavaResults:
  # return(tramoseatsJavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))

  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new (Class = "TRAMO_java",internal = jrarima)
  jrobct <- new (Class = "TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal)){
    return (NaN)
  }else{

    # Error during the preliminary check
    res = jrslt$getResults()$getProcessingInformation()

    if(is.null(jrslt$getDiagnostics()) & !.jcall(res,"Z","isEmpty")){
      proc_info <- jrslt$getResults()$getProcessingInformation()

      error_msg <- .jcall(proc_info, "Ljava/lang/Object;", "get", 0L)$getErrorMessages(proc_info)
      warning_msg <- .jcall(proc_info, "Ljava/lang/Object;", "get", 0L)$getWarningMessages(proc_info)
      if(!.jcall(error_msg,"Z","isEmpty"))
        stop(error_msg$toString())
      if(!.jcall(warning_msg,"Z","isEmpty"))
        warning(warning_msg$toString())
    }
    reg <- regarima_TS(jrobj = jrobct_arima, spec = spec$regarima)
    deco <- decomp_TS(jrobj = jrobct, spec = spec$seats)
    fin <- final(jrobj = jrobct)
    diagn <- diagnostics(jrobj = jrobct)

    z <- list(regarima = reg, decomposition = deco, final = fin,
              diagnostics = diagn,
              user_defined = user_defined(userdefined, jrobct))

    class(z) <- c("SA","TRAMO_SEATS")
    return(z)
  }
}
#' @export
tramoseats.character <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                           userdefined = NULL){
  jsa_obj <- jtramoseats.character(series, spec)
  jrslt <- jsa_obj[["result"]]@internal
  jrspec <- jsa_obj[["spec"]]

  return(tramoseatsJavaResults(jrslt = jrslt, spec = jrspec, userdefined = userdefined))
}

tramoseatsJavaResults <- function(jrslt, spec,
                                  userdefined = NULL,
                                  context_dictionary = NULL,
                                  extra_info = FALSE,
                                  freq = NA){
  jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
  jrobct_arima <- new(Class = "TRAMO_java",internal = jrarima)
  jrobct <- new(Class = "TramoSeats_java", internal = jrslt)

  if (is.null(jrobct@internal))
    return(NaN)

  # Error in preliminary check
  res = jrslt$getResults()$getProcessingInformation()

  if(is.null(jrslt$getDiagnostics()) & !.jcall(res,"Z","isEmpty")){
    proc_info <- jrslt$getResults()$getProcessingInformation()
    error_msg <- .jcall(proc_info, "Ljava/lang/Object;", "get", 0L)$getErrorMessages(proc_info)
    warning_msg <- .jcall(proc_info, "Ljava/lang/Object;", "get", 0L)$getWarningMessages(proc_info)
    if(!.jcall(error_msg,"Z","isEmpty"))
      stop(error_msg$toString())
    if(!.jcall(warning_msg,"Z","isEmpty"))
      warning(warning_msg$toString())
  }

  reg <- regarima_defTS(jrobj = jrobct_arima, spec = spec,
                        context_dictionary = context_dictionary,
                        extra_info = extra_info,
                        freq = freq)
  deco <- decomp_defTS(jrobj = jrobct, spec = spec)
  fin <- final(jrobj = jrobct)
  diagn <- diagnostics(jrobj = jrobct)

  z <- list(regarima = reg, decomposition = deco, final = fin,
            diagnostics = diagn,
            user_defined = user_defined(userdefined, jrobct))

  class(z) <- c("SA","TRAMO_SEATS")
  return(z)

}
