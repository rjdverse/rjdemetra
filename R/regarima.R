# To define a S4 java object
setClass(
  Class = "RegArima_java",
  contains = "ProcResults"
)
setClass(
  Class = "TRAMO_java",
  contains = "ProcResults"
)
#' RegARIMA model, pre-adjustment in X13 and TRAMO-SEATS
#' @description
#' The \code{regarima/regarima_x13/regarima_tramoseats} functions remove deterministic effects from the input series (e.g.calendar effects, outliers)
#' using a multivariate regression model with arima errors.
#' The \code{jregarima/jregarima_x13/jregarima_tramoseats} functions do the same computation but return the Java objects instead of
#' a formatted output.
#'
#' @param series an univariate time series
#' @param spec the model specification. For the function:
#' \itemize{
#' \item \code{regarima}: an object of class \code{c("regarima_spec","X13") or c("regarima_spec","TRAMO_SEATS")}.
#' See the functions \code{\link{regarima_spec_x13} and \link{regarima_spec_tramoseats}}.
#' \item \code{regarima_x13}: the name of a predefined X13 'JDemetra+' model specification (see \emph{Details}). The default value is "RG5c".
#' \item \code{regarima_tramoseats}:the name of a predefined TRAMO-SEATS 'JDemetra+' model specification (see \emph{Details}).
#' The default value is "TRfull".
#'}
#'
#' @details
#' When seasonally adjusting with X13 and TRAMO-SEATS, the first step consists in pre-adjusting the original series with a RegARIMA model,
#' where the original series is corrected for any deterministic effects and missing observations.
#' This step is also referred to as the linearization of the original series.
#'
#' The RegARIMA model (model with ARIMA errors) is specified as such:
#'
#' \deqn{z_t = y_t\beta + x_t}
#'
#' where:
#' \itemize{
#' \item \eqn{z_t} is the original series;
#' \item \eqn{\beta = (\beta_1,...,\beta_n)} is a vector of regression coefficients;
#' \item \eqn{y_t = (y_{1t},...,y_{nt})} are \eqn{n} regression variables (outliers, calendar effects, user-defined variables);
#' \item \eqn{x_t} is a disturbance that follows the general ARIMA process:
#' \eqn{\phi(B)\delta(B)x_t = \theta(B)a_t}; where \eqn{\phi(B), \delta(B)} and \eqn{\theta(B)} are finite polynomials in \eqn{B}
#' and \eqn{a_t} is a white noise variable with zero mean and a constant variance.
#' }
#'
#' The polynomial \eqn{\phi(B)} is a stationary autoregressive (AR) polynomial in \eqn{B},
#' which is a product of the stationary regular AR polynomial in \eqn{B} and the stationary seasonal polynomial
#' in \eqn{B^s}:
#'
#' \deqn{\phi(B)=\phi_p(B)\Phi_{bp}(B^s)=(1+\phi_1B+...+\phi_pB^p)(1+\Phi_1B^s+...+\Phi_{bp}B^{bps})}
#'
#' where:
#' \itemize{
#' \item \eqn{p} is the number of regular AR terms (here and in 'JDemetra+', \eqn{p \le 3});
#' \item \eqn{bp} is the number of seasonal AR terms (here and in 'JDemetra+', \eqn{bp \le 1});
#' \item \eqn{s} is the  number of observations per year (ie. The time series frequency).
#' }
#'
#' The polynomial \eqn{\theta(B)} is an invertible moving average (MA) polynomial in \eqn{B},
#' which is a product of the invertible regular MA polynomial in \eqn{B} and the invertible seasonal MA polynomial in \eqn{B^s}:
#'
#' \deqn{\theta(B)=\theta_q(B)\Theta_{bq}(B^s)=(1+\theta_1B+...+\theta_qB^q)(1+\Theta_1B^s+...+\Theta_{bq}B^{bqs})}
#'
#' where:
#' \itemize{
#' \item \eqn{q} is the number of regular MA terms (here and in 'JDemetra+', \eqn{q \le 3});
#' \item \eqn{bq} is the number of seasonal MA terms (here and in 'JDemetra+', \eqn{bq \le 1}).
#' }
#'
#' The polynomial \eqn{\delta(B)} is the non-stationary AR polynomial in \eqn{B} (unit roots):
#'
#' \deqn{\delta(B) = (1-B)^d(1-B^s)^{d_s}}
#'
#' where:
#' \itemize{
#' \item \eqn{d} is the regular differencing order (here and in 'JDemetra+', \eqn{d \le 1});
#' \item \eqn{d_s} is the seasonal differencing order (here and in 'JDemetra+', \eqn{d_s \le 1}).
#' }
#'
#' NB. The notations used for AR and MA processes, as well as the model denoted as ARIMA \eqn{(P,D,Q)(BP,BD,BQ)},
#' are consistent with those in 'JDemetra+'.
#'
#' The available predefined 'JDemetra+' X13 and TRAMO-SEATS model specifications are described in the tables below:
#'
#' \strong{X13:}
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RG0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RG3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RG4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RG5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
#' }
#'
#' \strong{TRAMO-SEATS:}
#' \tabular{rrrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' TR0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' TR1 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab Airline(+mean)\cr
#' TR2 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' TR3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' TR4 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' TR5 |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
#' TRfull |\tab automatic |\tab AO/LS/TC |\tab automatic |\tab automatic
#' }
#'
#' @return
#'
#' The \code{jregarima/jregarima_x13/jregarima_tramoseats} functions return a \code{\link{jSA}} object
#' that contains the result of the pre-adjustment method without any formatting. Therefore, the computation
#' is faster than with the \code{regarima/regarima_x13/regarima_tramoseats} functions.
#' The results of the seasonal adjustment can be extracted with the function \code{\link{get_indicators}}.
#'
#' The \code{regarima/regarima_x13/regarima_tramoseats} functions return an object of class \code{"regarima"}
#' and sub-class \code{"X13"} or \code{"TRAMO_SEATS"}.
#' \code{regarima_x13} returns an object of class \code{c("regarima","X13")} and \code{regarima_tramoseats},
#' an object of class \code{c("regarima","TRAMO_SEATS")}.
#' For the function \code{regarima}, the sub-class of the object depends on the used method that is defined by
#' the \code{spec} object class.
#'
#' An object of class \code{"regarima"} is a list containing the following components:
#'
#' \item{specification}{a list with the model specification as defined by the \code{spec} argument.
#' See also the Value of the \code{\link{regarima_spec_x13}} and  \code{\link{regarima_spec_tramoseats}} functions.}
#'
#' \item{arma}{a vector containing the orders of the autoregressive (AR), moving average (MA), seasonal AR and seasonal MA processes,
#' as well as the regular and seasonal differencing orders (P,D,Q) (BP,BD,BQ).}
#'
#' \item{arima.coefficients}{a matrix containing the estimated regular and seasonal AR and MA coefficients, as well as
#' the associated standard errors and t-statistics values. The estimated coefficients can be also extracted
#' with the function \code{\link[stats]{coef}} (whose output also includes the regression coefficients).}
#'
#' \item{regression.coefficients}{a matrix containing the estimated regression variables (i.e.: mean, calendar effect, outliers
#' and user-defined regressors) coefficients, as well as the associated standard errors and t-statistics values.
#' The estimated coefficients can be also extracted with the function \code{\link[stats]{coef}} (whose output also includes
#' the arima coefficients).}
#'
#' \item{loglik}{a matrix containing the log-likelihood of the RegARIMA model as well as the associated model selection criteria statistics
#' (AIC, AICC, BIC and BICC) and parameters (\code{np} = number of parameters in the likelihood, \code{neffectiveobs} = number
#' of effective observations in the likelihood). These statistics can also be extracted with the function \code{\link[stats]{logLik}}.}
#'
#' \item{model}{a list containing information on the model specification after its estimation (\code{spec_rslt}), as well as
#' the decomposed elements of the input series (ts matrix, \code{effects}). The model specification includes information on the estimation method
#' (\code{Model}) and time span (\code{T.span}), whether the original series was log transformed (\code{Log transformation})
#' and details on the regression part of the RegARIMA model i.e. if it includes a \code{Mean}, \code{Trading days} effects
#' (if so, it provides the number of regressors), \code{Leap year} effect, \code{Easter} effect and whether outliers were detected
#' (\code{Outliers} (if so, it provides the number of outliers). The decomposed elements of the input series contain the linearised series
#' (\code{y_lin}) and the deterministic components i.e.: trading days effect (\code{tde}), Easter effect (\code{ee}), other moving holidays effect
#' (\code{omhe}) and outliers effect (total - \code{out}, related to irregular - \code{out_i}, related to trend - \code{out_t},
#' related to seasonal - \code{out_s}).}
#'
#' \item{residuals}{the residuals (time series). They can be also extracted with the function \code{\link[stats]{residuals}}.}
#'
#' \item{residuals.stat}{a list containing statistics on the RegARIMA residuals. It provides the residuals standard error (\code{st.error})
#' and the results of normality, independence and linearity of the residuals (\code{tests}) - object of class \code{c("regarima_rtests","data.frame")}.}
#'
#' \item{forecast}{a ts matrix containing the forecast of the original series (\code{fcst}) and its standard error (\code{fcsterr}).}
#'
#'
#' @references
#' More information and examples related to 'JDemetra+' features in the online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#' BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.
#'
#' BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.
#'
#'
#' @examples\donttest{
#'  # X13 method
#' myseries <- ipi_c_eu[, "FR"]
#' myreg <- regarima_x13(myseries, spec ="RG5c")
#' summary(myreg)
#' plot(myreg)
#'
#' myspec1 <- regarima_spec_x13(myreg, tradingdays.option = "WorkingDays")
#' myreg1 <- regarima(myseries, myspec1)
#'
#' myspec2 <- regarima_spec_x13(myreg, usrdef.outliersEnabled = TRUE,
#'              usrdef.outliersType = c("LS", "AO"),
#'              usrdef.outliersDate = c("2008-10-01", "2002-01-01"),
#'              usrdef.outliersCoef = c(36, 14),
#'              transform.function = "None")
#' myreg2 <- regarima(myseries, myspec2)
#' myreg2
#'
#' myspec3 <- regarima_spec_x13(myreg, automdl.enabled = FALSE,
#'              arima.p = 1, arima.q = 1,
#'              arima.bp = 0, arima.bq = 1,
#'              arima.coefEnabled = TRUE,
#'              arima.coef = c(-0.8, -0.6, 0),
#'              arima.coefType = c(rep("Fixed", 2), "Undefined"))
#' s_arimaCoef(myspec3)
#' myreg3 <- regarima(myseries, myspec3)
#' summary(myreg3)
#' plot(myreg3)
#'
#'  # TRAMO-SEATS method
#' myspec <- regarima_spec_tramoseats("TRfull")
#' myreg <- regarima(myseries, myspec)
#' myreg
#'
#' myspec2 <- regarima_spec_tramoseats(myspec, tradingdays.mauto = "Unused",
#'              tradingdays.option = "WorkingDays",
#'              easter.type = "Standard",
#'              automdl.enabled = FALSE, arima.mu = TRUE)
#' myreg2 <- regarima(myseries, myspec2)
#'
#' var1 <- ts(rnorm(length(myseries))*10, start = start(myseries), frequency = 12)
#' var2 <- ts(rnorm(length(myseries))*100, start = start(myseries), frequency = 12)
#' var <- ts.union(var1, var2)
#' myspec3 <- regarima_spec_tramoseats(myspec,
#'              usrdef.varEnabled = TRUE, usrdef.var = var)
#' s_preVar(myspec3)
#' myreg3 <- regarima(myseries, myspec3)
#' myreg3
#' }
#' @export
# Generic function to create a "regarima" S3 class object from a user-defined specification (for X13 or TRAMO-SEATS method)
regarima <- function(series, spec = NA){
  UseMethod("regarima", spec)
}
# Method: "X13"
#' @export
regarima.X13 <- function(series, spec = NA){
  jsa_obj <- jregarima.X13(series, spec)
  jrobct <- jsa_obj[["result"]]

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_X13(jrobj = jrobct, spec = spec)
    return(z)
  }
}

# Method: "TRAMO_SEATS"
#' @export
regarima.TRAMO_SEATS <- function(series, spec = NA){
  jsa_obj <- jregarima.TRAMO_SEATS(series, spec)
  jrobct <- jsa_obj[["result"]]

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_TS(jrobj = jrobct, spec = spec)
    return(z)
  }
}

# The function creates a "regarima" S3 class object from a JD+ defined specification for TRAMO-SEATS method
#' @rdname regarima
#' @name regarima
#' @export
regarima_tramoseats <- function(series, spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5")){
  jsa_obj <- jregarima_tramoseats(series, spec)
  jrobct <- jsa_obj[["result"]]
  jrspec <- jsa_obj[["spec"]]

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_defTS(jrobj = jrobct, spec = jrspec)
    return(z)
  }
}

# The function creates a "regarima" S3 class object from a JD+ defined specification for X13 method
#' @rdname regarima
#' @name regarima
#' @export
regarima_x13 <- function(series, spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c")){
  jsa_obj <- jregarima_x13(series, spec)
  jrobct <- jsa_obj[["result"]]
  jrspec <- jsa_obj[["spec"]]

  if (is.null(jrobct@internal)) {
    return(NaN)
  }else{
    z <- regarima_defX13(jrobj = jrobct, spec = jrspec)
    return(z)
  }
}

regarima_defX13 <- function(jrobj, spec, context_dictionary = NULL,
                            extra_info = FALSE,
                            freq = NA){
  horizon <- -2
  # To extract model specification from the Java object
  rspec <- spec_regarima_X13_jd2r(spec = spec, context_dictionary = context_dictionary,
                        extra_info = extra_info, freq = freq)

  estimate <- with(rspec,
                   data.frame(preliminary.check = preliminary.check,
                              span = estimate.span, tolerance = estimate.tol,
                              row.names = "", stringsAsFactors = FALSE)
  )
  transform <- with(rspec,
                    data.frame(tfunction = transform.function,
                               adjust = transform.adjust,
                               aicdiff = transform.aicdiff,
                               row.names = "", stringsAsFactors = FALSE)
  )
  trading.days <- with(rspec,
                       data.frame(option = tradingdays.option,
                                  autoadjust = tradingdays.autoadjust,
                                  leapyear = tradingdays.leapyear,
                                  stocktd = tradingdays.stocktd,
                                  test = tradingdays.test, row.names = "", stringsAsFactors = FALSE)
  )
  easter <- with(rspec,
                 data.frame(enabled = easter.enabled, julian = easter.julian,
                            duration = easter.duration, test = easter.test,
                            row.names = "", stringsAsFactors = FALSE)
  )
  regression <- with(rspec,
                     list(userdef = userdef_spec, trading.days = trading.days, easter = easter)
  )
  outliers <- with(rspec,
                   data.frame(enabled = outlier.enabled, span = outlier.span,
                              ao = outlier.ao, tc = outlier.tc, ls = outlier.ls,
                              so = outlier.so, usedefcv = outlier.usedefcv,
                              cv = outlier.cv, method = outlier.method,
                              tcrate = outlier.tcrate,
                              row.names = "", stringsAsFactors = FALSE)
  )
  arima.dsc <- with(rspec,
                    data.frame(enabled = automdl.enabled, automdl.acceptdefault = automdl.acceptdefault,
                               automdl.cancel = automdl.cancel, automdl.ub1 = automdl.ub1,
                               automdl.ub2 = automdl.ub2, automdl.mixed = automdl.mixed,
                               automdl.balanced = automdl.balanced, automdl.armalimit = automdl.armalimit,
                               automdl.reducecv = automdl.reducecv, automdl.ljungboxlimit = automdl.ljungboxlimit,
                               automdl.ubfinal = automdl.ubfinal, arima.mu = arima.mu,
                               arima.p = arima.p, arima.d = arima.d, arima.q = arima.q,
                               arima.bp = arima.bp, arima.bd = arima.bd, arima.bq = arima.bq,
                               arima.coef = arima.coef, row.names = "", stringsAsFactors = FALSE)
  )
  arima <- with(rspec,
                list(specification = arima.dsc, coefficients = arima.coef.spec)
  )
  forecast <- data.frame(horizon = horizon, row.names = "", stringsAsFactors = FALSE)
  span <- rspec$span

  # specification
  specification <- list(estimate = estimate, transform = transform,
                        regression = regression, outliers = outliers,
                        arima = arima, forecast = forecast, span = span)
  # results
  jd_results <- regarima_rslts(jrobj,as.numeric(forecast))

  # new S3 class "regarima"
  z <- list(specification = specification,
            arma = jd_results$arma,
            arima.coefficients = jd_results$arima.coefficients,
            regression.coefficients = jd_results$regression.coefficients,
            loglik = jd_results$loglik,
            model = jd_results$model,
            residuals = jd_results$residuals,
            residuals.stat = jd_results$residuals.stat,
            forecast = jd_results$forecast)

  class(z) <- c("regarima","X13")
  return(z)
}

regarima_defTS <- function(jrobj, spec, context_dictionary = NULL,
                           extra_info = FALSE, freq = NA){
  # To extract model specification from the Java object

  horizon <- -2
  rspec <- spec_TRAMO_jd2r(spec = spec, context_dictionary = context_dictionary,
                        extra_info = extra_info, freq = freq)

  estimate <- with(rspec,
                   data.frame(preliminary.check = preliminary.check,
                              span = estimate.span, tolerance = estimate.tol,
                         exact_ml = estimate.eml, urfinal = estimate.urfinal,
                         row.names = "", stringsAsFactors = FALSE)
  )
  transform <- with(rspec,
                    data.frame(tfunction = transform.function,fct = transform.fct,
                          row.names = "", stringsAsFactors = FALSE)
  )
  trading.days <- with(rspec,
                       data.frame(automatic = tradingdays.mauto, pftd = tradingdays.pftd,
                             option = tradingdays.option, leapyear = tradingdays.leapyear,
                             stocktd = tradingdays.stocktd, test = tradingdays.test,
                             row.names = "", stringsAsFactors = FALSE)
  )
  easter <- with(rspec,
                 data.frame(type = easter.type,julian = easter.julian,
                       duration = easter.duration,test = easter.test,
                       row.names = "", stringsAsFactors = FALSE)
  )
  regression <- with(rspec,
                     list(userdef = userdef_spec, trading.days = trading.days, easter = easter)
  )
  outliers <- with(rspec,
                   data.frame(enabled = outlier.enabled,span = outlier.span,
                         ao = outlier.ao, tc = outlier.tc, ls = outlier.ls,
                         so = outlier.so,usedefcv = outlier.usedefcv,cv = outlier.cv,eml = outlier.eml,
                         tcrate = outlier.tcrate, row.names = "", stringsAsFactors = FALSE)
  )
  arima.dsc <- with(rspec,
                    data.frame(enabled = automdl.enabled,automdl.acceptdefault = automdl.acceptdefault,
                          automdl.cancel = automdl.cancel,
                          automdl.ub1 = automdl.ub1,automdl.ub2 = automdl.ub2,
                          automdl.armalimit = automdl.armalimit,
                          automdl.reducecv = automdl.reducecv,
                          automdl.ljungboxlimit = automdl.ljungboxlimit, compare = automdl.compare,
                          arima.mu = arima.mu,arima.p = arima.p,arima.d = arima.d,arima.q = arima.q,
                          arima.bp = arima.bp,arima.bd = arima.bd,arima.bq = arima.bq, arima.coef = arima.coef,
                          row.names = "", stringsAsFactors = FALSE)
  )
  arima <- with(rspec,
                list(specification = arima.dsc, coefficients = arima.coef.spec)
  )
  forecast <- with(rspec,
                   data.frame(horizon = horizon,row.names = "", stringsAsFactors = FALSE)
  )
  span <- rspec$span

  # specification
  specification <- list(estimate = estimate, transform = transform, regression = regression, outliers = outliers, arima = arima,
                        forecast = forecast, span = span)

  # results
  jd_results <- regarima_rslts(jrobj,as.numeric(forecast))

  # new S3 class "regarima"
  z <- list(specification = specification,
            arma = jd_results$arma,
            arima.coefficients = jd_results$arima.coefficients,
            regression.coefficients = jd_results$regression.coefficients,
            loglik = jd_results$loglik,
            model = jd_results$model,
            residuals = jd_results$residuals,
            residuals.stat = jd_results$residuals.stat,
            forecast = jd_results$forecast)

  class(z) <- c("regarima","TRAMO_SEATS")
  return(z)
}

regarima_X13 <- function(jrobj, spec){
  # results
  jd_results <- regarima_rslts(jrobj, as.numeric(s_fcst(spec)))
  # To import the model specification
  estimate <- s_estimate(spec)
  transform <- s_transform(spec)
  usrdef <- s_usrdef(spec)
  predef.outliers <- s_preOut(spec)
  predef.variables <- s_preVar(spec)
  trading.days <- s_td(spec)
  easter <- s_easter(spec)
  outliers <- s_out(spec)
  arima.dsc <- s_arima(spec)
  predef.coef <- s_arimaCoef(spec)
  span <- s_span(spec)
  userdef <- list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
  regression <- list(userdef = userdef, trading.days = trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = predef.coef)
  forecast <- s_fcst(spec)
  # specification
  specification <- list(estimate = estimate, transform = transform, regression = regression,
                        outliers = outliers, arima = arima, forecast = forecast, span = span)
  # the new S3 class "regarima"
  z <- list(specification = specification,
            arma = jd_results$arma,
            arima.coefficients = jd_results$arima.coefficients,
            regression.coefficients = jd_results$regression.coefficients,
            loglik = jd_results$loglik,
            model = jd_results$model,
            residuals = jd_results$residuals,
            residuals.stat = jd_results$residuals.stat,
            forecast = jd_results$forecast)

  class(z) = c("regarima","X13")
  return(z)
}

regarima_TS <- function(jrobj, spec){
  # results
  jd_results <- regarima_rslts(jrobj,as.numeric(s_fcst(spec)))
  # To import the model specification
  estimate <- s_estimate(spec)
  transform <- s_transform(spec)
  usrdef <- s_usrdef(spec)
  predef.outliers <- s_preOut(spec)
  predef.variables <- s_preVar(spec)
  trading.days <- s_td(spec)
  easter <- s_easter(spec)
  outliers <- s_out(spec)
  arima.dsc <- s_arima(spec)
  predef.coef <- s_arimaCoef(spec)
  span <- s_span(spec)
  userdef <- list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
  regression <- list(userdef = userdef, trading.days = trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = predef.coef)
  forecast <- s_fcst(spec)
  # specification
  specification <- list(estimate = estimate, transform = transform, regression = regression,
                        outliers = outliers, arima = arima, forecast = forecast, span = span)
  # The new S3 class "regarima"
  z <- list(specification = specification,
            arma = jd_results$arma,
            arima.coefficients = jd_results$arima.coefficients,
            regression.coefficients = jd_results$regression.coefficients,
            loglik = jd_results$loglik,
            model = jd_results$model,
            residuals = jd_results$residuals,
            residuals.stat = jd_results$residuals.stat,
            forecast = jd_results$forecast)

  class(z) = c("regarima","TRAMO_SEATS")
  return(z)
}




