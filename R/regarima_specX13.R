#' RegARIMA model specification, pre-adjustment in X13
#' @description
#'
#' \code{regarima_spec_def_x13} creates (and modifies), from a predefined 'JDemetra+' model specification, a \code{c("regarima_spec","X13")} class object with the RegARIMA model specification for the X13 method.
#'
#' \code{regarima_spec_x13} creates (and/or modifies) a \code{c("regarima_spec","X13")} class object with the RegARIMA model specification for the X13 method. The object is created from a \code{c("regarima","X13")} or \code{c("regarima_spec","X13")} class object.
#'
#' @param spec predefined 'JDemetra+' model specification (see \emph{Details}). The default is "RG5c".
#'
#' The time span of the series to be used for the estimation of the RegARIMA model coefficients (default from 1900-01-01 to 2020-12-31) is controlled by the following six variables: \code{estimate.from, estimate.to, estimate.first, estimate.last, estimate.exclFirst} and \code{estimate.exclLast}; where \code{estimate.from} and \code{estimate.to} have priority over remaining span control variables, \code{estimate.last} and \code{estimate.first} have priority over \code{estimate.exclFirst} and \code{estimate.exclLast}, and \code{estimate.last} has priority over \code{estimate.first}.
#'
#' @param estimate.from character in format "YYYY-MM-DD" indicating the start of the time span (e.g. "1900-01-01"). Can be combined with \code{estimate.to}.
#'
#' @param estimate.to character in format "YYYY-MM-DD" indicating the end of the time span (e.g. "2020-12-31"). Can be combined with \code{estimate.from}.
#'
#' @param estimate.first numeric specifying the number of periods considered at the beginning of the series.
#'
#' @param estimate.last numeric specifying the number of periods considered at the end of the series.
#'
#' @param estimate.exclFirst numeric specifying the number of periods excluded at the beginning of the series. Can be combined with \code{estimate.exclLast}.
#'
#' @param estimate.exclLast numeric specifying the number of periods excluded at the end of the series. Can be combined with \code{estimate.exclFirst}.
#'
#' @param estimate.tol numeric, convergence tolerance. The absolute changes in the log-likelihood function are compared to this value to check for the convergence of the estimation iterations.
#'
#' @param transform.function transformation of the input series: \code{"None"} - no transformation of the series; \code{"Log"} - takes the log of the series; \code{"Auto"} - the program tests for the log-level specification.
#'
#' @param transform.adjust pre-adjustment of the input series for length of period or leap year effects: \code{"None"} - no adjustment; \code{"LeapYear"} - leap year effect; \code{"LengthOfPeriod"} - length of period. Modifications of this variable are taken into account only when \code{transform.function} is set to \code{"Log"}.
#'
#' @param transform.aicdiff numeric defining the difference in AICC needed to accept no transformation when the automatic transformation selection is chosen (considered only when \code{transform.function} is set to \code{"Auto"}).
#'
#' Control variables for the pre-specified outliers. The pre-specified outliers are used in the model only if they are enabled (\code{usrdef.outliersEnabled=TRUE}) and the outliers' type (\code{usrdef.outliersType}) and date (\code{usrdef.outliersDate}) are provided.
#'
#' @param usrdef.outliersEnabled logicals. If \code{TRUE} the program uses the pre-specified outliers.
#'
#' @param usrdef.outliersType vector defining the outliers' type. Possible types are: \code{("AO")} - additive, \code{("LS")} - level shift, \code{("TC")} - transitory change, \code{("SO")} -  seasonal outlier. E.g.: \code{ usrdef.outliersType =c("AO","AO","LS")}.
#'
#' @param usrdef.outliersDate vector defining the outliers' date. The dates should be characters in format "YYYY-MM-DD". E.g.: \code{usrdef.outliersDate=c("2009-10-01","2005-02-01","2003-04-01")}.
#'
#' @param usrdef.outliersCoef vector providing fixed coefficients for the outliers. The coefficients can't be fixed if  \code{ transform.function} is set to \code{"Auto"} - the series transformation need to be pre-defined. E.g.: \code{ usrdef.outliersCoef=c(200,170,20)}.
#'
#' Control variables for the user-defined variables:
#'
#' @param usrdef.varEnabled logicals. If \code{TRUE} the program uses the user-defined variables.
#'
#' @param usrdef.var time series (\code{ts}) or matrix of time series (\code{mts}) with the user-defined variables.
#'
#' @param usrdef.varType vector of character(s) defining the user-defined variables component type. Possible types are: \code{"Undefined", "Series", "Trend", "Seasonal", "SeasonallyAdjusted", "Irregular"}. If not specified, the program will assign the \code{"Undefined"} type.
#'
#' @param usrdef.varCoef vector providing fixed coefficients for the user-defined variables. The coefficients can't be fixed if  \code{ transform.function} is set to \code{"Auto"} - the series transformation need to be pre-defined.
#'
#' @param tradingdays.option defines the type of the trading days regression variables: \code{"TradingDays"} -  six day-of-the-week regression variables; \code{"WorkingDays"} - one working/non-working day contrast variable; \code{"None"} - no correction for trading days and working days effects; \code{"UserDefined"} - user-defined trading days regressors (currently not working). \code{"None"} has also to be chosen for the "day-of-week effects" correction (\code{tradingdays.stocktd} to be modified accordingly).
#'
#' @param tradingdays.autoadjust  logicals. If \code{TRUE} the program corrects automatically for the leap year effect. Modifications of this variable are taken into account only when \code{transform.function} is set to \code{"Auto"}.
#'
#' @param tradingdays.leapyear option for including the leap-year effect in the model: \code{"LeapYear"} - leap year effect; \code{"LengthOfPeriod"} - length of period, \code{"None"} - no effect included. The leap-year effect can be pre-specified in the model only if the input series was not pre-adjusted (\code{transform.adjust} set to \code{"None"}) and the automatic correction for the leap-year effect was not selected (\code{tradingdays.autoadjust} set to \code{FALSE}).
#'
#' @param tradingdays.stocktd numeric indicating the day of the month when inventories and other stock are reported (to denote the last day of the month set the variable to 31). Modifications of this variable are taken into account only when \code{tradingdays.option} is set to \code{"None"}.
#'
#' @param tradingdays.test defines the pre-tests for the significance of the trading day regression variables based on the AICC statistics: \code{"Add"} - the trading day variables are not included in the initial regression model but can be added to the RegARIMA model after the test; \code{"Remove"} - the trading day variables belong to the initial regression model but can be removed from the RegARIMA model after the test; \code{"None"} - the trading day variables are not pre-tested and are included in the model.
#'
#' @param easter.enabled logicals. If \code{TRUE} the program considers the Easter effect in the model.
#'
#' @param easter.julian logicals. If \code{TRUE} the program uses the Julian Easter (expressed in Gregorian calendar).
#'
#' @param easter.duration numeric indicating the duration of the Easter effect (length in days, between 1 and 20).
#'
#' @param easter.test defines the pre-tests for the significance of the Easter effect based on the t-statistic (Easter effect is considered as significant if the t-statistic is greater than 1.96): \code{"Add"} - the Easter effect variable is not included in the initial regression model but can be added to the RegARIMA model after the test; \code{"Remove"} - the Easter effect variable belong to the initial regression model but can be removed from the RegARIMA model after the test; \code{"None"} - the Easter effect variable is not pre-tested and is included in the model.
#'
#' @param outlier.enabled logicals. If \code{TRUE} the automatic detection of outliers is enabled in the defined time span.
#'
#' The time span of the series to be searched for outliers (default from 1900-01-01 to 2020-12-31) is controlled by the following six variables: \code{outlier.from, outlier.to, outlier.first, outlier.last, outlier.exclFirst} and \code{outlier.exclLast}; where \code{outlier.from} and \code{outlier.to} have priority over remaining span control variables, \code{outlier.last} and \code{outlier.first} have priority over \code{outlier.exclFirst} and \code{outlier.exclLast}, and \code{outlier.last} has priority over \code{outlier.first}.
#'
#' @param outlier.from character in format "YYYY-MM-DD" indicating the start of the time span (e.g. "1900-01-01"). Can be combined with \code{outlier.to}.
#'
#' @param outlier.to character in format "YYYY-MM-DD" indicating the end of the time span (e.g. "2020-12-31"). Can be combined with \code{outlier.from}.
#'
#' @param outlier.first numeric specifying the number of periods considered at the beginning of the series.
#'
#' @param outlier.last numeric specifying the number of periods considered at the end of the series.
#'
#' @param outlier.exclFirst numeric specifying the number of periods excluded at the beginning of the series. Can be combined with \code{outlier.exclLast}.
#'
#' @param outlier.exclLast numeric specifying the number of periods excluded at the end of the series. Can be combined with \code{outlier.exclFirst}.
#'
#' @param outlier.ao logicals. If \code{TRUE} the automatic detection of additive outliers is enabled (\code{outlier.enabled} must be also set to \code{TRUE}).
#'
#' @param outlier.tc logicals. If \code{TRUE} the automatic detection of transitory changes is enabled (\code{outlier.enabled} must be also set to \code{TRUE}).
#'
#' @param outlier.ls logicals. If \code{TRUE} the automatic detection of level shifts is enabled (\code{outlier.enabled} must be also set to \code{TRUE}).
#'
#' @param outlier.so logicals. If \code{TRUE} the automatic detection of seasonal outliers is enabled (\code{outlier.enabled} must be also set to \code{TRUE}).
#'
#' @param outlier.usedefcv logicals. If \code{TRUE} the critical value for the outliers' detection procedure is automatically determined by the number of observations in the outlier detection time span. If \code{FALSE} the procedure uses the inputted critical value (\code{outlier.cv}).
#'
#' @param outlier.cv numeric. Inputted critical value for the outliers' detection procedure. The modification of this variable is taken into account only when \code{outlier.usedefcv} is set to \code{FALSE}.
#'
#' @param outlier.method determines how the program successively adds detected outliers to the model. At present only the \code{AddOne} method is supported.
#'
#' @param outlier.tcrate numeric. The rate of decay for the transitory change outlier.
#'
#' @param automdl.enabled logicals. If \code{TRUE} the automatic modelling of the ARIMA model is enabled. If \code{FALSE} the parameters of the ARIMA model can be specified.
#'
#' Control variables for the automatic modelling of the ARIMA model (\code{automdl.enabled} is set to \code{TRUE}):
#'
#' @param automdl.acceptdefault logicals. If \code{TRUE} the default model (ARIMA(0,1,1)(0,1,1)) may be chosen in the first step of the automatic model identification. If the Ljung-Box Q statistics for the residuals is acceptable, the default model is accepted and no further attempt will be made to identify any other.
#'
#' @param automdl.cancel  numeric, cancelation limit. If the difference in moduli of an AR and an MA roots (when estimating ARIMA(1,0,1)(1,0,1) models in the second step of the automatic identification of the differencing orders) is smaller than cancelation limit, the two roots are assumed equal and cancel out.
#'
#' @param automdl.ub1 numeric, first unit root limit. It is the threshold value for the initial unit root test in the automatic differencing procedure. When one of the roots in the estimation of the ARIMA(2,0,0)(1,0,0) plus mean model, performed in the first step of the automatic model identification procedure, is larger than first unit root limit in modulus, it is set equal to unity.
#'
#' @param automdl.ub2 numeric, second unit root limit. When one of the roots in the estimation of the ARIMA(1,0,1)(1,0,1) plus mean model, which is performed in the second step of the automatic model identification procedure, is larger than second unit root limit in modulus, it is checked if there is a common factor in the corresponding AR and MA polynomials of the ARMA model that can be cancelled (see \code{automdl.cancel}). If there is no cancellation, the AR root is set equal to unity (i.e. the differencing order changes).
#'
#' @param automdl.mixed logicals. The variable controls whether ARIMA models with non-seasonal AR and MA terms or seasonal AR and MA terms will be considered in the automatic model identification procedure. If \code{FALSE} a model with AR and MA terms in both the seasonal and non-seasonal parts of the model can be acceptable, provided there are not AR and MA terms in either the seasonal or non-seasonal.
#'
#' @param automdl.balanced logicals. If \code{TRUE}, the automatic model identification procedure will have a preference for balanced models (i.e. models for which the order of the combined AR and differencing operator is equal to the order of the combined MA operator).
#'
#' @param automdl.armalimit numeric, arma limit. It is the threshold value for t-statistics of ARMA coefficients and constant term used for the final test of model parsimony. If the highest order ARMA coefficient has a t-value less than this value in magnitude, the order of the model is reduced. Also if the constant term has a t-value less than arma limit in magnitude, it is removed from the set of regressors.
#'
#' @param automdl.reducecv numeric, ReduceCV.  The percentage by which the outlier's critical value will be reduced when an identified model is found to have a Ljung-Box statistic with an unacceptable confidence coefficient. The parameter should be between 0 and 1, and will only be active when automatic outlier identification is enabled. The reduced critical value will be set to (1-ReduceCV)xCV, where CV is the original critical value.
#'
#' @param automdl.ljungboxlimit numeric, Ljung Box limit. Acceptance criterion for the confidence intervals of the Ljung-Box Q statistic. If the LjungBox Q statistics for the residuals of a final model is greater than Ljung Box limit, the model is rejected, the outlier critical value is reduced, and model and outlier identification (if specified) is redone with a reduced value.
#'
#' @param automdl.ubfinal numeric, final unit root limit. The threshold value for the final unit root test. If the magnitude of an AR root for the final model is less than the final unit root limit, a unit root is assumed, the order of the AR polynomial is reduced by one, and the appropriate order of the differencing (non-seasonal, seasonal) is increased. The parameter value should be greater than one.
#'
#' Control variables for the non-automatic modelling of the ARIMA model (\code{automdl.enabled} is set to \code{FALSE}):
#'
#' @param arima.mu logicals. If \code{TRUE}, the mean is considered as part of the ARIMA model.
#'
#' @param arima.p numeric. The order of the non-seasonal autoregressive (AR) polynomial.
#'
#' @param arima.d numeric. Regular differencing order.
#'
#' @param arima.q numeric. The order of the non-seasonal moving average (MA) polynomial.
#'
#' @param arima.bp numeric. The order of the seasonal autoregressive (AR) polynomial.
#'
#' @param arima.bd numeric. Seasonal differencing order.
#'
#' @param arima.bq numeric. The order of the seasonal moving average (MA) polynomial.
#'
#' Control variables for the user-defined ARMA coefficients. Coefficients can be defined for the regular and seasonal autoregressive (AR) polynomials and moving average (MA) polynomials. The model considers the coefficients only if the procedure for their estimation (\code{arima.coefType}) is provided, and the number of provided coefficients matches the sum of (regular and seasonal) AR and MA orders (\code{p,q,bp,bq}).
#'
#' @param arima.coefEnabled logicals. If \code{TRUE} the program uses the user-defined ARMA coefficients.
#'
#' @param arima.coef vector providing the coefficients for the regular and seasonal AR and MA polynominals. The length of the vector must equal the sum of the regular and seasonal AR and MA orders. The coefficients shall be provided in the order: regular AR (\emph{Phi} - \code{p} elements), regular MA  (\emph{Theta} - \code{q} elements), seasonal AR (\emph{BPhi} - \code{bp} elements) and seasonal MA (\emph{BTheta} - \code{bq} elements). E.g.: \code{arima.coef=c(0.6,0.7)} with \code{arima.p=1, arima.q=0,arima.bp=1} and \code{arima.bq=0}.
#'
#' @param arima.coefType vector defining ARMA coefficients estimation procedure. Possible procedures are: \code{"Undefined"} - no use of user-defined input (i.e. coefficients are estimated), \code{"Fixed"} - fixes the coefficients at the value provided by the user, \code{"Initial"} - the value defined by the user is used as initial condition. For orders for which the coefficients shall not be defined, the \code{arima.coef} can be set to \code{NA} or \code{0} or the \code{arima.coefType} can be set to \code{"Undefined"}.
#' E.g.: \code{arima.coef = c(-0.8,-0.6,NA)}, \code{arima.coefType = c("Fixed","Fixed","Undefined")}.
#'
#' @param fcst.horizon numeric, forecasts horizon. Length of the forecasts generated by the RegARIMA model in periods (positive values) or years (negative values). By default the program generates two years forecasts (\code{fcst.horizon} set to \code{-2}).
#'
#'
#' @details
#' The available predefined 'JDemetra+' model specifications (for the function \code{regarima_spec_def_x13}) are described in the table below.
#'
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calender effects} |\tab \strong{ARIMA}\cr
#' RG0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RG3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RG4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RG5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
#' }
#'
#' @return
#' A list of class \code{c("regarima_spec","X13")} with the below components. Each component refers to different part of the RegARIMA model specification, mirroring the arguments of the function (for details see arguments description). Each of the lowest-level component (except span, pre-specified outliers, user-defined variables and pre-specified ARMA coefficients) is structured within a data frame with columns denoting different variables of the model specification and rows referring to: first row - base specification, as provided within the argument \code{spec} or \code{object}; second row - user modifications as specified by the remaining arguments of the function (e.g.: \code{arima.d}); and third row - final model specification, values that will be used in the function \code{\link{regarima}}. The final specification (third row) shall include user modifications (row two) unless they were wrongly specified. The pre-specified outliers, user-defined variables and pre-specified ARMA coefficients consist of a list with the \code{Predefined} (base model specification) and \code{Final} values.
#'
#' \item{estimate}{data frame. Variables referring to: \code{span} - time span for the model estimation, \code{tolerance} - argument \code{estimate.tol}. The final values can be also accessed with the function \code{\link{s_estimate}}.}
#'
#' \item{transform}{data frame. Variables referring to: \code{tfunction} - argument \code{transform.function}, \code{adjust} - argument \code{transform.adjust}, \code{aicdiff} - argument \code{transform.aicdiff}. The final values can be also accessed with the function \code{\link{s_transform}}.}
#'
#' \item{regression}{list including the information on the user-defined variables (\code{userdef}), \code{trading.days} effect and \code{easter} effect. The user-defined part includes: \code{specification} - data frame with the information if pre-specified outliers (\code{outlier}) and user-defined variables (\code{variables}) are included in the model and if fixed coefficients are used (\code{outlier.coef} and \code{variables.coef}). The final values can be also accessed with the function \code{\link{s_usrdef}}; \code{outliers} - matrixes with the outliers (\code{Predefined} and \code{Final}). The final outliers can be also accessed with the function \code{\link{s_preOut}}; and \code{variables} - list with the \code{Predefined} and \code{Final} user-defined variables (\code{series}) and its description (\code{description}) including the information on the variable type and values of fixed coefficients. The final user-defined variables can be also accessed with the function \code{\link{s_preVar}}.
#' Within the data frame \code{trading.days} variables refer to: \code{option} - argument \code{tradingdays.option, autoadjust} - argument \code{tradingdays.autoadjust, leapyear} - argument \code{tradingdays.leapyear, stocktd} - argument \code{tradingdays.stocktd, test} - argument \code{tradingdays.test}. The final \code{trading.days} values can be also accessed with the function \code{\link{s_td}}. Within the data frame \code{easter} variables refer to: \code{enabled} - argument \code{easter.enabled, julian} - argument \code{easter.julian, duration} - argument \code{easter.duration, test} - argument \code{easter.test}. The final \code{easter} values can be also accessed with the function \code{\link{s_easter}}.}
#'
#' \item{outliers}{data frame. Variables referring to: \code{enabled} - argument \code{outlier.enabled},  \code{span}  - time span for the outliers' detection, \code{ao} - argument \code{outlier.ao, tc} - argument \code{outlier.tc, ls} - argument \code{outlier.ls, so} - argument \code{outlier.so, usedefcv} - argument \code{outlier.usedefcv, cv} - argument \code{outlier.cv, method} - argument \code{outlier.method, tcrate} - argument \code{outlier.tcrate}. The final values can be also accessed with the function \code{\link{s_out}}.}
#'
#' \item{arima}{list containing a data frame with the ARIMA settings (\code{specification}) and matrixes with the information on the pre-specified ARMA coefficients (\code{coefficients}). The matrix \code{Predefined} refers to the pre-defined model specification and matrix \code{Final} to the final specification. Both matrixes contain the value of the ARMA coefficients and the procedure for its estimation.
#' Within the data frame \code{specification} the variable \code{enabled} refer to the argument \code{automdl.enabled} and all the remaining variables (\code{automdl.acceptdefault, automdl.cancel, automdl.ub1, automdl.ub2, automdl.mixed, automdl.balanced, automdl.armalimit, automdl.reducecv, automdl.ljungboxlimit, automdl.ubfinal, arima.mu, arima.p, arima.d, arima.q, arima.bp, arima.bd, arima.bq}) to the respective function arguments.
#' The final values of the \code{specification} can be also accessed with the function \code{\link{s_arima}} and final pre-specified ARMA coefficients with the function \code{\link{s_arimaCoef}}.}
#'
#' \item{forecast}{data frame with the forecast horizon (argument \code{fcst.horizon}). The final value can be also accessed with the function \code{\link{s_fcst}}.}
#'
#' \item{span}{matrix containing the final time span for the model estimation and outliers' detection. Contains the same information as the variable span in the data frames estimate and outliers. The matrix can be also accessed with the function \code{\link{s_span}}.}
#'
#' @references
#' Info on 'JDemetra+', usage and functions:
#' \url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
#'
#' @examples
#' 
#' myseries <- ipi_c_eu[, "FR"]
#' myspec1 <- regarima_spec_def_x13(spec = "RG5c")
#' myreg1 <- regarima(myseries, spec = myspec1)
#' \donttest{
#'  # Modify a pre-specified model specification
#' myspec2 <- regarima_spec_def_x13(spec = "RG5c",
#'                                  tradingdays.option = "WorkingDays")
#' myreg2 <- regarima(myseries, spec = myspec2)
#'
#'  # Modify the model specification from a "regarima" object
#' myspec3 <- regarima_spec_x13(myreg1, tradingdays.option = "WorkingDays")
#' myreg3 <- regarima(myseries, myspec3)
#'
#'  # Modify the model specification from a "regarima_spec" object
#' myspec4 <- regarima_spec_x13(myspec1, tradingdays.option = "WorkingDays")
#' myreg4 <- regarima(myseries, myspec4)
#'
#'  # Pre-specified outliers
#' myspec1 <- regarima_spec_def_x13(spec = "RG5c", usrdef.outliersEnabled = TRUE,
#'               usrdef.outliersType = c("LS", "AO"),
#'               usrdef.outliersDate = c("2008-10-01", "2002-01-01"),
#'               usrdef.outliersCoef = c(36, 14),
#'               transform.function = "None")
#'
#' myreg1 <- regarima(myseries, myspec1)
#' myreg1
#' s_preOut(myreg1)
#'
#'
#'  # User-defined variables
#' var1 <- ts(rnorm(length(myseries))*10, start = start(myseries),
#'            frequency = 12)
#' var2 <- ts(rnorm(length(myseries))*100, start = start(myseries),
#'            frequency = 12)
#' var <- ts.union(var1, var2)
#'
#' myspec1 <- regarima_spec_def_x13(spec = "RG5c", usrdef.varEnabled = TRUE,
#'                                  usrdef.var = var)
#' myreg1 <- regarima(myseries, myspec1)
#' myreg1
#'
#' myspec2 <- regarima_spec_def_x13(spec="RG5c", usrdef.varEnabled = TRUE,
#'                                  usrdef.var = var1, usrdef.varCoef = 2,
#'                                  transform.function = "None")
#' myreg2 <- regarima(myseries, myspec2)
#' s_preVar(myreg2)
#'
#'  # Pre-specified ARMA coefficients
#' myspec1 <- regarima_spec_def_x13(spec="RG5c", automdl.enabled =FALSE,
#'              arima.p = 1, arima.q = 1, arima.bp = 0, arima.bq = 1,
#'              arima.coefEnabled = TRUE, arima.coef = c(-0.8, -0.6, 0),
#'              arima.coefType = c(rep("Fixed", 2), "Undefined"))
#'
#' s_arimaCoef(myspec1)
#' myreg1 <- regarima(myseries, myspec1)
#' myreg1
#' }
#' @export
# The function creates a "regarima_spec" S3 class object from a JD+ defined specification for X13 method
regarima_spec_def_x13  <-function(spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c"),
                            estimate.from = NA_character_,
                            estimate.to = NA_character_,
                            estimate.first = NA_integer_,
                            estimate.last = NA_integer_,
                            estimate.exclFirst = NA_integer_,
                            estimate.exclLast = NA_integer_,
                            estimate.tol = NA_integer_,
                            transform.function = c(NA, "Auto", "None", "Log"),
                            transform.adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                            transform.aicdiff = NA_integer_,
                            usrdef.outliersEnabled = NA,
                            usrdef.outliersType = NA,
                            usrdef.outliersDate = NA,
                            usrdef.outliersCoef = NA,
                            usrdef.varEnabled = NA,
                            usrdef.var = NA,
                            usrdef.varType = NA,
                            usrdef.varCoef = NA,
                            tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                            tradingdays.autoadjust = NA,
                            tradingdays.leapyear = c(NA, "LeapYear", "LengthOfPeriod","None"),
                            tradingdays.stocktd = NA_integer_,
                            tradingdays.test = c(NA, "Remove", "Add", "None"),
                            easter.enabled = NA,
                            easter.julian = NA,
                            easter.duration = NA_integer_,
                            easter.test = c(NA, "Add", "Remove", "None"),
                            outlier.enabled = NA,
                            outlier.from = NA_character_,
                            outlier.to = NA_character_,
                            outlier.first = NA_integer_,
                            outlier.last = NA_integer_,
                            outlier.exclFirst = NA_integer_,
                            outlier.exclLast = NA_integer_,
                            outlier.ao = NA,
                            outlier.tc = NA,
                            outlier.ls = NA,
                            outlier.so = NA,
                            outlier.usedefcv = NA,
                            outlier.cv = NA_integer_,
                            outlier.method = c(NA, "AddOne", "AddAll"),
                            outlier.tcrate  = NA_integer_,
                            automdl.enabled = NA,
                            automdl.acceptdefault = NA,
                            automdl.cancel = NA_integer_,
                            automdl.ub1 = NA_integer_,
                            automdl.ub2 = NA_integer_,
                            automdl.mixed = NA,
                            automdl.balanced = NA,
                            automdl.armalimit = NA_integer_,
                            automdl.reducecv = NA_integer_,
                            automdl.ljungboxlimit = NA_integer_,
                            automdl.ubfinal= NA_integer_,
                            arima.mu = NA,
                            arima.p = NA_integer_,
                            arima.d = NA_integer_,
                            arima.q = NA_integer_,
                            arima.bp = NA_integer_,
                            arima.bd = NA_integer_,
                            arima.bq = NA_integer_,
                            arima.coefEnabled = NA,
                            arima.coef = NA,
                            arima.coefType = NA,
                            fcst.horizon = NA_integer_)
{
  spec<-match.arg(spec)
  transform.function <- match.arg(transform.function)
  transform.adjust <- match.arg(transform.adjust)
  tradingdays.option <- match.arg(tradingdays.option)
  tradingdays.leapyear <- match.arg(tradingdays.leapyear)
  tradingdays.test <- match.arg(tradingdays.test)
  easter.test <- match.arg(easter.test)
  outlier.method <- match.arg(outlier.method)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # check and define the time span variables for estimate and outlier
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                                 exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                                exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # check the predefined-outliers varaiables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType,outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # check the user-defined variables
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # check the mode of remaining variables
  list.logical.usrdef <-list("usrdef.outliersEnabled","usrdef.varEnabled","arima.coefEnabled")
  list.logical<-list("tradingdays.autoadjust","easter.enabled","easter.julian",
                    "outlier.enabled","outlier.ao","outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","automdl.enabled",
                    "automdl.acceptdefault","automdl.mixed","automdl.balanced","arima.mu")
  list.logical.check <- append(list.logical.usrdef,list.logical)
  list.numeric.span <- list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                           "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast","fcst.horizon")
  list.numeric<-list("estimate.tol","transform.aicdiff","tradingdays.stocktd","easter.duration","outlier.cv",
                    "outlier.tcrate","automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit",
                    "automdl.reducecv","automdl.ljungboxlimit","automdl.ubfinal","arima.p","arima.d",
                    "arima.q","arima.bp","arima.bd","arima.bq")
  list.numeric.check <- append(list.numeric.span,list.numeric)
  list.character<-list("transform.function","transform.adjust","tradingdays.option","tradingdays.leapyear","tradingdays.test",
                      "easter.test","outlier.method")

  var.list<-list()
  for (i in 1:length(list.logical.check)) {
    eval(parse(text=paste("if( !is.logical(",list.logical.check[i],")) {",list.logical.check[i]," = NA; var.list=append(var.list,'",list.logical.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."))}

  var.list<-list()
  for (i in 1:length(list.numeric.check)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric.check[i],")) {",list.numeric.check[i]," = NA; var.list=append(var.list,'",list.numeric.check[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."))}

  variables<-append(list("estimate.span","outlier.span"),list.logical)
  variables<-append(variables,list.numeric)
  variables<-append(variables,list.character)

  # create the java object
  jrspec<-.jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", spec)

  # extract model specification from the java object
  rspec <- specX13_jd2r(spec = jrspec, extra_info = FALSE)

  # Predefined and modified values
  predef.out <- list(Predefined = NA, Final = predef.outliers)
  predef.var <- list(Predefined = list(series = NA, description = NA), Final = predef.variables)
  arima.coeff <- list(Predefined = NA , Final = predef.coef)

  for (i in 1:length(variables)) {
    eval(parse(text=paste(variables[i],".tab=c(rspec$",variables[i],",",variables[i],",","NA)", sep="")))
  }

  v_estimate <-data.frame(span = estimate.span.tab, tolerance = estimate.tol.tab, stringsAsFactors=FALSE)
  v_transform <- data.frame(tfunction=transform.function.tab,adjust=transform.adjust.tab,aicdiff=transform.aicdiff.tab,
                            stringsAsFactors=FALSE)
  v_trading.days<-data.frame( option = tradingdays.option.tab, autoadjust=tradingdays.autoadjust.tab, leapyear = tradingdays.leapyear.tab,
                              stocktd = tradingdays.stocktd.tab, test = tradingdays.test.tab, stringsAsFactors=FALSE)
  v_easter<-data.frame(enabled=easter.enabled.tab,julian=easter.julian.tab,duration=easter.duration.tab,test=easter.test.tab, stringsAsFactors=FALSE)
  v_usrdef <- data.frame(outlier= c(FALSE, usrdef.outliersEnabled,NA),outlier.coef= c(FALSE,NA,NA),
                         variables =c(FALSE, usrdef.varEnabled,NA), variables.coef = c(FALSE,NA,NA),stringsAsFactors=FALSE)
  v_outliers<-data.frame(enabled=outlier.enabled.tab,span=outlier.span.tab,ao=outlier.ao.tab, tc=outlier.tc.tab, ls = outlier.ls.tab,
                         so=outlier.so.tab,usedefcv=outlier.usedefcv.tab,cv=outlier.cv.tab,method=outlier.method.tab,
                         tcrate=outlier.tcrate.tab,stringsAsFactors=FALSE)
  v_arima <-data.frame(enabled=automdl.enabled.tab,automdl.acceptdefault=automdl.acceptdefault.tab,automdl.cancel=automdl.cancel.tab,
                       automdl.ub1=automdl.ub1.tab,automdl.ub2=automdl.ub2.tab,automdl.mixed=automdl.mixed.tab,automdl.balanced=automdl.balanced.tab,
                       automdl.armalimit=automdl.armalimit.tab,automdl.reducecv=automdl.reducecv.tab, automdl.ljungboxlimit=automdl.ljungboxlimit.tab,
                       automdl.ubfinal=automdl.ubfinal.tab,arima.mu=arima.mu.tab,arima.p=arima.p.tab,arima.d =arima.d.tab,arima.q=arima.q.tab,
                       arima.bp=arima.bp.tab,arima.bd=arima.bd.tab,arima.bq=arima.bq.tab,arima.coef = c(FALSE,arima.coefEnabled,NA), stringsAsFactors=FALSE)
  v_forecast <- data.frame(horizon = c(-2,fcst.horizon,NA), stringsAsFactors=FALSE)

  span.spec <-rspec$span

  # Final values
  x <- spec_estimateX13(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformX13(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdX13(td=v_trading.days,tf = transform[3,1], tadj = transform[3,2])
  easter<- spec_easterX13(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersX13(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaX13(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
           transform=transform,
           regression=regression,
           outliers=outliers,
           arima=arima,
           forecast = forecast,
           span=span)
  class(z) = c("regarima_spec","X13")
  return(z)
}
reformat_spec_def <- function(x, parameter){
  data_names <- names(x[[parameter]])
  res <- lapply(data_names,function(name){
    already_formatted <- length(grep("\\.",name)) > 0
    if(already_formatted){
      var_name <- already_formatted
    }else{
      var_name <- paste(parameter, name, sep = ".")
    }
    if(exists(var_name, envir = parent.frame(n = 3))){
      x[[parameter]][[name]]
    }else{
      c(x[[parameter]][[name]], get(var_name, envir = parent.frame(n = 3)), NA)
    }

  })
  names(res) <- data_names
  res
}
# The function creates a ("regarima_spec","X13") class object from from a regarima_spec or regarima object
#' @rdname regarima_spec_def_x13
#' @name regarima_spec_def_x13
#' @param object object of class \code{c("regarima_spec","X13")} or of class \code{c("regarima","X13")}.
#' @export
regarima_spec_x13  <-function(object = object,
                              estimate.from = NA_character_,
                              estimate.to = NA_character_,
                              estimate.first = NA_integer_,
                              estimate.last = NA_integer_,
                              estimate.exclFirst = NA_integer_,
                              estimate.exclLast = NA_integer_,
                              estimate.tol = NA_integer_,
                              transform.function = c(NA, "Auto", "None", "Log"),
                              transform.adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                              transform.aicdiff = NA_integer_,
                              usrdef.outliersEnabled = NA,
                              usrdef.outliersType = NA,
                              usrdef.outliersDate = NA,
                              usrdef.outliersCoef = NA,
                              usrdef.varEnabled = NA,
                              usrdef.var = NA,
                              usrdef.varType = NA,
                              usrdef.varCoef = NA,
                              tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                              tradingdays.autoadjust = NA,
                              tradingdays.leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                              tradingdays.stocktd = NA_integer_,
                              tradingdays.test = c(NA, "Remove", "Add", "None"),
                              easter.enabled = NA,
                              easter.julian = NA,
                              easter.duration = NA_integer_,
                              easter.test = c(NA_character_,"Add","Remove","None"),
                              outlier.enabled = NA,
                              outlier.from = NA_character_,
                              outlier.to = NA_character_,
                              outlier.first = NA_integer_,
                              outlier.last = NA_integer_,
                              outlier.exclFirst = NA_integer_,
                              outlier.exclLast = NA_integer_,
                              outlier.ao = NA,
                              outlier.tc = NA,
                              outlier.ls = NA,
                              outlier.so = NA,
                              outlier.usedefcv = NA,
                              outlier.cv = NA_integer_,
                              outlier.method = c(NA_character_,"AddOne","AddAll"),
                              outlier.tcrate = NA_integer_,
                              automdl.enabled = NA,
                              automdl.acceptdefault = NA,
                              automdl.cancel = NA_integer_,
                              automdl.ub1 = NA_integer_,
                              automdl.ub2 = NA_integer_,
                              automdl.mixed = NA,
                              automdl.balanced = NA,
                              automdl.armalimit = NA_integer_,
                              automdl.reducecv = NA_integer_,
                              automdl.ljungboxlimit = NA_integer_,
                              automdl.ubfinal = NA_integer_,
                              arima.mu = NA,
                              arima.p = NA_integer_,
                              arima.d = NA_integer_,
                              arima.q = NA_integer_,
                              arima.bp = NA_integer_,
                              arima.bd = NA_integer_,
                              arima.bq = NA_integer_,
                              arima.coefEnabled = NA,
                              arima.coef = NA,
                              arima.coefType = NA,
                              fcst.horizon = NA_integer_)
{
  if (!inherits(object, "X13") & (!inherits(object, "regarima") | !inherits(object, "regarima_spec")))
    stop("use only with c(\"regarima\",\"X13\") or c(\"regarima_spec\",\"X13\") objects", call. = FALSE)

  transform.function <-match.arg(transform.function)
  transform.adjust <-match.arg(transform.adjust)
  tradingdays.option <-match.arg(tradingdays.option)
  tradingdays.leapyear <-match.arg(tradingdays.leapyear)
  tradingdays.test <-match.arg(tradingdays.test)
  easter.test <-match.arg(easter.test)
  outlier.method <-match.arg(outlier.method)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # check and define the time span variables for estimate and outlier
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                        exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                        exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # check the predefined-outliers varaiables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType, outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # check the user-defined variables
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # check the mode of remaining variables
  list.logical<-list("usrdef.outliersEnabled","usrdef.varEnabled","tradingdays.autoadjust","easter.enabled","easter.julian",
                    "outlier.enabled","outlier.ao","outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","automdl.enabled",
                    "automdl.acceptdefault","automdl.mixed","automdl.balanced","arima.mu","arima.coefEnabled")

  list.numeric<-list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                      "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast","estimate.tol",
                     "transform.aicdiff","tradingdays.stocktd","easter.duration","outlier.cv",
                    "outlier.tcrate","automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit",
                    "automdl.reducecv","automdl.ljungboxlimit","automdl.ubfinal","arima.p","arima.d",
                    "arima.q","arima.bp","arima.bd","arima.bq","fcst.horizon")

  var.list<-list()
  for (i in 1:length(list.logical)) {
    eval(parse(text=paste("if( !is.logical(",list.logical[i],")) {",list.logical[i]," = NA; var.list=append(var.list,'",list.logical[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be logical. They are ignored."))}

  var.list<-list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text=paste("if( !is.numeric(",list.numeric[i],")) {",list.numeric[i]," = NA; var.list=append(var.list,'",list.numeric[i],"')}",sep="")))
  }
  if (length(var.list)>0) {warning(paste("Variable(s)",deparse(as.character(var.list))," should be numeric. They are ignored."))}

  # Predefined values
  estimate.spec <- s_estimate(object)
  transform.spec <- s_transform(object)
  usrdef.spec <- s_usrdef(object)
  trading.days.spec <- s_td(object)
  easter.spec <- s_easter(object)
  outliers.spec <- s_out(object)
  arima.spec <- s_arima(object)
  forecast.spec <- s_fcst(object)
  span.spec <- s_span(object)

  predef.outliers.spec <- s_preOut(object)
  predef.variables.spec <- s_preVar(object)
  predef.coef.spec <- s_arimaCoef(object)

  # Modified values
  predef.out <- list(Predefined = predef.outliers.spec, Final = predef.outliers)
  predef.var <- list(Predefined = predef.variables.spec, Final = predef.variables)
  arima.coeff <- list(Predefined = predef.coef.spec , Final = predef.coef)

  estimate.mod <- data.frame(span = estimate.span, tolerance = estimate.tol, stringsAsFactors=FALSE)
  transform.mod <- data.frame(tfunction=transform.function,adjust=transform.adjust,aicdiff=transform.aicdiff, stringsAsFactors=FALSE)
  usrdef.mod <- data.frame(outlier=usrdef.outliersEnabled, outlier.coef= NA, variables = usrdef.varEnabled,
                              variables.coef = NA, stringsAsFactors=FALSE)
  trading.days.mod <- data.frame( option = tradingdays.option, autoadjust=tradingdays.autoadjust, leapyear = tradingdays.leapyear,
                           stocktd = tradingdays.stocktd, test = tradingdays.test, stringsAsFactors=FALSE)
  easter.mod <- data.frame(enabled=easter.enabled,julian=easter.julian,duration=easter.duration,test=easter.test, stringsAsFactors=FALSE)
  outliers.mod <- data.frame(enabled=outlier.enabled,span=outlier.span,ao=outlier.ao, tc=outlier.tc, ls = outlier.ls,
                      so=outlier.so,usedefcv=outlier.usedefcv,cv=outlier.cv,method=outlier.method,
                      tcrate=outlier.tcrate, stringsAsFactors=FALSE)
  arima.mod <- data.frame(enabled=automdl.enabled,automdl.acceptdefault=automdl.acceptdefault,automdl.cancel=automdl.cancel,
                   automdl.ub1=automdl.ub1,automdl.ub2=automdl.ub2,automdl.mixed=automdl.mixed,automdl.balanced=automdl.balanced,
                   automdl.armalimit=automdl.armalimit,automdl.reducecv=automdl.reducecv, automdl.ljungboxlimit=automdl.ljungboxlimit,
                   automdl.ubfinal=automdl.ubfinal,arima.mu=arima.mu,arima.p=arima.p,arima.d =arima.d,arima.q=arima.q,
                   arima.bp=arima.bp,arima.bd=arima.bd,arima.bq=arima.bq,arima.coef = arima.coefEnabled,
                   stringsAsFactors=FALSE)
  forecast.mod <-data.frame(horizon=fcst.horizon)

  v_estimate <- rbind(estimate.spec,estimate.mod,rep(NA,length(estimate.spec)))
  v_transform <- rbind(transform.spec,transform.mod,rep(NA,length(transform.spec)))
  v_usrdef <- rbind(usrdef.spec,usrdef.mod,rep(NA,length(usrdef.spec)))
  v_trading.days <- rbind(trading.days.spec,trading.days.mod,rep(NA,length(trading.days.spec)))
  v_easter <- rbind(easter.spec,easter.mod,rep(NA,length(easter.spec)))
  v_outliers <- rbind(outliers.spec,outliers.mod,rep(NA,length(outliers.spec)))
  v_arima <- rbind(arima.spec,arima.mod,rep(NA,length(arima.spec)))
  v_forecast <-rbind(forecast.spec,forecast.mod,NA)

  # Final values
  x <- spec_estimateX13(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformX13(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdX13(td=v_trading.days,tf = transform[3,1], tadj = transform[3,2])
  easter<- spec_easterX13(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersX13(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaX13(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
            transform=transform,
            regression=regression,
            outliers=outliers,
            arima=arima,
            forecast = forecast,
            span=span)
  class(z) = c("regarima_spec","X13")
  return(z)

}
