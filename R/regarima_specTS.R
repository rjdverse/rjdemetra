#' RegARIMA model specification, pre-adjustment in TRAMO-SEATS
#' @description
#'
#' Function to create (and/or modify) a \code{c("regarima_spec","TRAMO_SEATS")} class object with the RegARIMA model specification
#' for the TRAMO-SEATS method. The object can be created from the name (\code{character}) of a predefined 'JDemetra+' model specification,
#' a previous specification (\code{c("regarima_spec","TRAMO_SEATS")} object) or a TRAMO-SEATS RegARIMA model (\code{c("regarima","TRAMO_SEATS")}).
#'
#' @param spec the model specification. It can be the name (\code{character}) of a predefined 'JDemetra+' model specification
#' (see \emph{Details}), an object of class \code{c("regarima_spec","TRAMO_SEATS")} or an object of class \code{c("regarima", "TRAMO_SEATS")}.
#' The default is \code{"TRfull"}.
#'
#' @param preliminary.check a \code{logical} to check the quality of the input series and exclude highly problematic series
#' e.g. the series with a number of identical observations and/or missing values above pre-specified threshold values.
#'
#' The time span of the series, which is the (sub)period used to estimate the regarima model, is controlled by the following six variables:
#' \code{estimate.from, estimate.to, estimate.first, estimate.last, estimate.exclFirst} and \code{estimate.exclLast};
#' where \code{estimate.from} and \code{estimate.to} have priority over the remaining span control variables,
#' \code{estimate.last} and \code{estimate.first} have priority over \code{estimate.exclFirst} and \code{estimate.exclLast},
#' and \code{estimate.last} has priority over \code{estimate.first}. Default= "All".
#'
#' @param estimate.from a character in format "YYYY-MM-DD" indicating the start of the time span (e.g. "1900-01-01").
#' It can be combined with the parameter \code{estimate.to}.
#'
#' @param estimate.to a \code{character} in format "YYYY-MM-DD" indicating the end of the time span (e.g. "2020-12-31").
#' It can be combined with the parameter \code{estimate.from}.
#'
#' @param estimate.first \code{numeric}, the number of periods considered at the beginning of the series.
#'
#' @param estimate.last \code{numeric}, the number of periods considered at the end of the series.
#'
#' @param estimate.exclFirst \code{numeric}, the number of periods excluded at the beginning of the series.
#' It can be combined with the parameter \code{estimate.exclLast}.
#'
#' @param estimate.exclLast \code{numeric}, the number of periods excluded at the end of the series.
#' It can be combined with the parameter \code{estimate.exclFirst}.
#'
#' @param estimate.tol \code{numeric}, the convergence tolerance. The absolute changes in the log-likelihood function are compared to this value
#' to check for the convergence of the estimation iterations.
#'
#' @param estimate.eml \code{logical}, the exact maximum likelihood estimation. If \code{TRUE}, the program performs an exact maximum likelihood estimation.
#' If \code{FASLE}, the Unconditional Least Squares method is used.
#'
#' @param estimate.urfinal \code{numeric}, the final unit root limit. The threshold value for the final unit root test
#' for identification of differencing orders. If the magnitude of an AR root for the final model is smaller than this number,
#'  then a unit root is assumed, the order of the AR polynomial is reduced by one and the appropriate order of the differencing
#'  (non-seasonal, seasonal) is increased.
#'
#' @param transform.function the transformation of the input series: \code{"None"} = no transformation of the series;
#' \code{"Log"} = takes the log of the series; \code{"Auto"} = the program tests for the log-level specification.
#'
#' @param transform.fct \code{numeric} controlling the bias in the log/level pre-test:
#' \code{ transform.fct }> 1 favours levels, \code{transform.fct}< 1 favours logs.
#' Considered only when \code{transform.function} is set to \code{"Auto"}.
#'
#' Control variables for the pre-specified outliers. Said pre-specified outliers are used in the model only when  enabled
#' (\code{usrdef.outliersEnabled=TRUE}) and when the outliers' type (\code{usrdef.outliersType}) and date (\code{usrdef.outliersDate})
#' are provided.
#'
#' @param usrdef.outliersEnabled \code{logical}. If \code{TRUE}, the program uses the pre-specified outliers.
#'
#' @param usrdef.outliersType a vector defining the outliers' type. Possible types are: \code{("AO"}) = additive,
#' \code{("LS"}) = level shift, \code{("TC"}) = transitory change, \code{("SO"}) = seasonal outlier.
#' E.g.: \code{ usrdef.outliersType= c("AO","AO","LS")}.
#'
#' @param usrdef.outliersDate a vector defining the outliers' date. The dates should be characters in format "YYYY-MM-DD".
#' E.g.: \code{usrdef.outliersDate= c("2009-10-01","2005-02-01","2003-04-01")}.
#'
#' @param usrdef.outliersCoef a vector providing fixed coefficients for the outliers. The coefficients can't be fixed if
#' the parameter \code{ transform.function} is set to \code{"Auto"} (i.e. if the series transformation needs to be pre-defined.)
#' E.g.: \code{usrdef.outliersCoef= c(200,170,20)}.
#'
#' Control variables for the user-defined variables:
#'
#' @param usrdef.varEnabled \code{logical} If \code{TRUE}, the program uses the user-defined variables.
#'
#' @param usrdef.var a time series (\code{ts}) or a matrix of time series (\code{mts}) containing the user-defined variables.
#'
#' @param usrdef.varType a vector of character(s) defining the user-defined variables component type.
#' Possible types are: \code{"Undefined", "Series", "Trend", "Seasonal", "SeasonallyAdjusted", "Irregular", "Calendar"}.
#' To use the user-defined calendar regressors, the type \code{"Calendar"} must be defined in conjunction with \code{tradingdays.option = "UserDefined"}.
#' Otherwise, the program will automatically set \code{usrdef.varType = "Undefined"}.
#'
#' @param usrdef.varCoef a vector providing fixed coefficients for the user-defined variables. The coefficients can't be fixed if
#'  \code{ transform.function} is set to \code{"Auto"} (i.e. if the series transformation needs to be pre-defined).
#'
#' @param tradingdays.mauto defines whether the calendar effects should be added to the model manually (\code{"Unused"}) or automatically.
#' During the automatic selection, the choice of the number of calendar variables can be based on the F-Test (\code{"FTest"}) or the Wald Test (\code{"WaldTest"});
#' the model with higher F value is chosen, provided that it is higher than \code{tradingdays.pftd}).
#'
#' @param tradingdays.pftd \code{numeric}. The p-value used in the test specified by the automatic parameter (\code{tradingdays.mauto})
#' to assess the significance of the pre-tested calendar effects variables and whether they should be included in the RegArima model.
#'
#' Control variables for the manual selection of calendar effects variables (\code{tradingdays.mauto} is set to \code{"Unused"}):
#'
#' @param tradingdays.option to choose the trading days regression variables: \code{"TradingDays"} =  six day-of-the-week regression variables;
#' \code{"WorkingDays"} = one working/non-working day contrast variable; \code{"None"} = no correction for trading days and working days effects;
#' \code{"UserDefined"} = user-defined trading days regressors (regressors must be defined by the \code{usrdef.var} argument
#' with \code{usrdef.varType} set to \code{"Calendar"} and \code{usrdef.varEnabled = TRUE}).
#' \code{"None"} must also be chosen for the "day-of-week effects" correction (and \code{tradingdays.stocktd} must be modified accordingly).
#'
#' @param tradingdays.leapyear \code{logical}. Specifies if the leap-year correction should be included.
#' If \code{TRUE}, the model includes the leap-year effect.
#'
#' @param tradingdays.stocktd numeric indicating the day of the month when inventories and other stock are reported (to denote the last day of the month set the variable to 31). Modifications of this variable are taken into account only when \code{tradingdays.option} is set to \code{"None"}.
#'
#' @param tradingdays.test defines the pre-tests of the trading day effects: \code{"None"} = calendar variables are used in the model without pre-testing;
#' \code{"Separate_T"} = a t-test is applied to each trading day variable separately and the trading day variables are included in the RegArima model
#' if at least one t-statistic is greater than 2.6 or if two t-statistics are greater than 2.0 (in absolute terms);
#' \code{"Joint_F"} = a joint F-test of significance of all the trading day variables. The trading day effect is significant if the F statistic is greater than 0.95.
#'
#' @param easter.type a\code{character} that specifies the presence and the length of the Easter effect:
#' \code{"Unused"} = the Easter effect is not considered; \code{"Standard"} = influences the period of \code{n} days strictly before Easter Sunday;
#' \code{"IncludeEaster"} = influences the entire period (\code{n}) up to and including Easter Sunday;
#' \code{"IncludeEasterMonday"} = influences the entire period (\code{n}) up to and including Easter Monday.
#'
#' @param easter.julian \code{logical}. If \code{TRUE}, the program uses the Julian Easter (expressed in Gregorian calendar).
#'
#' @param easter.duration \code{numeric} indicating the duration of the Easter effect (length in days, between 1 and 15).
#'
#' @param easter.test \code{logical}. If \code{TRUE}, the program performs a t-test for the significance of the Easter effect.
#' The Easter effect is considered as significant if the modulus of t-statistic is greater than 1.96.
#'
#' @param outlier.enabled \code{logical}. If \code{TRUE}, the automatic detection of outliers is enabled in the defined time span.
#'
#' The time span of the series to be searched for outliers is controlled by the following six variables:
#' \code{outlier.from, outlier.to, outlier.first, outlier.last, outlier.exclFirst} and \code{outlier.exclLast};
#' where \code{outlier.from} and \code{outlier.to} have priority over the remaining span control variables,
#' \code{outlier.last} and \code{outlier.first} have priority over \code{outlier.exclFirst} and \code{outlier.exclLast},
#' and \code{outlier.last} has priority over \code{outlier.first}.
#'
#' @param outlier.from a character in format "YYYY-MM-DD" indicating the start of the time span (e.g. "1900-01-01").
#' It can be combined with \code{outlier.to}.
#'
#' @param outlier.to a character in format "YYYY-MM-DD" indicating the end of the time span (e.g. "2020-12-31").
#' It can be combined with \code{outlier.from}.
#'
#' @param outlier.first \code{numeric} specifying the number of periods considered at the beginning of the series.
#'
#' @param outlier.last \code{numeric} specifying the number of periods considered at the end of the series.
#'
#' @param outlier.exclFirst \code{numeric} specifying the number of periods excluded at the beginning of the series. It can be combined with \code{outlier.exclLast}.
#'
#' @param outlier.exclLast \code{numeric} specifying the number of periods excluded at the end of the series. It can be combined with \code{outlier.exclFirst}.
#'
#' @param outlier.ao \code{logical}. If \code{TRUE}, the automatic detection of additive outliers is enabled (\code{outlier.enabled} must also be set to \code{TRUE}).
#'
#' @param outlier.tc \code{logical}. If \code{TRUE}, the automatic detection of transitory changes is enabled (\code{outlier.enabled} must also be set to \code{TRUE}).
#'
#' @param outlier.ls \code{logical}. If \code{TRUE}, the automatic detection of level shifts is enabled (\code{outlier.enabled} must also be set to \code{TRUE}).
#'
#' @param outlier.so \code{logical}. If \code{TRUE}, the automatic detection of seasonal outliers is enabled (\code{outlier.enabled} must also be set to \code{TRUE}).
#'
#' @param outlier.usedefcv \code{logical}. If \code{TRUE}, the critical value for the outliers' detection procedure is automatically determined
#' by the number of observations in the outlier detection time span. If \code{FALSE}, the procedure uses the entered critical value (\code{outlier.cv}).
#'
#' @param outlier.cv \code{numeric}. The entered critical value for the outliers' detection procedure. The modification of this variable
#' is only taken in to account when \code{outlier.usedefcv} is set to \code{FALSE}.
#'
#' @param outlier.eml \code{logical} for the exact likelihood estimation method. It controls the method applied for a parameter estimation
#' in the intermediate steps of the automatic detection and correction of outliers. If \code{TRUE}, an exact likelihood estimation method is used.
#' When \code{FALSE}, the fast Hannan-Rissanen method is used.
#'
#' @param outlier.tcrate \code{numeric}. The rate of decay for the transitory change outlier.
#'
#' @param automdl.enabled \code{logical}. If \code{TRUE}, the automatic modelling of the ARIMA model is enabled.
#' If \code{FALSE}, the parameters of the ARIMA model can be specified.
#'
#' Control variables for the automatic modelling of the ARIMA model (\code{automdl.enabled} is set to \code{TRUE}):
#'
#' @param automdl.acceptdefault \code{logical}. If \code{TRUE}, the default model (ARIMA(0,1,1)(0,1,1)) may be chosen in the first step
#' of the automatic model identification. If the Ljung-Box Q statistics for the residuals is acceptable, the default model is accepted
#' and no further attempt will be made to identify another model.
#'
#' @param automdl.cancel \code{numeric}, the cancellation limit. If the difference in moduli of an AR and an MA roots (when estimating ARIMA(1,0,1)(1,0,1) models
#' in the second step of the automatic identification of the differencing orders) is smaller than the cancellation limit, the two roots are assumed equal and canceled out.
#'
#' @param automdl.ub1 \code{numeric}, the first unit root limit. It is the threshold value for the initial unit root test in the automatic differencing procedure.
#' When one of the roots in the estimation of the ARIMA(2,0,0)(1,0,0) plus mean model, performed in the first step of the automatic model identification procedure,
#' is larger than first unit root limit in modulus, it is set equal to unity.
#'
#' @param automdl.ub2 \code{numeric}, the second unit root limit. When one of the roots in the estimation of the ARIMA(1,0,1)(1,0,1) plus mean model,
#' which is performed in the second step of the automatic model identification procedure, is larger than second unit root limit in modulus,
#' it is checked if there is a common factor in the corresponding AR and MA polynomials of the ARMA model that can be canceled (see \code{automdl.cancel}).
#' If there is no cancellation, the AR root is set equal to unity (i.e. the differencing order changes).
#'
#' @param automdl.armalimit \code{numeric}, the arma limit. It is the threshold value for t-statistics of ARMA coefficients and the constant term used
#' for the final test of model parsimony. If the highest order ARMA coefficient has a t-value smaller than this value in magnitude, the order of the model is reduced.
#' If the constant term has a t-value smaller than the ARMA limit in magnitude, it is removed from the set of regressors.
#'
#' @param automdl.reducecv \code{numeric}, ReduceCV. The percentage by which the outlier critical value will be reduced
#' when an identified model is found to have a Ljung-Box statistic with an unacceptable confidence coefficient.
#' The parameter should be between 0 and 1, and will only be active when automatic outlier identification is enabled.
#' The reduced critical value will be set to (1-ReduceCV)xCV, where CV is the original critical value.
#'
#' @param automdl.ljungboxlimit \code{numeric}, the Ljung Box limit, setting the acceptance criterion for the confidence intervals of the Ljung-Box Q statistic.
#' If the LjungBox Q statistics for the residuals of a final model is greater than Ljung Box limit, then the model is rejected, the outlier critical value is reduced,
#' and model and outlier identification (if specified) is redone with a reduced value.
#'
#' @param automdl.compare \code{logical}. If \code{TRUE}, the program compares the model identified by the automatic procedure to the default model (ARIMA(0,1,1)(0,1,1))
#' and the model with the best fit is selected. Criteria considered are residual diagnostics, the model structure and the number of outliers.
#'
#' Control variables for the non-automatic modelling of the ARIMA model (\code{automdl.enabled} is set to \code{FALSE}):
#'
#' @param arima.mu \code{logical}. If \code{TRUE}, the mean is considered as part of the ARIMA model.
#'
#' @param arima.p \code{numeric}. The order of the non-seasonal autoregressive (AR) polynomial.
#'
#' @param arima.d \code{numeric}. The regular differencing order.
#'
#' @param arima.q \code{numeric}. The order of the non-seasonal moving average (MA) polynomial.
#'
#' @param arima.bp \code{numeric}. The order of the seasonal autoregressive (AR) polynomial.
#'
#' @param arima.bd \code{numeric}. The seasonal differencing order.
#'
#' @param arima.bq \code{numeric}. The order of the seasonal moving average (MA) polynomial.
#'
#' Control variables for the user-defined ARMA coefficients. Such coefficients can be defined for the regular and seasonal autoregressive (AR) polynomials
#' and moving average (MA) polynomials. The model considers the coefficients only if the procedure for their estimation (\code{arima.coefType}) is provided,
#' and the number of provided coefficients matches the sum of (regular and seasonal) AR and MA orders (\code{p,q,bp,bq}).
#'
#' @param arima.coefEnabled \code{logical}. If \code{TRUE}, the program uses the user-defined ARMA coefficients.
#'
#' @param arima.coef a vector providing the coefficients for the regular and seasonal AR and MA polynomials.
#' The length of the vector must be equal to the sum of the regular and seasonal AR and MA orders. The coefficients shall be provided in the following order:
#' regular AR (\emph{Phi} - \code{p} elements), regular MA  (\emph{Theta} - \code{q} elements), seasonal AR (\emph{BPhi} - \code{bp} elements)
#' and seasonal MA (\emph{BTheta} - \code{bq} elements).
#' E.g.: \code{arima.coef=c(0.6,0.7)} with \code{arima.p=1, arima.q=0,arima.bp=1} and \code{arima.bq=0}.
#'
#' @param arima.coefType avector defining the ARMA coefficients estimation procedure. Possible procedures are:
#' \code{"Undefined"} = no use of user-defined input (i.e. coefficients are estimated),
#' \code{"Fixed"} = fixes the coefficients at the value provided by the user,
#' \code{"Initial"} = the value defined by the user is used as initial condition.
#' For orders for which the coefficients shall not be defined, the \code{arima.coef} can be set to \code{NA} or \code{0}
#' or the \code{arima.coefType} can be set to \code{"Undefined"}.
#' E.g.: \code{arima.coef = c(-0.8,-0.6,NA)}, \code{arima.coefType = c("Fixed","Fixed","Undefined")}.
#'
#' @param fcst.horizon \code{numeric}, the forecasting horizon. The length of the forecasts generated by the RegARIMA model
#' in periods (positive values) or years (negative values). By default, the program generates two years forecasts (\code{fcst.horizon} set to \code{-2}).
#'
#'
#'
#' @details
#' The available predefined 'JDemetra+' model specifications are described in the table below:
#'
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
#' A list of class \code{c("regarima_spec","TRAMO_SEATS")} with the following components, each referring to a different part
#' of the RegARIMA model specification, mirroring the arguments of the function (for details, see the arguments description).
#' Each lowest-level component (except the span, pre-specified outliers, user-defined variables and pre-specified ARMA coefficients)
#' is structured within a data frame with columns denoting different variables of the model specification and rows referring to:
#' first row = the base specification, as provided within the argument \code{spec};
#' second row = user modifications as specified by the remaining arguments of the function (e.g.: \code{arima.d});
#' and third row = the final model specification, values that will be used in the function \code{\link{regarima}}.
#' The final specification (third row) shall include user modifications (row two) unless they were wrongly specified.
#' The pre-specified outliers, user-defined variables and pre-specified ARMA coefficients consist of a list
#' with the \code{Predefined} (base model specification) and \code{Final} values.
#'
#' \item{estimate}{a data frame containing Variables referring to: \code{span} - time span to be used for the estimation,
#' \code{tolerance} - argument \code{estimate.tol}, \code{exact_ml} - argument \code{estimate.eml}, \code{urfinal} - argument \code{esimate.urfinal}.
#' The final values can be also accessed with the function \code{\link{s_estimate}}.}
#'
#' \item{transform}{a data frame containing variables referring to: \code{tfunction} - argument \code{transform.function},
#' \code{fct} - argument \code{transform.fct}. The final values can be also accessed with the function \code{\link{s_transform}}.}
#'
#' \item{regression}{a list containing information on the user-defined variables (\code{userdef}), \code{trading.days} effect and \code{easter} effect.
#' The user-defined part includes: \code{specification} - data frame with the information if pre-specified outliers (\code{outlier})
#' and user-defined variables (\code{variables}) are included in the model and if fixed coefficients are used (\code{outlier.coef}
#' and \code{variables.coef}).
#' The final values can be also accessed with the function \code{\link{s_usrdef}}; \code{outliers} - matrixes with the outliers
#' (\code{Predefined} and \code{Final}). The final outliers can be also accessed with the function \code{\link{s_preOut}};
#' and \code{variables} - list with the \code{Predefined} and \code{Final} user-defined variables (\code{series}) and its description
#' (\code{description}) including information on the variable type and values of fixed coefficients.
#' The final user-defined variables can be also accessed with the function \code{\link{s_preVar}}.
#'
#' The \code{trading.days} data frame variables refer to:
#' \code{automatic} - argument \code{tradingdays.mauto},
#' \code{pftd} - argument \code{tradingdays.pftd},
#' \code{option} - argument \code{tradingdays.option},
#' \code{leapyear} - argument \code{tradingdays.leapyear},
#' \code{stocktd} - argument \code{tradingdays.stocktd},
#' \code{test} - argument \code{tradingdays.test}. The final \code{trading.days} values can be also accessed with the function \code{\link{s_td}}.
#' The \code{easter} data frame variables refer to:
#' \code{type} - argument \code{easter.type},
#' \code{julian} - argument \code{easter.julian},
#' \code{duration} - argument \code{easter.duration},
#' \code{test} - argument \code{easter.test}. The final \code{easter} values can be also accessed with the function \code{\link{s_easter}}.}
#'
#' \item{outliers}{a data frame. Variables referring to:
#' \code{ao} - argument \code{outlier.ao},
#' \code{tc} - argument \code{outlier.tc},
#' \code{ls} - argument \code{outlier.ls},
#' \code{so} - argument \code{outlier.so},
#' \code{usedefcv} - argument \code{outlier.usedefcv},
#' \code{cv} - argument \code{outlier.cv},
#' \code{eml} - argument \code{outlier.eml},
#' \code{tcrate} - argument \code{outlier.tcrate}. The final values can be also accessed with the function \code{\link{s_out}}.}
#'
#' \item{arima}{a list containing a data frame with the ARIMA settings (\code{specification}) and matrices giving information
#' on the pre-specified ARMA coefficients (\code{coefficients}). The matrix \code{Predefined} refers to the pre-defined model specification
#' and matrix \code{Final}, to the final specification. Both matrices contain the values of the ARMA coefficients and the procedure
#' for its estimation.
#' In the data frame \code{specification}, the variable \code{enabled} refers to the argument \code{automdl.enabled}
#' and all remaining variables (\code{automdl.acceptdefault, automdl.cancel, automdl.ub1, automdl.ub2, automdl.armalimit,
#' automdl.reducecv, automdl.ljungboxlimit, automdl.compare, arima.mu, arima.p, arima.d, arima.q, arima.bp, arima.bd,
#' arima.bq}), to the respective function arguments.
#' The final values of the \code{specification} can be also accessed with the function \code{\link{s_arima}},
#' and final pre-specified ARMA coefficients with the function \code{\link{s_arimaCoef}}.}
#'
#' \item{forecast}{a data frame with the forecasting horizon (argument \code{fcst.horizon}).
#' The final value can be also accessed with the function \code{\link{s_fcst}}.}
#'
#' \item{span}{a matrix containing the final time span for the model estimation and outliers' detection.
#' It contains the same information as the variable span in the data frames estimate and outliers.
#' The matrix can be also accessed with the function \code{\link{s_span}}.}
#'
#' @references
#' More information and examples related to 'JDemetra+' features in the online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#' @examples\donttest{
#' myseries <- ipi_c_eu[, "FR"]
#' myspec1 <- regarima_spec_tramoseats(spec = "TRfull")
#' myreg1 <- regarima(myseries, spec = myspec1)
#'
#'  # To modify a pre-specified model specification
#' myspec2 <- regarima_spec_tramoseats(spec = "TRfull",
#'              tradingdays.mauto = "Unused",
#'              tradingdays.option = "WorkingDays",
#'              easter.type = "Standard",
#'              automdl.enabled = FALSE, arima.mu = TRUE)
#' myreg2 <- regarima(myseries, spec = myspec2)
#'
#'  # To modify the model specification of a "regarima" object
#' myspec3 <- regarima_spec_tramoseats(myreg1,
#'              tradingdays.mauto = "Unused",
#'              tradingdays.option = "WorkingDays",
#'              easter.type = "Standard", automdl.enabled = FALSE,
#'              arima.mu = TRUE)
#' myreg3 <- regarima(myseries, myspec3)
#'
#'  # To modify the model specification of a "regarima_spec" object
#' myspec4 <- regarima_spec_tramoseats(myspec1,
#'              tradingdays.mauto = "Unused",
#'              tradingdays.option = "WorkingDays",
#'              easter.type = "Standard",
#'              automdl.enabled = FALSE, arima.mu = TRUE)
#' myreg4 <- regarima(myseries, myspec4)
#'
#'  # Pre-specified outliers
#' myspec1 <- regarima_spec_tramoseats(spec = "TRfull",
#'              usrdef.outliersEnabled = TRUE,
#'              usrdef.outliersType = c("LS", "LS"),
#'              usrdef.outliersDate = c("2008-10-01" ,"2003-01-01"),
#'              usrdef.outliersCoef = c(10, -8), transform.function = "None")
#' s_preOut(myspec1)
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
#' myspec1 <- regarima_spec_tramoseats(spec = "TRfull",
#'             usrdef.varEnabled = TRUE, usrdef.var = var)
#' s_preVar(myspec1)
#' myreg1 <- regarima(myseries,myspec1)
#'
#' myspec2 <- regarima_spec_tramoseats(spec = "TRfull",
#'              usrdef.varEnabled = TRUE,
#'              usrdef.var = var, usrdef.varCoef = c(17,-1),
#'              transform.function = "None")
#' myreg2 <- regarima(myseries, myspec2)
#'
#'  # Pre-specified ARMA coefficients
#' myspec1 <- regarima_spec_tramoseats(spec = "TRfull",
#'              arima.coefEnabled = TRUE, automdl.enabled = FALSE,
#'              arima.p = 2, arima.q = 0, arima.bp = 1, arima.bq = 1,
#'              arima.coef = c(-0.12, -0.12, -0.3, -0.99),
#'              arima.coefType = rep("Fixed", 4))
#' myreg1 <- regarima(myseries, myspec1)
#' myreg1
#' summary(myreg1)
#' s_arimaCoef(myspec1)
#' s_arimaCoef(myreg1)
#' }
#' @export
regarima_spec_tramoseats <- function(spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5"),
                                     preliminary.check = NA,
                                         estimate.from = NA_character_,
                                         estimate.to = NA_character_,
                                         estimate.first = NA_integer_,
                                         estimate.last = NA_integer_,
                                         estimate.exclFirst = NA_integer_,
                                         estimate.exclLast = NA_integer_,
                                         estimate.tol = NA_integer_,
                                         estimate.eml = NA,
                                         estimate.urfinal = NA_integer_,
                                         transform.function = c(NA, "Auto", "None", "Log"),
                                         transform.fct = NA_integer_,
                                         usrdef.outliersEnabled = NA,
                                         usrdef.outliersType = NA,
                                         usrdef.outliersDate = NA,
                                         usrdef.outliersCoef = NA,
                                         usrdef.varEnabled = NA,
                                         usrdef.var = NA,
                                         usrdef.varType = NA,
                                         usrdef.varCoef = NA,
                                         tradingdays.mauto = c(NA, "Unused", "FTest" ,"WaldTest"),
                                         tradingdays.pftd = NA_integer_,
                                         tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                                         tradingdays.leapyear = NA,
                                         tradingdays.stocktd = NA_integer_,
                                         tradingdays.test = c(NA, "Separate_T", "Joint_F", "None"),
                                         easter.type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
                                         easter.julian = NA,
                                         easter.duration = NA_integer_,
                                         easter.test = NA,
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
                                         outlier.eml = NA,
                                         outlier.tcrate = NA_integer_,
                                         automdl.enabled = NA,
                                         automdl.acceptdefault = NA,
                                         automdl.cancel = NA_integer_,
                                         automdl.ub1 = NA_integer_,
                                         automdl.ub2 = NA_integer_,
                                         automdl.armalimit = NA_integer_,
                                         automdl.reducecv = NA_integer_,
                                         automdl.ljungboxlimit = NA_integer_,
                                         automdl.compare = NA,
                                         arima.mu = NA,
                                         arima.p = NA_integer_,
                                         arima.d = NA_integer_,
                                         arima.q = NA_integer_,
                                         arima.bp = NA_integer_,
                                         arima.bd = NA_integer_,
                                         arima.bq = NA_integer_,
                                         arima.coefEnabled = NA,
                                         arima.coef= NA,
                                         arima.coefType = NA,
                                         fcst.horizon = NA_integer_
                                     )
{
  UseMethod("regarima_spec_tramoseats", spec)
}
#' @export
regarima_spec_tramoseats.character <- function(spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5"),
                                               preliminary.check = NA,
                                     estimate.from = NA_character_,
                                     estimate.to = NA_character_,
                                     estimate.first = NA_integer_,
                                     estimate.last = NA_integer_,
                                     estimate.exclFirst = NA_integer_,
                                     estimate.exclLast = NA_integer_,
                                     estimate.tol = NA_integer_,
                                     estimate.eml = NA,
                                     estimate.urfinal = NA_integer_,
                                     transform.function = c(NA, "Auto", "None", "Log"),
                                     transform.fct = NA_integer_,
                                     usrdef.outliersEnabled = NA,
                                     usrdef.outliersType = NA,
                                     usrdef.outliersDate = NA,
                                     usrdef.outliersCoef = NA,
                                     usrdef.varEnabled = NA,
                                     usrdef.var = NA,
                                     usrdef.varType = NA,
                                     usrdef.varCoef = NA,
                                     tradingdays.mauto = c(NA, "Unused", "FTest" ,"WaldTest"),
                                     tradingdays.pftd = NA_integer_,
                                     tradingdays.option = c(NA, "TradingDays", "WorkingDays", "UserDefined", "None"),
                                     tradingdays.leapyear = NA,
                                     tradingdays.stocktd = NA_integer_,
                                     tradingdays.test = c(NA, "Separate_T", "Joint_F", "None"),
                                     easter.type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"),
                                     easter.julian = NA,
                                     easter.duration = NA_integer_,
                                     easter.test = NA,
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
                                     outlier.eml = NA,
                                     outlier.tcrate = NA_integer_,
                                     automdl.enabled = NA,
                                     automdl.acceptdefault = NA,
                                     automdl.cancel = NA_integer_,
                                     automdl.ub1 = NA_integer_,
                                     automdl.ub2 = NA_integer_,
                                     automdl.armalimit = NA_integer_,
                                     automdl.reducecv = NA_integer_,
                                     automdl.ljungboxlimit = NA_integer_,
                                     automdl.compare = NA,
                                     arima.mu = NA,
                                     arima.p = NA_integer_,
                                     arima.d = NA_integer_,
                                     arima.q = NA_integer_,
                                     arima.bp = NA_integer_,
                                     arima.bd = NA_integer_,
                                     arima.bq = NA_integer_,
                                     arima.coefEnabled = NA,
                                     arima.coef= NA,
                                     arima.coefType = NA,
                                     fcst.horizon = NA_integer_){
  spec <- match.arg(spec)
  transform.function <- match.arg(transform.function)
  tradingdays.mauto <- match.arg(tradingdays.mauto)
  tradingdays.option <- match.arg(tradingdays.option)
  tradingdays.test <- match.arg(tradingdays.test)
  easter.type <- match.arg(easter.type)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # To check & define the time span variables for estimates and outliers
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                        exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                        exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # To  check the predefined outliers variables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType,
                                 outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # To  check the user-defined variables
  n.usrvar <- if (is.mts(usrdef.var)) {dim(usrdef.var)[2]} else if (is.ts(usrdef.var)) {1} else {0}
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # To check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)

  # To check the mode of the remaining variables
  list.logical.usrdef <-list("usrdef.outliersEnabled","usrdef.varEnabled","arima.coefEnabled")
  list.logical = list("preliminary.check", "estimate.eml","tradingdays.leapyear","easter.julian","easter.test","outlier.enabled","outlier.ao",
                      "outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","outlier.eml","automdl.enabled",
                      "automdl.acceptdefault","automdl.compare","arima.mu")
  list.logical.check <- append(list.logical.usrdef,list.logical)

  list.numeric.span <- list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                            "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast","fcst.horizon")

  list.numeric = list("estimate.tol","estimate.urfinal","transform.fct","tradingdays.pftd",
                      "tradingdays.stocktd","easter.duration","outlier.cv","outlier.tcrate",
                      "automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit","automdl.reducecv",
                      "automdl.ljungboxlimit","arima.p","arima.d","arima.q","arima.bp","arima.bd","arima.bq")

  list.numeric.check <- append(list.numeric.span,list.numeric)

  list.character <- list("transform.function","tradingdays.mauto","tradingdays.option","tradingdays.test","easter.type")

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

  # To create the Java object
  jrspec<-.jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)

  # To extract the model specification from the Java object
  rspec <- spec_TRAMO_jd2r( spec = jrspec)

  # Predefined and modified values
  predef.out <- list(Predefined = NA, Final = predef.outliers)
  predef.var <- list(Predefined = list(series = NA, description = NA), Final = predef.variables)
  arima.coeff <- list(Predefined = NA , Final = predef.coef)

  for (i in 1:length(variables)) {
    eval(parse(text=paste(variables[i],".tab=c(rspec$",variables[i],",",variables[i],",","NA)", sep="")))
  }

  v_estimate<-data.frame(preliminary.check = preliminary.check.tab, span = estimate.span.tab, tolerance = estimate.tol.tab, exact_ml = estimate.eml.tab, urfinal = estimate.urfinal.tab, row.names = c("Predefined","User_modif","Final"), stringsAsFactors=FALSE)
  v_transform <- data.frame(tfunction=transform.function.tab,fct=transform.fct.tab, stringsAsFactors=FALSE)
  v_usrdef <- data.frame(outlier= c(FALSE, usrdef.outliersEnabled,NA), outlier.coef= c(FALSE,NA,NA),
                         variables =c(FALSE, usrdef.varEnabled,NA), variables.coef = c(FALSE,NA,NA), stringsAsFactors=FALSE)
  v_trading.days<-data.frame( automatic = tradingdays.mauto.tab, pftd = tradingdays.pftd.tab, option = tradingdays.option.tab,
                              leapyear = tradingdays.leapyear.tab,stocktd = tradingdays.stocktd.tab, test = tradingdays.test.tab,
                              stringsAsFactors=FALSE)
  v_easter<-data.frame(type=easter.type.tab,julian=easter.julian.tab,duration=easter.duration.tab,test=easter.test.tab,
                       stringsAsFactors=FALSE)
  v_outliers<-data.frame(enabled=outlier.enabled.tab,span=outlier.span.tab,ao=outlier.ao.tab, tc=outlier.tc.tab, ls = outlier.ls.tab,
                         so=outlier.so.tab,usedefcv=outlier.usedefcv.tab,cv=outlier.cv.tab,eml=outlier.eml.tab,
                         tcrate=outlier.tcrate.tab, stringsAsFactors=FALSE)
  v_arima <-data.frame(enabled=automdl.enabled.tab,automdl.acceptdefault=automdl.acceptdefault.tab,automdl.cancel=automdl.cancel.tab,
                       automdl.ub1=automdl.ub1.tab,automdl.ub2=automdl.ub2.tab,automdl.armalimit=automdl.armalimit.tab,
                       automdl.reducecv=automdl.reducecv.tab, automdl.ljungboxlimit=automdl.ljungboxlimit.tab, compare = automdl.compare.tab,
                       arima.mu=arima.mu.tab,arima.p=arima.p.tab,arima.d =arima.d.tab,arima.q=arima.q.tab,
                       arima.bp=arima.bp.tab,arima.bd=arima.bd.tab,arima.bq=arima.bq.tab,arima.coef = c(FALSE,arima.coefEnabled,NA),
                       stringsAsFactors=FALSE)
  v_forecast <- data.frame(horizon = c(-2,fcst.horizon,NA), stringsAsFactors=FALSE)

  span.spec <-rspec$span

  # Final values
  x <- spec_estimateTS(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformTS(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdTS(td=v_trading.days)
  easter<- spec_easterTS(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersTS(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaTS(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
            transform=transform,
            regression=regression,
            outliers=outliers,
            arima=arima,
            forecast = forecast,
            span=span)
  class(z) = c("regarima_spec","TRAMO_SEATS")
  return(z)
}
# The function creates a ("regarima_spec","TRAMO_SEATS") class object from a regarima_Spec or a regarima object
#' @export
regarima_spec_tramoseats.TRAMO_SEATS <- function(spec,
                          preliminary.check = NA,
                          estimate.from=NA_character_,
                          estimate.to=NA_character_,
                          estimate.first=NA_integer_,
                          estimate.last=NA_integer_,
                          estimate.exclFirst=NA_integer_,
                          estimate.exclLast=NA_integer_,
                          estimate.tol=NA_integer_,
                          estimate.eml=NA,
                          estimate.urfinal=NA_integer_,
                          transform.function=c(NA_character_,"Auto","None","Log"),
                          transform.fct=NA_integer_,
                          usrdef.outliersEnabled = NA,
                          usrdef.outliersType = NA,
                          usrdef.outliersDate = NA,
                          usrdef.outliersCoef = NA,
                          usrdef.varEnabled = NA,
                          usrdef.var = NA,
                          usrdef.varType = NA,
                          usrdef.varCoef = NA,
                          tradingdays.mauto=c(NA_character_,"Unused","FTest","WaldTest"),
                          tradingdays.pftd=NA_integer_,
                          tradingdays.option = c(NA_character_,"TradingDays","WorkingDays","UserDefined","None"),
                          tradingdays.leapyear = NA,
                          tradingdays.stocktd = NA_integer_,
                          tradingdays.test = c(NA_character_,"Separate_T","Joint_F","None"),
                          easter.type = c(NA_character_,"Unused","Standard","IncludeEaster","IncludeEasterMonday"),
                          easter.julian = NA,
                          easter.duration = NA_integer_,
                          easter.test = NA,
                          outlier.enabled = NA,
                          outlier.from=NA_character_,
                          outlier.to=NA_character_,
                          outlier.first=NA_integer_,
                          outlier.last=NA_integer_,
                          outlier.exclFirst=NA_integer_,
                          outlier.exclLast=NA_integer_,
                          outlier.ao = NA,
                          outlier.tc = NA,
                          outlier.ls = NA,
                          outlier.so = NA,
                          outlier.usedefcv = NA,
                          outlier.cv = NA_integer_,
                          outlier.eml = NA,
                          outlier.tcrate = NA_integer_,
                          automdl.enabled = NA,
                          automdl.acceptdefault = NA,
                          automdl.cancel = NA_integer_,
                          automdl.ub1 = NA_integer_,
                          automdl.ub2 = NA_integer_,
                          automdl.armalimit = NA_integer_,
                          automdl.reducecv = NA_integer_,
                          automdl.ljungboxlimit = NA_integer_,
                          automdl.compare = NA,
                          arima.mu = NA,
                          arima.p = NA_integer_,
                          arima.d = NA_integer_,
                          arima.q = NA_integer_,
                          arima.bp = NA_integer_,
                          arima.bd = NA_integer_,
                          arima.bq = NA_integer_,
                          arima.coefEnabled = NA,
                          arima.coef= NA,
                          arima.coefType = NA,
                          fcst.horizon=NA_integer_)
{

  if (!inherits(spec, "TRAMO_SEATS") & (!inherits(spec, "regarima") | !inherits(spec, "regarima_spec")))
    stop("This function must only be used with c(\"regarima\",\"TRAMO_SEATS\") or c(\"regarima_spec\",\"TRAMO_SEATS\") objects", call. = FALSE)

  transform.function <-match.arg(transform.function)
  tradingdays.mauto <-match.arg(tradingdays.mauto)
  tradingdays.option <-match.arg(tradingdays.option)
  tradingdays.test <-match.arg(tradingdays.test)
  easter.type <-match.arg(easter.type)
  estimate.fromD <- as.Date(estimate.from)
  estimate.toD <- as.Date(estimate.to)
  outlier.fromD <- as.Date(outlier.from)
  outlier.toD <- as.Date(outlier.to)

  # To check & define the time span variables for estimates and outliers
  est.span <- spec_span(from=estimate.fromD,to=estimate.toD,first=estimate.first,last=estimate.last,
                        exclFirst=estimate.exclFirst,exclLast=estimate.exclLast, var="estimate")

  out.span <- spec_span(from=outlier.fromD,to=outlier.toD,first=outlier.first,last=outlier.last,
                        exclFirst=outlier.exclFirst,exclLast=outlier.exclLast, var="outlier")

  estimate.span <- as.character(est.span[1,1])
  outlier.span <- as.character(out.span[1,1])

  span <- rbind(est.span[,-1],out.span[,-1])
  rownames(span) <- c("estimate","outlier")

  # To check the predefined outliers varaiables
  predef.outliers <- spec_preOut(outliertype=usrdef.outliersType,
                                             outlierdate=usrdef.outliersDate, outliercoef=usrdef.outliersCoef)

  # To check the user-defined variables
  n.usrvar <- if (is.mts(usrdef.var)) {dim(usrdef.var)[2]} else if (is.ts(usrdef.var)) {1} else {0}
  predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, varcoef = usrdef.varCoef,
                                  tradingdays.option = tradingdays.option)

  # To check the ARIMA coefficients
  predef.coef <- spec_arimaCoef(coef = arima.coef, coeftype = arima.coefType)


  # To check the mode of the remaining variables
  list.logical = list("preliminary.check","usrdef.outliersEnabled","usrdef.varEnabled","estimate.eml","tradingdays.leapyear",
                      "easter.julian","easter.test","outlier.enabled","outlier.ao",
                      "outlier.tc","outlier.ls","outlier.so","outlier.usedefcv","outlier.eml","automdl.enabled",
                      "automdl.acceptdefault","automdl.compare","arima.mu","arima.coefEnabled")

  list.numeric = list("estimate.first","estimate.last","estimate.exclFirst","estimate.exclLast",
                      "outlier.first","outlier.last","outlier.exclFirst","outlier.exclLast",
                      "estimate.tol","estimate.urfinal","transform.fct","tradingdays.pftd",
                      "tradingdays.stocktd","easter.duration","outlier.cv","outlier.tcrate",
                      "automdl.cancel","automdl.ub1","automdl.ub2","automdl.armalimit","automdl.reducecv",
                      "automdl.ljungboxlimit","arima.p","arima.d","arima.q","arima.bp","arima.bd","arima.bq",
                      "fcst.horizon")

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
  estimate.spec <- s_estimate(spec)
  transform.spec <- s_transform(spec)
  usrdef.spec <- s_usrdef(spec)
  trading.days.spec <- s_td(spec)
  easter.spec <- s_easter(spec)
  outliers.spec <- s_out(spec)
  arima.spec <- s_arima(spec)
  forecast.spec <- s_fcst(spec)
  span.spec <- s_span(spec)

  predef.outliers.spec <- s_preOut(spec)
  predef.variables.spec <- s_preVar(spec)
  predef.coef.spec <- s_arimaCoef(spec)

  # Modified values
  predef.out <- list(Predefined = predef.outliers.spec, Final = predef.outliers)
  predef.var <- list(Predefined = predef.variables.spec, Final = predef.variables)
  arima.coeff <- list(Predefined = predef.coef.spec , Final = predef.coef)

  estimate.mod <- data.frame(preliminary.check = preliminary.check,
                            span = estimate.span, tolerance = estimate.tol,
                            exact_ml = estimate.eml, urfinal = estimate.urfinal, stringsAsFactors=FALSE)
  transform.mod <- data.frame(tfunction=transform.function,fct=transform.fct, stringsAsFactors=FALSE)
  usrdef.mod <- data.frame(outlier=usrdef.outliersEnabled, outlier.coef= NA,
                           variables = usrdef.varEnabled, variables.coef = NA, stringsAsFactors=FALSE)
  trading.days.mod <-data.frame( automatic = tradingdays.mauto, pftd = tradingdays.pftd, option = tradingdays.option,
                                leapyear = tradingdays.leapyear,stocktd = tradingdays.stocktd, test = tradingdays.test, stringsAsFactors=FALSE)
  easter.mod <-data.frame(type=easter.type,julian=easter.julian,duration=easter.duration,test=easter.test, stringsAsFactors=FALSE)
  outliers.mod <-data.frame(enabled=outlier.enabled,span=outlier.span,ao=outlier.ao, tc=outlier.tc, ls = outlier.ls,
                       so=outlier.so,usedefcv=outlier.usedefcv,cv=outlier.cv,eml=outlier.eml,tcrate=outlier.tcrate, stringsAsFactors=FALSE)
  arima.mod <-data.frame(enabled=automdl.enabled,automdl.acceptdefault=automdl.acceptdefault,automdl.cancel=automdl.cancel,
                    automdl.ub1=automdl.ub1,automdl.ub2=automdl.ub2,automdl.armalimit=automdl.armalimit,
                    automdl.reducecv=automdl.reducecv, automdl.ljungboxlimit=automdl.ljungboxlimit, compare = automdl.compare,
                    arima.mu=arima.mu,arima.p=arima.p,arima.d =arima.d,arima.q=arima.q,arima.bp=arima.bp,arima.bd=arima.bd,
                    arima.bq=arima.bq,arima.coef = arima.coefEnabled, stringsAsFactors=FALSE)
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
  x <- spec_estimateTS(est = v_estimate, spanP = span.spec, spanM = span)
  estimate <- x$est
  span <- x$span
  transform<-spec_transformTS(trans = v_transform)
  userdef <- spec_userdef(usrspc = v_usrdef, out = predef.out, var = predef.var, tf = transform[3,1])
  trading.days <- spec_tdTS(td=v_trading.days)
  easter<- spec_easterTS(easter=v_easter)
  regression<-list(userdef = userdef, trading.days = trading.days, easter = easter)
  y <- spec_outliersTS(out = v_outliers, spanP = span.spec, spanM = span)
  outliers <- y$out
  span <- y$span
  arima <- spec_arimaTS(arimaspc=v_arima, arimaco=arima.coeff)
  forecast <- spec_forecast(fcst=v_forecast)

  z <- list(estimate=estimate,
            transform=transform,
            regression=regression,
            outliers=outliers,
            arima=arima,
            forecast = forecast,
            span=span)
  class(z) = c("regarima_spec","TRAMO_SEATS")
  return(z)

}

