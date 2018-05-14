# Generic function to save model specification (RegARIMA and SA)
jd_saveSpec = function (object, file = "JD_Spec.RData") {
  if (!inherits(object, "X13") & !inherits(object, "TRAMO_SEATS")) {
    stop("use only with \"X13\" and \"TRAMO_SEATS\") class objects", call. = FALSE)
  }else{
  UseMethod("jd_saveSpec", object)
  }
}
# Method: JD_RegArima_Spec
jd_saveSpec.JD_RegArima_Spec = function(object, file){
  if (!inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima_Spec\" class object")

  estimate <- s_estimate(object)
  transform <- s_transform(object)
  usrdef <- s_usrdef(object)
  predef.outliers <- s_preOut(object)
  predef.variables <- s_preVar(object)
  trading.days <- s_td(object)
  easter <- s_easter(object)
  outliers <- s_out(object)
  arima.dsc <- s_arima(object)
  predef.coef <- s_arimaCoef(object)
  forecast <- s_fcst(object)
  span <- s_span(object)
  userdef <-list(specification = usrdef, outliers = predef.outliers, variables = predef.variables)
  regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = predef.coef)

  spec <- list(estimate=estimate, transform=transform, regression=regression,
                        outliers=outliers, arima=arima, forecast = forecast ,span=span)

  class(spec) <- if (inherits(object, "X13")) {"regarima_saveSpecX13"} else {"regarima_saveSpecTS"}
  save(spec, file = file)
}
# Method: JD_RegArima
jd_saveSpec.JD_RegArima = function(object, file){
  if (!inherits(object, "JD_RegArima"))
    stop("use only with \"JD_RegArima\" class object")

  spec = object$specification
  class(spec) <- if (inherits(object, "X13")) {"regarima_saveSpecX13"} else {"regarima_saveSpecTS"}
  save(spec, file = file)
}

# Generic function to load previously saved model specification
jd_loadSpec <- function (file = "JD_Spec.RData") {
  object <- get(load(file = file))
  if (("regarima_saveSpecX13" != class(object)) & ("regarima_saveSpecTS" != class(object)))
    stop("no model specification found in the file!\n")

  s.estimate <- object$estimate
  s.transform <- object$transform
  s.usrdef <- object$regression$userdef$specification
  s.predef.outliers <- object$regression$userdef$outliers
  s.predef.variables <- object$regression$userdef$variables
  s.trading.days <- object$regression$trading.days
  s.easter <- object$regression$easter
  s.outliers <- object$outliers
  s.arima.dsc <- object$arima$specification
  s.predef.coef <- object$arima$coefficients
  s.forecast <- object$forecast
  span <- object$span

  estimate<- rbind(s.estimate,rep(NA,length(s.estimate)),s.estimate)
  transform <- rbind(s.transform,rep(NA,length(s.transform)),s.transform)
  usrdef <- rbind(s.usrdef,rep(NA,length(s.usrdef)),s.usrdef)
  trading.days <- rbind(s.trading.days,rep(NA,length(s.trading.days)),s.trading.days)
  easter <- rbind(s.easter,rep(NA,length(s.easter)),s.easter)
  outliers <- rbind(s.outliers,rep(NA,length(s.outliers)),s.outliers)
  arima.dsc <- rbind(s.arima.dsc,rep(NA,length(s.arima.dsc)),s.arima.dsc)
  forecast <- rbind(s.forecast,rep(NA,length(s.estimate)),s.forecast)

  rownames(estimate)=c("Loaded","User_modif","Final")
  rownames(transform)=c("Loaded","User_modif","Final")
  rownames(usrdef)=c("Loaded","User_modif","Final")
  rownames(trading.days)=c("Loaded","User_modif","Final")
  rownames(easter)=c("Loaded","User_modif","Final")
  rownames(outliers)=c("Loaded","User_modif","Final")
  rownames(arima.dsc)=c("Loaded","User_modif","Final")
  rownames(forecast)=c("Loaded","User_modif","Final")

  userdef <-list(specification = usrdef, outliers = list(Predefined = s.predef.outliers, Final = s.predef.outliers),
                 variables = list(Predefined = s.predef.variables, Final = s.predef.variables))
  regression <- list(userdef=userdef, trading.days=trading.days, easter = easter)
  arima <- list(specification = arima.dsc, coefficients = list(Predefined = s.predef.coef, Final = s.predef.coef))

  spec <- list(estimate=estimate, transform=transform, regression=regression,
               outliers=outliers, arima=arima, forecast = forecast , span=span)

  class(spec) <- if (class(object)== "regarima_saveSpecX13") {c("JD_RegArima_Spec","X13")} else {c("JD_RegArima_Spec","TRAMO_SEATS")}
  return(spec)
}
