#Functions to extract different elements of the specification file
s_estimate <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$estimate)
  }else{
    obj <- object$estimate[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_transform <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$transform)
  }else{
    obj <- object$transform[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_usrdef<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$specification)
  }else{
    obj <- object$regression$userdef$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_preOut<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$outliers)
  }else{
    return(object$regression$userdef$outliers$Final)
  }
}

s_preVar<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$userdef$variables)
  }else{
    return(object$regression$userdef$variables$Final)
  }
}

s_td<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$trading.days)
  }else{
    obj <- object$regression$trading.days[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_easter<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$regression$easter)
  }else{
    obj <- object$regression$easter[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_out<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$outliers)
  }else{
    obj <- object$outliers[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_arima<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$arima$specification)
  }else{
    obj <- object$arima$specification[3,]
    rownames(obj) <- ""
    return(obj)
  }
}

s_arimaCoef<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$arima$coefficients)
  }else{
    return(object$arima$coefficients$Final)
  }
}

s_fcst <- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$forecast)
  }else{
    obj <- data.frame(horizon = object$forecast[3,],row.names = c(""))
    return(obj)
  }
}

s_span<- function(object=NA){
  if (!inherits(object, "JD_RegArima") & !inherits(object, "JD_RegArima_Spec"))
    stop("use only with \"JD_RegArima\" or \"JD_RegArima_Spec\" objects", call. = FALSE)

  if (inherits(object, "JD_RegArima")){
    return(object$specification$span)
  }else{
    return(object$span)
  }
}
