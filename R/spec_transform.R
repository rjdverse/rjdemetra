# Functions to maniputalte the "regarima_spec" object

spec_span<-function(from=NA_character_,to=NA_character_,first=NA_integer_,last=NA_integer_,
                    exclFirst=NA_integer_,exclLast=NA_integer_, var=NA_character_){

  x.from<-as.Date(from)
  x.to <- as.Date(to)
  x.first <-first
  x.last <- last
  x.exclFirst <- exclFirst
  x.exclLast <- exclLast

  if (sum(!is.na(x.from),!is.na(x.to))==2){
    if (x.to <= x.from){
      warning(paste(var,".to <= ",var,".from. Provided values will be ignored.",sep=""), call. = FALSE)
      x.to <-NA_character_
      x.from <-NA_character_
    }
  }

  if (sum(!is.na(x.from),!is.na(x.to),!is.na(x.last),!is.na(x.first),!is.na(x.exclFirst),
          !is.na(x.exclLast))==0) {
    z.span <- NA_character_
    z.type <- NA_character_
    z.d0 <- NA_character_
    z.d1 <- NA_character_
    z.n0 <- NA_integer_
    z.n1 <- NA_integer_

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  }else if (sum(!is.na(x.from),!is.na(x.to))==2){
    z.span  <- paste(as.character(x.from),"-",as.character(x.to), sep=" ")
    z.type <- "Between"
    z.d0 <- as.character(x.from)
    z.d1 <- as.character(x.to)
    z.n0 <- 0
    z.n1 <- 0
    if (sum(!is.na(x.last),!is.na(x.first),!is.na(x.exclFirst),!is.na(x.exclLast))!=0) {
      warning(paste("(",var,".to, ",var,".from) used. Remainig variables (",var,".last,",var,".first,",var,".exclFirst,",var,".exclLast) will be ignored.",sep=""), call. = FALSE)
    }

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (sum(!is.na(x.from),!is.na(x.to))==1) {
    if (!is.na(x.from)) {
      z.span  <- paste("From",as.character(x.from), sep=" ")
      z.type <- "From"
      z.d0 <- as.character(x.from)
      z.d1 <- "2020-12-31"
      z.n0 <- 0
      z.n1 <- 0
    }else{
      z.span  <- paste("Until",as.character(x.to), sep=" ")
      z.type <- "To"
      z.d0 <- "1900-01-01"
      z.d1 <- as.character(x.to)
      z.n0 <- 0
      z.n1 <- 0
    }
    if (sum(!is.na(x.last),!is.na(x.first),!is.na(x.exclFirst),!is.na(x.exclLast))!=0) {
      warning(paste("(",var,".to, ",var,".from) used. Remainig variables (",var,".last,",var,".first,",var,".exclFirst,",var,".exclLast) will be ignored.",sep=""), call. = FALSE)
    }

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (!is.na(x.last) & x.last> 0) {
    z.span  <- paste("Last",as.character(x.last),"periods", sep=" ")
    z.type <- "Last"
    z.d0 <- "1900-01-01"
    z.d1 <- "2020-12-31"
    z.n0 <- 0
    z.n1 <- x.last
    if (sum(!is.na(x.first),!is.na(x.exclFirst),!is.na(x.exclLast))!=0) {
      warning(paste("(",var,".last used. Remainig variables (",var,".first,",var,".exclFirst,",var,".exclLast) will be ignored.",sep=""), call. = FALSE)
    }

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (!is.na(x.first) & x.first> 0) {
    z.span  <- paste("First",as.character(x.first),"periods", sep=" ")
    z.type <- "First"
    z.d0 <- "1900-01-01"
    z.d1 <- "2020-12-31"
    z.n0 <- x.first
    z.n1 <- 0
    if (sum(!is.na(x.exclFirst),!is.na(x.exclLast))!=0) {
      warning(paste("(",var,".first used. Remainig variables (",var,".exclFirst,",var,".exclLast) will be ignored.",sep=""), call. = FALSE)
    }

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (sum(!is.na(x.exclFirst),!is.na(x.exclLast))==2){

    if (sum(x.exclFirst,x.exclLast)==0){
      z.span <- "All"
      z.type <- "All"
      z.d0 <- "1900-01-01"
      z.d1 <- "2020-12-31"
      z.n0 <- 0
      z.n1 <- 0
    }else{
      z.type <- "Excluding"
      z.d0 <- "1900-01-01"
      z.d1 <- "2020-12-31"
      z.n0 <- x.exclFirst
      z.n1 <- x.exclLast

      if (x.exclFirst==0) {
        z.span  <- paste("All but last",as.character(x.exclLast),"periods", sep=" ")
      } else if (x.exclLast==0) {
        z.span  <- paste("All but first",as.character(x.exclFirst),"periods", sep=" ")
      }else{
        z.span  <- paste("All but first",as.character(x.exclFirst),"periods and last",as.character(x.exclLast),"periods", sep=" ")
      }
    }

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  }else if ((is.na(x.exclFirst)& x.exclLast==0)| (is.na(x.exclLast)& x.exclFirst==0)){
    z.span <- "All"
    z.type <- "All"
    z.d0 <- "1900-01-01"
    z.d1 <- "2020-12-31"
    z.n0 <- 0
    z.n1 <- 0

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (!is.na(x.exclFirst)) {
    z.span <- paste("All but first",as.character(x.exclFirst),"periods", sep=" ")
    z.type <- "Excluding"
    z.d0 <- "1900-01-01"
    z.d1 <- "2020-12-31"
    z.n0 <- x.exclFirst
    z.n1 <- 0

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  } else if (!is.na(x.exclLast)) {
    z.span <- paste("All but last",as.character(x.exclLast),"periods", sep=" ")
    z.type <- "Excluding"
    z.d0 <- "1900-01-01"
    z.d1 <- "2020-12-31"
    z.n0 <- 0
    z.n1 <- x.exclLast

    z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
    return(z)

  }else{
    z.span <- NA_character_
    z.type <- NA_character_
    z.d0 <- NA_character_
    z.d1 <- NA_character_
    z.n0 <- NA_integer_
    z.n1 <- NA_integer_
  }

  z<- data.frame(span=z.span,type=z.type,d0=z.d0,d1=z.d1,n0=z.n0,n1=z.n1, row.names= "")
  return(z)

}

spec_preOut<-function(outliertype=NA,outlierdate=NA,outliercoef=NA){
  outliers.type <- c("AO","LS","TC","SO")
  if (sum(is.na(outliertype))==0){
    if (!is.vector(outliertype)|is.list(outliertype)|length(setdiff(outliertype,outliers.type))>0){
      warning("wrong format of the userdef.outliertype. Pre-specified outlier(s) will be ignored.", call. = FALSE)
      return(NA)
    }else if (!is.vector(outlierdate)| sum(is.na(as.Date(outlierdate)))>0){
      warning("wrong format of the userdef.outlierdate. Pre-specified outlier(s) will be ignored.", call. = FALSE)
      return(NA)
    }else if (length(outliertype)!=length(outlierdate)){
      warning("userdef.outlierdate is not of the same length as userdef.outliertype. Pre-specified outlier(s) will be ignored.", call. = FALSE)
      return(NA)
    }else{
      if (sum(is.na(outliercoef))!=0){
        outliers <-data.frame(type = outliertype, date = outlierdate, coeff = rep(NA,length(outliertype)))
      }else if(!is.vector(outliercoef)|!is.numeric(outliercoef)|(length(outliercoef)!= length(outliertype))){
        warning("userdef.outliercoef is wrongly specified. The coefficient(s) will be ignored.", call. = FALSE)
        outliers <-data.frame(type = outliertype, date = outlierdate, coeff = rep(NA,length(outliertype)))
      }else{
        outliers <-data.frame(type = outliertype, date = outlierdate, coeff = outliercoef)
      }
    }
  }else{
    return(NA)
  }

  outliers
}

spec_preVar<-function(var = NA, vartype = NA, varcoef = NA, tradingdays.option = NA){

  variables.type <- c("Undefined","Series","Trend","Seasonal","SeasonallyAdjusted","Irregular","Calendar")
  nvar <- if (is.mts(var)) {dim(var)[2]} else if (is.ts(var)) {1} else {0}

  if (all(sapply(vartype,is.na)))
    vartype <- rep("Undefined", nvar)

  if (sum(!is.na(var))!=0){
    if (!is.ts(var) & !is.mts(var)){
      warning("userdef.var must be a time series or a matrix of time series. User-defined variable(s) will be ignored.", call. = FALSE)
      vars <- list(series = NA, description = NA)
      return(vars)
    }else if (!is.vector(vartype)|is.list(vartype)|length(setdiff(vartype,variables.type))>0) {
      warning("wrong format of the userdef.varType. User-defined variable(s) will be ignored.", call. = FALSE)
      vars <- list(series = NA, description = NA)
      return(vars)
    }else if (nvar!=length(vartype)){
      warning("userdef.varType does not have the same number of elements as userdef.var. User-defined variable(s) will be ignored.", call. = FALSE)
      vars <- list(series = NA, description = NA)
      return(vars)
    }else{
      #Pre-defined calendar
      if (!is.na(tradingdays.option)){
        var_calendar <- grep("Calendar", vartype)
        if(tradingdays.option != "UserDefined" && length(var_calendar) > 0){
          warning("userdef.VarType contains Calendar variables but the tradingdays.options isn't UserDefined.\n",
                  "Corresponding variables will be ignored.", call. = FALSE)
          if(length(var_calendar) == nvar){
            return(list(series = NA, description = NA))
          }else{
            var <- var[,-var_calendar]
            vartype <- vartype[-var_calendar]
            if(sum(is.na(varcoef))!=0 | !is.vector(varcoef) |
               !is.numeric(varcoef)|
               (length(varcoef)!= length(vartype))){
              varcoef <- NA
            }else{
              varcoef <- varcoef[-var_calendar]
            }
          }

        }
      }
      #The names of the variables
      if(is.mts(var)){
        description_names <- base::make.names(colnames(var), unique = TRUE)
        description_names <- gsub(".","_", description_names, fixed = TRUE)
      }else{
        description_names <- "userdef"
      }
      if (sum(is.na(varcoef)) != 0){
        vars <- list(series = var,
                     description = data.frame(type = vartype, coeff = NA,
                                              row.names = description_names))
      }else if(!is.vector(varcoef)|!is.numeric(varcoef)|(length(varcoef)!= length(vartype))){
        warning("userdef.varCoef is wrongly specified. The coefficient(s) will be ignored.", call. = FALSE)
        vars <- list(series = var,
                     description = data.frame(type = vartype, coeff = NA,
                                                            row.names = description_names))
      }else{
        vars <- list(series = var,
                     description = data.frame(type = vartype, coeff = varcoef,
                                                            row.names = description_names))
      }
    }

  }else{
    vars <- list(series = NA, description = NA)
  }
  vars
}

spec_arimaCoef<-function(coef = NA, coeftype= NA){

  coef.types <- c("Fixed", "Initial","Undefined")

  if (sum(!is.na(coef))!=0){
    if (!is.vector(coef)|is.list(coef)| !is.numeric(coef)){
      warning("wrong format of the arima.coef. Defined ARIMA coef. variable(s) will be ignored.", call. = FALSE)
      return(NA)
    }else if (!is.vector(coeftype)|is.list(coeftype)|length(setdiff(coeftype,coef.types))>0) {
      warning("wrong format of the arima.coefType. Defined ARIMA coef. variable(s) will be ignored.", call. = FALSE)
      return(NA)
    } else if (length(coef)!=length(coeftype)){
      warning("arima.coefType is not of the same length as arima.coef. Defined ARIMA coef. variable(s) will be ignored.", call. = FALSE)
      return(NA)
    }else{
      coeff <-data.frame(Type = coeftype,  Value = coef )
    }
  }else{
    return(NA)
  }
  return(coeff)
}

spec_arimaCoefF <- function(enabled=NA, armaP=NA, armaF=NA , coefP=NA, coefF=NA){

  if (enabled == TRUE & sum(!is.na(coefF))==0 & sum(!is.na(coefP))!=0){
    if (dim(coefP)[1]== sum(armaP) & identical(armaP,armaF)){
      coef <- coefP
      ena <- TRUE
    } else {
      coef <- NA
      ena<- FALSE
    }
  }else if (sum(!is.na(coefF))==0){
    coef <- NA
    ena<- FALSE
  }else if (dim(coefF)[1]!= sum(armaP)){
    coef <- NA
    ena<- FALSE
    warning("wrong number of ARIMA coef was provided (arima.coef and arima.coefType). Defined ARIMA coef. variable(s) will be ignored.", call. = FALSE)
  } else {

    dsc <-c()
    if (armaP[1]!=0){
      for (i in 1:armaP[1]){dsc <- c(dsc,paste("Phi(",as.character(i),")",sep=""))}
    }
    if (armaP[2]!=0){
      for (i in 1:armaP[2]){dsc <-c(dsc,paste("Theta(",as.character(i),")",sep=""))}
    }
    if (armaP[3]!=0){
      for (i in 1:armaP[3]){dsc <-c(dsc,paste("BPhi(",as.character(i),")",sep=""))}
    }
    if (armaP[4]!=0){
      for (i in 1:armaP[4]){dsc <-c(dsc,paste("BTheta(",as.character(i),")",sep=""))}
    }
    coef <- coefF
    rownames(coef) <- dsc
    ena <- enabled
  }
  return(list(ena, coef))
}

spec_seasma <- function(seasma=NA){

  len <- length(seasma)
  seasma.type <- c("Msr","Stable","X11Default","S3X1","S3X3","S3X5","S3X9","S3X15")
  if (sum(is.na(seasma))!=0){
    return(NA)
  } else if (!is.vector(seasma)|is.list(seasma)|length(setdiff(seasma,seasma.type))>0){
      warning("wrong format of the x11.seasonalma.\nPossisble filters per period: \"Msr\",\"Stable\", \"X11Default\", \"S3X1\", \"S3X3\", \"S3X5\", \"S3X9\" and \"S3X15\".\nPre-specified seasonal filters will be ignored.", call. = FALSE)
      return(NA)
  } else if (!(len %in% c(1,2,4,12))){
    warning("wrong format of the x11.seasonalma.\nPre-specified seasonal filters will be ignored.", call. = FALSE)
    return(NA)
  } else {
    z <- toString(seasma)
    return(z)
  }
}

spec_trendma <- function(trendma=NA){

  if (sum(!is.na(trendma))==0){
    return(NA)
  } else if (!is.numeric(trendma)){
    return(NA)
    warning("Variable x11.trendma should be numeric.\nThe variable will be ignored.", call. = FALSE)
  } else if (trendma <= 1 | trendma > 101 | (floor(trendma/2)==trendma/2)) {
    warning("Variable x11.trendma should be in the range (1,101] and be an odd number.\nThe variable will be ignored.", call. = FALSE)
    return(NA)
    } else {
    return(trendma)
  }
}

# Derive the final values in the "regarima_spec" object
# X-13
spec_estimateX13<-function(est, spanP, spanM){

  span <-spanM

  est[3,1] <- if(!is.na(est[2,1])) {est[2,1]} else {est[1,1]}
  if (is.na(est[2,1])) {
    span <- rbind(spanP[1,],spanM[2,])
    rownames(span) <- c("estimate","outlier")
  }
  est[3,2] <- if(!is.na(est[2,2])) {est[2,2]} else {est[1,2]}
  rownames(est)  <- c("Predefined","User_modif","Final")
  return(list(est=est,span=span))
}

spec_transformX13<-function(trans){

  trans[3,1] <- if(!is.na(trans[2,1])) {trans[2,1]} else {trans[1,1]}

  if (trans[3,1]== "None"){
    trans[3,2] <-"None"
    trans[3,3] <-trans[1,3]
  } else if (trans[3,1]== "Log"){
    trans[3,3] <-trans[1,3]
  } else if (trans[3,1]== "Auto"){
    trans[3,2] <-"None"
  }
  if (is.na(trans[3,2]))
    trans[3,2] <- if(!is.na(trans[2,2])) {trans[2,2]} else {trans[1,2]}
  if (is.na(trans[3,3]))
    trans[3,3] <- if(!is.na(trans[2,3])) {trans[2,3]} else {trans[1,3]}

  rownames(trans) <- c("Predefined","User_modif","Final")
  return(trans)
}

spec_tdX13<-function(td, tf, tadj){

  td[3,1] <- if(!is.na(td[2,1])) {td[2,1]} else {td[1,1]}
  td[3,4] <- if(!is.na(td[2,4])) {td[2,4]} else {td[1,4]}

  if (td[3,1]=="None" & td[3,4]==0) {
    td[3,2]<- td[1,2]
    td[3,c(3,5)]<- "None"
  } else if (td[3,1]=="None") {
    td[3,2]<- td[1,2]
    td[3,3]<- "None"
  } else {
    td[3,4] <-0
  }
  if (is.na(td[3,2])){
    td[3,2] <- if(!is.na(td[2,2])) {td[2,2]} else {td[1,2]}

    if (td[3,2]==TRUE & as.character(tf)=="Auto") {
      td[3,3]<- td[1,3]
    }else{
      td[3,2]<-FALSE
    }
  }
  if (is.na(td[3,3])){
    if (tadj!="None") {
      td[3,3] <- "None"
    }else{
      td[3,3] <- if(!is.na(td[2,3])) {td[2,3]} else {td[1,3]}
    }
  }
  if (is.na(td[3,5]))
    td[3,5] <- if(!is.na(td[2,5])) {td[2,5]} else {td[1,5]}

  #UserDefined TD regressors
  if(td[3,1] == "UserDefined"){
    td[3,3] <- "None"
    td[3,2] <- FALSE
    td[3,4] <- 0
  }
  rownames(td) <- c("Predefined","User_modif","Final")
  return(td)
}

spec_easterX13<-function(easter){

  easter[3,1] <- if(!is.na(easter[2,1])) {easter[2,1]} else {easter[1,1]}

  if (easter[3,1]== FALSE) {
    easter[3,2] <-FALSE
    easter[3,3] <-easter[1,3]
    easter[3,4] <- "None"
  }
  if (is.na(easter[3,2])){
    easter[3,2] <- if (!is.na(easter[2,2])) {easter[2,2]} else {easter[1,2]}
  }
  if (is.na(easter[3,4])){
    easter[3,4] <- if (!is.na(easter[2,4])) {easter[2,4]} else {easter[1,4]}
  }
  if (is.na(easter[3,3])){
    if (easter[3,4]=="Add") {
      easter[3,3] <-easter[1,3]
    }else{
      easter[3,3] <- if (!is.na(easter[2,3])) {easter[2,3]} else {easter[1,3]}
    }
  }
  rownames(easter) <- c("Predefined","User_modif","Final")
  return(easter)
}

spec_outliersX13<-function(out, spanP, spanM){

  span <-spanM
  out[3,1]<- if (!is.na(out[2,1])) {out[2,1]} else {out[1,1]}
  out[3,3]<- if (!is.na(out[2,3])) {out[2,3]} else {out[1,3]}
  out[3,4]<- if (!is.na(out[2,4])) {out[2,4]} else {out[1,4]}
  out[3,5]<- if (!is.na(out[2,5])) {out[2,5]} else {out[1,5]}
  out[3,6]<- if (!is.na(out[2,6])) {out[2,6]} else {out[1,6]}

  if ((out[3,1]==FALSE) | (sum(out[3,3],out[3,4],out[3,5],out[3,6])==0)) {
    out[3,c(1,3:6)]<- FALSE
    out[3,c(2,7:10)]<- out[1,c(2,7:10)]
    span <- rbind(spanM[1,],spanP[2,])
    rownames(span) <- c("estimate","outlier")
  }else{
    out[3,2]<- if (!is.na(out[2,2])) {out[2,2]} else {out[1,2]}
    out[3,7]<- if (!is.na(out[2,7])) {out[2,7]} else {out[1,7]}
    out[3,8]<- if (!is.na(out[2,8])) {out[2,8]} else {out[1,8]}
    out[3,9]<- if (!is.na(out[2,9])) {out[2,9]} else {out[1,9]}
    out[3,10]<- if (!is.na(out[2,10])) {out[2,10]} else {out[1,10]}

    if (out[3,7]==TRUE) {out[3,8]<-4}
  }
  if (is.na(out[2,2])) {
   span <- rbind(spanM[1,],spanP[2,])
    rownames(span) <- c("estimate","outlier")
  }

  rownames(out) <- c("Predefined","User_modif","Final")
  return(list(out=out,span=span))
}

spec_arimaX13 <-function(arimaspc, arimaco){

  arimacoF <- arimaco$Final
  arimacoP <- arimaco$Predefined

  arimaspc[3,1]<- if (!is.na(arimaspc[2,1])) {arimaspc[2,1]} else {arimaspc[1,1]}

  if (arimaspc[3,1] == FALSE){
    arimaspc[3,2:11] <- arimaspc[1,2:11]
    arimaspc[3,12]<- if (!is.na(arimaspc[2,12])) {arimaspc[2,12]} else {arimaspc[1,12]}
    arimaspc[3,13]<- if (!is.na(arimaspc[2,13])) {arimaspc[2,13]} else {arimaspc[1,13]}
    arimaspc[3,14]<- if (!is.na(arimaspc[2,14])) {arimaspc[2,14]} else {arimaspc[1,14]}
    arimaspc[3,15]<- if (!is.na(arimaspc[2,15])) {arimaspc[2,15]} else {arimaspc[1,15]}
    arimaspc[3,16]<- if (!is.na(arimaspc[2,16])) {arimaspc[2,16]} else {arimaspc[1,16]}
    arimaspc[3,17]<- if (!is.na(arimaspc[2,17])) {arimaspc[2,17]} else {arimaspc[1,17]}
    arimaspc[3,18]<- if (!is.na(arimaspc[2,18])) {arimaspc[2,18]} else {arimaspc[1,18]}
    arimaspc[3,19]<- if (!is.na(arimaspc[2,19])) {arimaspc[2,19]} else {arimaspc[1,19]}
  } else {
    arimaspc[3,2]<- if (!is.na(arimaspc[2,2])) {arimaspc[2,2]} else {arimaspc[1,2]}
    arimaspc[3,3]<- if (!is.na(arimaspc[2,3])) {arimaspc[2,3]} else {arimaspc[1,3]}
    arimaspc[3,4]<- if (!is.na(arimaspc[2,4])) {arimaspc[2,4]} else {arimaspc[1,4]}
    arimaspc[3,5]<- if (!is.na(arimaspc[2,5])) {arimaspc[2,5]} else {arimaspc[1,5]}
    arimaspc[3,6]<- if (!is.na(arimaspc[2,6])) {arimaspc[2,6]} else {arimaspc[1,6]}
    arimaspc[3,7]<- if (!is.na(arimaspc[2,7])) {arimaspc[2,7]} else {arimaspc[1,7]}
    arimaspc[3,8]<- if (!is.na(arimaspc[2,8])) {arimaspc[2,8]} else {arimaspc[1,8]}
    arimaspc[3,9]<- if (!is.na(arimaspc[2,9])) {arimaspc[2,9]} else {arimaspc[1,9]}
    arimaspc[3,10]<- if (!is.na(arimaspc[2,10])) {arimaspc[2,10]} else {arimaspc[1,10]}
    arimaspc[3,11]<- if (!is.na(arimaspc[2,11])) {arimaspc[2,11]} else {arimaspc[1,11]}
    arimaspc[3,12:18] <- arimaspc[1,12:18]
    arimaspc[3,19] <- FALSE
  }
  # defined ARIMA-coefficents
  arma.cp <- c(arimaspc[3,13],arimaspc[3,15],arimaspc[3,16],arimaspc[3,18])
  arma.cf <- c(arimaspc[1,13],arimaspc[1,15],arimaspc[1,16],arimaspc[1,18])

  arimacoefFinal <- spec_arimaCoefF(enabled = arimaspc[3,19], armaP = arma.cp, armaF = arma.cf,
                                  coefP = arimacoP, coefF = arimacoF)

  arimaspc[3,19] <- as.logical(arimacoefFinal[1])
  arimacoF <- if (is.na(arimacoefFinal[2])) {NA} else {as.data.frame(arimacoefFinal[2])}

  rownames(arimaspc) <- c("Predefined","User_modif","Final")
  x <- list(Predefined = arimacoP , Final = arimacoF)
  y <- list(specification = arimaspc, coefficients = x)
  return(y)
}


# TRAMO_SEATS
spec_estimateTS<-function(est, spanP, spanM){

  span <-spanM

  est[3,1] <- if(!is.na(est[2,1])) {est[2,1]} else {est[1,1]}
  if (is.na(est[2,1])) {
    span <- rbind(spanP[1,],spanM[2,])
    rownames(span) <- c("estimate","outlier")
  }
  est[3,2] <- if(!is.na(est[2,2])) {est[2,2]} else {est[1,2]}
  est[3,3] <- if(!is.na(est[2,3])) {est[2,3]} else {est[1,3]}
  est[3,4] <- if(!is.na(est[2,4])) {est[2,4]} else {est[1,4]}

  rownames(est) <- c("Predefined","User_modif","Final")
  return(list(est=est,span=span))
}

spec_transformTS<-function(trans){

  trans[3,1] <- if(!is.na(trans[2,1])) {trans[2,1]} else {trans[1,1]}

  if (trans[3,1]=="Auto") {
    trans[3,2] <- if(!is.na(trans[2,2])) {trans[2,2]} else {trans[1,2]}
  }else{
    trans[3,2]<-trans[1,2]
  }

  rownames(trans) <- c("Predefined","User_modif","Final")
  return(trans)
}

spec_tdTS<-function(td){

  td[3,1] <- if(!is.na(td[2,1])) {td[2,1]} else {td[1,1]}

  if (td[3,1]!= "Unused"){
    td[3,2] <- if(!is.na(td[2,2])) {td[2,2]} else {td[1,2]}
    td[3,3:6] <- td[1,3:6]
  }else{
    td[3,2] <-td[1,2]
    td[3,3] <- if(!is.na(td[2,3])) {td[2,3]} else {td[1,3]}
    td[3,5] <- if(!is.na(td[2,5])) {td[2,5]} else {td[1,5]}

    if (td[3,3]=="None" & td[3,5]==0){
      td[3,4] <- FALSE
      td[3,6] <- "None"
    }else if (td[3,3]=="None"){
     td[3,4] <- FALSE
      td[3,6] <- if(!is.na(td[2,6])) {td[2,6]} else {td[1,6]}
    }else{
      td[3,5]<-0
      td[3,4] <- if(!is.na(td[2,4])) {td[2,4]} else {td[1,4]}
      td[3,6] <- if(!is.na(td[2,6])) {td[2,6]} else {td[1,6]}
    }
  }

  #UserDefined TD regressors
  if(td[3,3] == "UserDefined"){
    td[3,4] <- FALSE
    td[3,5] <- 0
  }

  rownames(td) <- c("Predefined","User_modif","Final")
  return(td)
}

spec_easterTS<-function(easter){

  easter[3,1] <- if(!is.na(easter[2,1])) {easter[2,1]} else {easter[1,1]}

  if (easter[3,1] == "Unused"){
    easter[3,2] <- FALSE
    easter[3,3:4] <- easter[1,3:4]
  }else{
    easter[3,2] <- if(!is.na(easter[2,2])) {easter[2,2]} else {easter[1,2]}
    easter[3,3] <- if(!is.na(easter[2,3])) {easter[2,3]} else {easter[1,3]}
    easter[3,4] <- if(!is.na(easter[2,4])) {easter[2,4]} else {easter[1,4]}
  }

  rownames(easter) <- c("Predefined","User_modif","Final")
  return(easter)
}

spec_outliersTS<-function(out, spanP, spanM){

  span <-spanM

  out[3,1]<- if (!is.na(out[2,1])) {out[2,1]} else {out[1,1]}
  out[3,3]<- if (!is.na(out[2,3])) {out[2,3]} else {out[1,3]}
  out[3,4]<- if (!is.na(out[2,4])) {out[2,4]} else {out[1,4]}
  out[3,5]<- if (!is.na(out[2,5])) {out[2,5]} else {out[1,5]}
  out[3,6]<- if (!is.na(out[2,6])) {out[2,6]} else {out[1,6]}

  if ((out[3,1]==FALSE) | (sum(out[3,3],out[3,4],out[3,5],out[3,6])==0)) {
    out[3,c(1,3:6)]<- FALSE
    out[3,c(2,7:10)]<- out[1,c(2,7:10)]
    span <- rbind(spanM[1,],spanP[2,])
    rownames(span) <- c("estimate","outlier")
  }else{
    out[3,2]<- if (!is.na(out[2,2])) {out[2,2]} else {out[1,2]}
    out[3,7]<- if (!is.na(out[2,7])) {out[2,7]} else {out[1,7]}
    out[3,8]<- if (!is.na(out[2,8])) {out[2,8]} else {out[1,8]}
    out[3,9]<- if (!is.na(out[2,9])) {out[2,9]} else {out[1,9]}
    out[3,10]<- if (!is.na(out[2,10])) {out[2,10]} else {out[1,10]}

    if (out[3,7]==TRUE) {out[3,8]<-3.5}
  }
  if (is.na(out[2,2])) {
    span <- rbind(spanM[1,],spanP[2,])
    rownames(span) <- c("estimate","outlier")
  }

  rownames(out) <- c("Predefined","User_modif","Final")
  return(list(out=out,span=span))
}

spec_arimaTS <-function(arimaspc, arimaco){

  arimacoF <- arimaco$Final
  arimacoP <- arimaco$Predefined

  arimaspc[3,1]<- if (!is.na(arimaspc[2,1])) {arimaspc[2,1]} else {arimaspc[1,1]}

  if (arimaspc[3,1] == FALSE){
    arimaspc[3,2:9] <- arimaspc[1,2:9]
    arimaspc[3,10]<- if (!is.na(arimaspc[2,10])) {arimaspc[2,10]} else {arimaspc[1,10]}
    arimaspc[3,11]<- if (!is.na(arimaspc[2,11])) {arimaspc[2,11]} else {arimaspc[1,11]}
    arimaspc[3,12]<- if (!is.na(arimaspc[2,12])) {arimaspc[2,12]} else {arimaspc[1,12]}
    arimaspc[3,13]<- if (!is.na(arimaspc[2,13])) {arimaspc[2,13]} else {arimaspc[1,13]}
    arimaspc[3,14]<- if (!is.na(arimaspc[2,14])) {arimaspc[2,14]} else {arimaspc[1,14]}
    arimaspc[3,15]<- if (!is.na(arimaspc[2,15])) {arimaspc[2,15]} else {arimaspc[1,15]}
    arimaspc[3,16]<- if (!is.na(arimaspc[2,16])) {arimaspc[2,16]} else {arimaspc[1,16]}
    arimaspc[3,17]<- if (!is.na(arimaspc[2,17])) {arimaspc[2,17]} else {arimaspc[1,17]}
  }else{
    arimaspc[3,2]<- if (!is.na(arimaspc[2,2])) {arimaspc[2,2]} else {arimaspc[1,2]}
    arimaspc[3,3]<- if (!is.na(arimaspc[2,3])) {arimaspc[2,3]} else {arimaspc[1,3]}
    arimaspc[3,4]<- if (!is.na(arimaspc[2,4])) {arimaspc[2,4]} else {arimaspc[1,4]}
    arimaspc[3,5]<- if (!is.na(arimaspc[2,5])) {arimaspc[2,5]} else {arimaspc[1,5]}
    arimaspc[3,6]<- if (!is.na(arimaspc[2,6])) {arimaspc[2,6]} else {arimaspc[1,6]}
    arimaspc[3,7]<- if (!is.na(arimaspc[2,7])) {arimaspc[2,7]} else {arimaspc[1,7]}
    arimaspc[3,8]<- if (!is.na(arimaspc[2,8])) {arimaspc[2,8]} else {arimaspc[1,8]}
    arimaspc[3,9]<- if (!is.na(arimaspc[2,9])) {arimaspc[2,9]} else {arimaspc[1,9]}
    arimaspc[3,10:16] <- arimaspc[1,10:16]
    arimaspc[3,17] <- FALSE
  }
  #Defined ARIMA-coefficents
  arma.cp <- c(arimaspc[3,11],arimaspc[3,13],arimaspc[3,14],arimaspc[3,16])
  arma.cf <- c(arimaspc[1,11],arimaspc[1,13],arimaspc[1,14],arimaspc[1,16])

  arimacoefFinal <- spec_arimaCoefF(enabled = arimaspc[3,17], armaP = arma.cp, armaF = arma.cf,
                                    coefP = arimacoP, coefF = arimacoF)

  arimaspc[3,17] <- as.logical(arimacoefFinal[1])
  arimacoF <- if (is.na(arimacoefFinal[2])) {NA} else {as.data.frame(arimacoefFinal[2])}

  rownames(arimaspc) <- c("Predefined","User_modif","Final")
  x <- list(Predefined = arimacoP , Final = arimacoF)
  y <- list(specification = arimaspc, coefficients = x)
  return(y)
}

# Common for X-13 and TRAMO-SEATS

spec_userdef <-function(usrspc, out, var, tf) {
  outF <- out$Final
  outP <- out$Predefined
  varF <- var$Final
  varP <- var$Predefined

  if(is.na(usrspc [2,1])){
    usrspc[3,1] <- usrspc[1,1]
    if (usrspc[1,1]==TRUE & (sum(!is.na(outF))==0)){
      outF<- outP
    }
  }else if (usrspc[2,1]==TRUE & usrspc[1,1]==TRUE & sum(!is.na(outF))==0){
    usrspc[3,1] <- TRUE
    outF<- outP
  }else if(sum(!is.na(outF))==0){
    usrspc[3,1]<-FALSE
  }else{
    usrspc[3,1] <- usrspc[2,1]
  }
  if (usrspc[3,1]==TRUE){
    if (sum(!is.na(outF[,3]))==0){
      usrspc[2:3,2]<- FALSE
    }else if (tf=="Auto"){
      usrspc[2,2]<- TRUE
      usrspc[3,2]<- FALSE
    }else{
      usrspc[2:3,2]<- TRUE
    }
  }else{
    usrspc[3,2]<- FALSE
  }

  if(is.na(usrspc[2,3])){
    usrspc[3,3] <- usrspc[1,3]
    if (usrspc[1,3]==TRUE & (sum(!is.na(varF$series))==0)){
      varF<- varP
    }
  }else if (usrspc[2,3]==TRUE & usrspc[1,3]==TRUE & sum(!is.na(varF$series))==0){
    usrspc[3,3] <- TRUE
    varF<- varP
  }else if(sum(!is.na(varF$series))==0){
    usrspc[3,3]<-FALSE
  }else{
    usrspc[3,3] <- usrspc[2,3]
  }
  if (usrspc[3,3]==TRUE){
    if (sum(!is.na(varF$description[,2]))==0){
      usrspc[2:3,4]<- FALSE
    }else if (tf=="Auto"){
      usrspc[2,4]<- TRUE
      usrspc[3,4]<- FALSE
    }else{
      usrspc[2:3,4]<- TRUE
    }
  }else{
    usrspc[3,4]<- FALSE
  }

  rownames(usrspc) <- c("Predefined","User_modif","Final")
  outliers <- list(Predefined = outP, Final = outF)
  variables <- list(Predefined = varP, Final = varF)
  x <- list(specification = usrspc, outliers = outliers, variables = variables)
  return(x)
}

spec_forecast <-function(fcst){
  fcst[3,1] <- if (!is.na(fcst[2,1])) {fcst[2,1]} else {fcst[1,1]}
  rownames(fcst) <- c("Predefined","User_modif","Final")
  return(fcst)
}

# X11/ SEATS

spec_x11 <- function(x11spc){

  x11 <- x11spc

  for (i in c(1:5,8:10)){
    x11[3,i] <- if(!is.na(x11[2,i])) {x11[2,i]} else {x11[1,i]}
  }
  if(x11[3,5]==TRUE | is.na(x11[2,6])){
    x11[3,6]<- x11[1,6]
  }else {
    x11[3,6] <- x11[2,6]
  }
  if(x11[3,2]==FALSE | is.na(x11[2,7])){
    x11[3,7] <- x11[1,7]
  }else {
    x11[3,7] <- x11[2,7]
  }

  rownames(x11) <- c("Predefined","User_modif","Final")
  return(x11)
}

spec_seats <- function(seatspc){
  seats <- seatspc

  for (i in c(1:7)){
    seats[3,i] <- if(!is.na(seats[2,i])) {seats[2,i]} else {seats[1,i]}
  }
  rownames(seats) <- c("Predefined","User_modif","Final")
  return(seats)
}

