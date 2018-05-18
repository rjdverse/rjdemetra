#' @import rJava
#' @importFrom grDevices as.graphicsAnnot  dev.cur  dev.flush dev.hold  dev.interactive  dev.set  devAskNewPage dev.new
#' @importFrom graphics abline axis curve hist layout legend mtext par plot title lines points
#' @importFrom stats frequency  is.ts  terms  ts  ts.union acf  dnorm  pacf  plot.ts  printCoefmat pt  qqnorm  sd is.mts end na.omit start time ts.plot window window<- cycle .preformat.ts
#' @importFrom methods as
#' @importFrom utils capture.output
#'
utils::globalVariables(c("arima.bd.tab", "arima.bp.tab", "arima.bq.tab", "arima.d.tab", "arima.mu.tab", "", "", "arima.p.tab", "arima.q.tab", "automdl.acceptdefault.tab", "automdl.armalimit.tab", "automdl.balanced.tab", "automdl.cancel.tab", "automdl.compare.tab", "automdl.enabled.tab", "automdl.ljungboxlimit.tab", "automdl.mixed.tab", "automdl.reducecv.tab", "automdl.ub1.tab", "automdl.ub2.tab", "automdl.ubfinal.tab", "easter.duration.tab", "easter.enabled.tab", "easter.julian.tab", "easter.test.tab", "easter.type.tab", "estimate.eml.tab", "estimate.span.tab", "estimate.tol.tab", "estimate.urfinal.tab", "outlier.ao.tab", "outlier.cv.tab", "outlier.eml.tab", "outlier.enabled.tab", "outlier.ls.tab", "outlier.method.tab", "outlier.so.tab", "outlier.span.tab", "outlier.tc.tab", "outlier.tcrate.tab", "outlier.usedefcv.tab", "tradingdays.autoadjust.tab", "tradingdays.leapyear.tab", "tradingdays.mauto.tab", "tradingdays.option.tab", "tradingdays.pftd.tab", "tradingdays.stocktd.tab", "tradingdays.test.tab", "transform.adjust.tab", "transform.aicdiff.tab", "transform.fct.tab", "transform.function.tab"))

library("rJava")
.jinit()
.jaddClassPath("./Java/demetra-tstoolkit-2.2.2-SNAPSHOT.jar")
.jaddClassPath("./Java/jdr-2.2.2-SNAPSHOT.jar")

## jd2_rslts.R
proc_data<-function(rslt, name, clobj){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, clobj)
  if (is.null(s))
    return (NULL)
  if (.jinstanceof(s, "ec.tstoolkit.timeseries.simplets.TsData"))
    return(ts_jd2r(.jcast(s,"ec.tstoolkit.timeseries.simplets.TsData")))
  else if (.jinstanceof(s, "ec.tstoolkit.maths.matrices.Matrix"))
    return(matrix_jd2r(.jcast(s,"ec.tstoolkit.maths.matrices.Matrix")))
  else if (.jinstanceof(s, "ec.tstoolkit.information.StatisticalTest"))
    return (test_jd2r(s))
  else if (.jinstanceof(s, "ec.tstoolkit.Parameter")){
    val<-.jcall(s, "D", "getValue")
    e<-.jcall(s, "D", "getStde")
    return (c(val, e))
  }
  else if (.jinstanceof(s, "[Lec.tstoolkit.Parameter;")){
    p<-.jcastToArray(s)
    len<-length(p)
    all<-array(0, dim=c(len,2))
    for (i in 1:len){
      all[i, 1]<-.jcall(p[[i]], "D", "getValue")
      all[i, 2]<-.jcall(p[[i]], "D", "getStde")
    }
    return (all)
  }
  else if (.jcall(.jcall(s, "Ljava/lang/Class;", "getClass"), "Z", "isArray"))
    return (.jevalArray(s, silent=TRUE))
  else if (.jinstanceof(s, "java/lang/Number"))
    return (.jcall(s, "D", "doubleValue"))
  else
    return (.jcall(s, "S", "toString"))
}

proc_parameter<-function(rslt, name, clobj){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, clobj)
  if (is.jnull(s))
    return(NULL)
  val<-.jcall(s, "D", "getValue")
  e<-.jcall(s, "D", "getStde")
  return (c(val, e))
}

proc_parameters<-function(rslt, name, clobj){
  jd_p<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, clobj)
  if (is.jnull(jd_p))
    return(NULL)
  p<-.jcastToArray(jd_p)
  len<-length(p)
  all<-array(0, dim=c(len,3))
  for (i in 1:len){
    n<-.jcall("ec/tstoolkit/Parameter", "Z", "isDefault", .jcast(p[[i]], "ec/tstoolkit/Parameter"))
    if (n){
      all[i, 1]<-NaN
      all[i, 2]<-0
      all[i, 3]<-FALSE
    }else{
      all[i, 1]<-.jcall(p[[i]], "D", "getValue")
      all[i, 2]<-.jcall(p[[i]], "D", "getStde")
      all[i, 3]<-.jcall(p[[i]], "Z", "isFixed")
    }
  }
  return (all)
}

proc_dictionary<-function(name){
  jmapping<-.jcall(name, "Ldemetra/information/InformationMapping;", "getMapping")
  jmap<-.jnew("java/util/LinkedHashMap")
  .jcall(jmapping, "V", "fillDictionary", .jnull("java/lang/String"), .jcast(jmap, "java/util/Map"), TRUE )
  jkeys<-.jcall(jmap, "Ljava/util/Set;", "keySet")
  size<-.jcall(jkeys, "I", "size")
  keys<-array(dim=size)
  jiter<-.jcall(jkeys, "Ljava/util/Iterator;", "iterator")
  for (i in 1:size){
    keys[i]=.jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
  }
  return (keys)
}

matrix_jd2r<-function(s){
  if (is.jnull(s)){
    return (NULL)
  }
  nr<-.jcall(s, "I", "getRowsCount")
  nc<-.jcall(s, "I", "getColumnsCount")
  d<-.jcall(s, "[D", "internalStorage")
  return (array(d, dim=c(nr, nc)))
}

test_jd2r<-function(s){
  if (is.null(s))
    return(NULL)
  desc<-.jfield(s, "S", "description")
  val<-.jfield(s, "D", "value")
  pval<-.jfield(s, "D", "pvalue")
  all<-c(val, pval)
  attr(all, "description")<-desc
  return (all)
}

## jd2_ts.R

ts_jd2r<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  jd_start<-.jcall(s, "Lec/tstoolkit/timeseries/simplets/TsPeriod;", "getStart")
  pstart<-period_jd2r(jd_start)
  x<-.jcall(s, "[D", "internalStorage")
  ts(x,start=pstart[2:3], frequency=pstart[1])
}

ts_r2jd<-function(s){
  freq<-frequency(s)
  start<-start(s)
  jd_freq<-.jcall("ec/tstoolkit/timeseries/simplets/TsFrequency", "Lec/tstoolkit/timeseries/simplets/TsFrequency;", "valueOf", as.integer(freq))
  jd_period<-.jnew("ec/tstoolkit/timeseries/simplets/TsPeriod", jd_freq, as.integer(start[1]), as.integer(start[2]-1))
  ts<-.jnew("ec/tstoolkit/timeseries/simplets/TsData", jd_period, as.double(s), FALSE)
  return (ts)
}

period_r2jd<-function(s){
  freq<-s[1]
  jd_freq<-.jcall("ec/tstoolkit/timeseries/simplets/TsFrequency", "Lec/tstoolkit/timeseries/simplets/TsFrequency;", "valueOf", as.integer(freq))
  .jnew("ec/tstoolkit/timeseries/simplets/TsPeriod", jd_freq, as.integer(s[2]), as.integer(s[3]-1))
}

period_jd2r<-function(jd_p){
  if (is.null(jd_p))
    return (NULL)
  jd_freq<-.jcall(jd_p, "Lec/tstoolkit/timeseries/simplets/TsFrequency;", "getFrequency")
  frequency<-.jcall(jd_freq, "I", "intValue")
  year<-.jcall(jd_p, "I", "getYear")
  position<-.jcall(jd_p, "I", "getPosition")
  c(frequency, year, position+1)
}


parameters_r2jd<-function(params, fixed=NULL){
  if (is.null(fixed))
    return(.jcall("jdr/spec/ts/Utility", "[Lec/tstoolkit/Parameter;", "parameters", .jarray(params), evalArray = FALSE))
  else
    return(.jcall("jdr/spec/ts/Utility", "[Lec/tstoolkit/Parameter;", "parameters", .jarray(params), .jarray(fixed), evalArray = FALSE))
}

parameters_jd2r<-function(jparams){
  if (is.jnull(jparams))
    return(NULL)
  p<-.jcastToArray(jparams)
  len<-length(p)
  all<-array(0, dim=c(len,3))
  for (i in 1:len){
    n<-.jcall("ec/tstoolkit/Parameter", "Z", "isDefault", .jcast(p[[i]], "ec/tstoolkit/Parameter"))
    if (n){
      all[i, 1]<-NaN
      all[i, 2]<-0
      all[i, 3]<-FALSE
    }else{
      all[i, 1]<-.jcall(p[[i]], "D", "getValue")
      all[i, 2]<-.jcall(p[[i]], "D", "getStde")
      all[i, 3]<-.jcall(p[[i]], "Z", "isFixed")
    }
  }
  return (all)
}

## jd2_proceresults.R
setGeneric(name="result", def = function(object, id, ... ){standardGeneric("result")})

setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})


setClass(
  Class="JD2_ProcResults",
  representation = representation(internal = "jobjRef" )
)

setMethod("dictionary", "JD2_ProcResults", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_dictionary(object@internal)
  }

})

setMethod("result", signature = c(object="JD2_ProcResults", id="character"), function(object, id){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_data(object@internal, id, rjdemetra_java$clobject)}
})

rjdemetra_java <- new.env(parent = emptyenv())
rjdemetra_java$clobject <- NULL

