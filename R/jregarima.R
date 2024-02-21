#' @rdname regarima
#' @name regarima
#' @export
jregarima <- function(series, spec = NA){
  UseMethod("jregarima", spec)
}
#' @export
jregarima.X13 <- function(series, spec = NA){
  if (!is.ts(series))
    stop("series must be a time series")
  spec <- regarima_spec_x13(spec)

  # To create the Java objects
  jrspec <- .jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", "RG1")
  # To introduce modifications to the spec and create the java dictionary with the user-defined variables
  jdictionary <- spec_regarima_X13_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;", "getCore")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "x12",
                  ts_r2jd(series), jspec, jdictionary)
  jrobct <- new(Class = "RegArima_java", internal = jrslt)

  jSA(result = jrobct, spec = jrspec, dictionary = jdictionary)
}
#' @export
jregarima.TRAMO_SEATS <- function(series, spec = NA){
  if (!is.ts(series))
    stop("The series must be a time series")

  spec <- regarima_spec_tramoseats(spec)

  # create the java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", "TR1")
  # introduce modifications from the spec and create the java dictionary with the user-defined variables
  jdictionary <- spec_TRAMO_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;",
                  "getCore")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "tramo",
                  ts_r2jd(series),
                  jspec, jdictionary)
  jrobct <- new(Class = "TRAMO_java", internal = jrslt)

  jSA(result = jrobct, spec = jrspec, dictionary = jdictionary)
}

#' @rdname regarima
#' @name regarima
#' @export
jregarima_tramoseats <- function(series, spec = c("TRfull", "TR0", "TR1", "TR2", "TR3", "TR4", "TR5")){
  if (!is.ts(series)) {
    stop("The series must be a time series")
  }
  spec <- match.arg(spec)

  # create the java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSpec", "Ljdr/spec/tramoseats/TramoSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "tramo",
                  ts_r2jd(series),
                  jspec, jdictionary)
  jrobct <- new(Class = "TRAMO_java", internal = jrslt)

  jSA(result = jrobct, spec = jrspec, dictionary = jdictionary)
}

#' @rdname regarima
#' @name regarima
#' @export
jregarima_x13 <- function(series, spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c")){
  if (!is.ts(series)) {
    stop("The series must be a time series")
  }
  spec <- match.arg(spec)

  # To create the Java objects
  jrspec <- .jcall("jdr/spec/x13/RegArimaSpec", "Ljdr/spec/x13/RegArimaSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/regarima/Processor",
                  "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "x12",
                  ts_r2jd(series), jspec, jdictionary)

  jrobct <- new(Class = "RegArima_java", internal = jrslt)

  jSA(result = jrobct, spec = jrspec, dictionary = jdictionary)
}
