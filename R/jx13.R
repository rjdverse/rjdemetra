#' @rdname x13
#' @name x13
#' @export
jx13 <- function(series, spec = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"),
                userdefined = NULL){
  if (!is.ts(series)) {
    stop("series must be a time series")
  }
  UseMethod("jx13", spec)
}
#' @export
jx13.SA_spec <- function(series, spec, userdefined = NULL){
  if (!inherits(spec, "X13"))
    stop("use only with c(\"SA_spec\",\"X13\") class object")

  # create the java objects
  if (is.null(s_estimate(spec))) {
    # For X-11 specification
    jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "X11")
  } else {
    jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  }
  jdictionary <- spec_regarima_X13_r2jd(spec, jrspec)
  seasma <- specX11_r2jd(spec, jrspec, freq = frequency(series))
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)
  jrslt <- new(Class = "X13_java", internal = jrslt)
  jSA(result = jrslt, spec = jrspec, dictionary = jdictionary)
}
#' @export
jx13.character <- function(series, spec = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"),
                          userdefined = NULL){
  spec <- match.arg(spec)
  # create the java objects
  jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13", ts_r2jd(series), jspec, jdictionary)
  jrslt <- new(Class = "X13_java", internal = jrslt)
  jSA(result = jrslt, spec = jrspec, dictionary = jdictionary)
}
