#' @rdname tramoseats
#' @name tramoseats
#' @export
jtramoseats <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                       userdefined = NULL){
  if (!is.ts(series)) {
    stop("The series must be a time series")
  }
  UseMethod("jtramoseats", spec)
}
#' @export
jtramoseats.SA_spec <- function(series, spec,
                               userdefined = NULL){

  if (!inherits(spec, "TRAMO_SEATS"))
    stop("use only with c(\"SA_spec\",\"TRAMO_SEATS\") class object")
  # To create the Java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- spec_TRAMO_r2jd(spec,jrspec)
  specSeats_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary )
  jrslt <- new (Class = "TramoSeats_java", internal = jrslt)
  jSA(result = jrslt, spec = jrspec, dictionary = jdictionary)

}
#' @export
jtramoseats.character <- function(series, spec = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
                                 userdefined = NULL){
  spec <- match.arg(spec)
  # To create the Java objects
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", spec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
  jrslt <- .jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats", ts_r2jd(series), jspec, jdictionary)
  jrslt <- new (Class = "TramoSeats_java", internal = jrslt)
  jSA(result = jrslt, spec = jrspec, dictionary = jdictionary)
}

