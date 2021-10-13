# To extract a jspec from a R SA object or from a sa_item

#' @rdname jSA
#' @name jSA
#' @export
get_jspec <- function(x, ...){
  UseMethod("get_jspec", x)
}
#' @export
get_jspec.X13 <- function(x, ...){
  spec <- x13_spec(x, ...)
  if (is.null(s_estimate(spec))) {
    # For X-11 specification
    jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "X11")
  } else {
    jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  }
  jdictionary <- spec_regarima_X13_r2jd(spec,jrspec)
  seasma <- specX11_r2jd(spec,jrspec, freq = frequency(x$final$series))
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jspec
}
#' @export
get_jspec.TRAMO_SEATS <- function(x, ...){
  spec <- tramoseats_spec(x, ...)
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- spec_TRAMO_r2jd(spec,jrspec)
  spec_seats <- specSeats_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jspec
}
#' @export
get_jspec.sa_item <- function(x, ...){
  spec <- sa_spec(x)
  if (.jinstanceof(spec, "ec/satoolkit/tramoseats/TramoSeatsSpecification")) {
    spec <- .jcast(spec, "ec/satoolkit/tramoseats/TramoSeatsSpecification")
    spec <- .jnew("jdr/spec/tramoseats/TramoSeatsSpec",spec)
  }else{
    if (.jinstanceof(spec, "ec/satoolkit/x13/X13Specification")) {
      spec <- .jcast(spec, "ec/satoolkit/x13/X13Specification")
      spec <- .jnew("jdr/spec/x13/X13Spec", spec)
    }else{
      stop("Error loading the specification")
    }
  }
  spec
}
#' @export
get_jspec.jSA <- function(x, ...){
  x$spec$getCore()
}
