jSA <- function(result = NULL, spec = NULL, dictionary = NULL){
  jsa_object <- list(result = result, spec = spec, dictionary = dictionary)
  class(jsa_object) <- "jSA"
  jsa_object
}
is.jSA <- function(x){
  inherits(x, "jSA")
}

#' Functions around 'jSA' objects
#'
#' \code{get_dictionary} returns the indicators that can be extracted from \code{"jSA"} objects, \code{get_indicators} extract a list of indicators and \code{"jSA2R"} returns returns the corresponding \code{"SA"} or a \code{"regarima"} object.
#'
#' @param x a \code{"jSA"} object.
#' @param ... characters containing the names of the indicators to extract.
#' @param userdefined userdefined vector with characters for additional output variables (see \code{\link{user_defined_variables}}). Only used for \code{"SA"} objects.
#'
#' @details
#' A \code{"jSA"} object is a list with three elements: \itemize{
#' \item \code{"result"}: the Java object with the results of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"spec"}: the Java object with the specification of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"dictionary"}: the Java object with dictionnary of a seasonal adjustment or a pre-adjustment method. In particular, it contains all the user-defined regressors.
#'  }
#'
#' \code{get_dictionary} returns the list of indicators that can be extracted from a \code{jSA} object by the function \code{get_indicators}.
#'
#' \code{jSA2R} returns the corresponding formatted seasonal adjustment (\code{"SA"} object) or RegARIMA (\code{"regarima"} object) model.
#'
#' @return \code{get_dictionary} a vector of characters, \code{get_indicators} returns a list containing the indicators that are extracted and \code{jSA2R} returns a \code{"SA"} or a \code{"regarima"} object.
#'
#' @examples
#' myseries <- ipi_c_eu[, "FR"]
#' mysa <- jx13(myseries, spec = "RSA5c")
#' get_dictionary(mysa)
#'
#' get_indicators(mysa, "decomposition.b2", "decomposition.d10")
#'
#' # To convert to the R object
#' jSA2R(mysa)
#' @rdname jSA
#' @name jSA
#' @export
get_dictionary <- function(x){
  if(!is.jSA(x))
    stop("x must be a jSA object!")
  jresult <- x[["result"]]
  if(is.null(jresult))
    return(NULL)
  dictionary(jresult)
}
#' @rdname jSA
#' @name jSA
#' @export
get_indicators <- function(x, ...){
  if(!is.jSA(x))
    stop("x must be a jSA object!")
  jresult <- x[["result"]]
  if(is.null(jresult))
    return(NULL)
  list_indicators <- c(...)
  if(!is.character(list_indicators))
    stop("The indicators must be a vector of characters!")
  indicators <- lapply(list_indicators, function(id){
    result(jresult, id)
  })
  names(indicators) <- list_indicators
  indicators
}
#' @rdname jSA
#' @name jSA
#' @export
jSA2R <- function(x, userdefined = NULL){
  if(!is.jSA(x))
    stop("x must be a jSA object!")

  jresult <- x[["result"]]@internal
  jspec <- x[["spec"]]
  dictionary <- x[["dictionary"]]
  context_dictionary <- dictionary$toContext()
  if(is.null(jresult))
    return(NULL)

  if(.jinstanceof(jspec,"jdr/spec/x13/RegArimaSpec")){
    # X13-RegARIMA object
    model <- regarima_defX13(jrobj = x[["result"]], spec = jspec,
                             context_dictionary = context_dictionary,
                             extra_info = TRUE)
  }else{
    if(.jinstanceof(jspec, "jdr/spec/tramoseats/TramoSpec")){
      # TRAMOSEATS-RegARIMA object
      model <- regarima_defTS(jrobj = x[["result"]], spec = jspec,
                              context_dictionary = context_dictionary,
                              extra_info = TRUE)
    }else{
      y_ts <- result(x[["result"]], "y")
      # SA object
      model <- sa_jd2r(jrslt = jresult, spec = jspec, userdefined = userdefined,
                       context_dictionary = context_dictionary,
                       extra_info = TRUE, freq = frequency(y_ts))
    }
  }
  model
}



sa_jd2r <- function(jrslt, spec,
                    userdefined = NULL,
                    context_dictionary = NULL,
                    extra_info = FALSE, freq = NA){
  if (is.null(jrslt))
    return(NULL)

  if (.jinstanceof(spec, "jdr/spec/tramoseats/TramoSeatsSpec")) {
    tramoseatsJavaResults(jrslt = jrslt, spec = spec, userdefined = userdefined,
                          context_dictionary = context_dictionary,
                          extra_info = extra_info)
  }else{
    if (.jinstanceof(spec, "jdr/spec/x13/X13Spec")) {
      x13JavaResults(jrslt = jrslt, spec = spec, userdefined = userdefined,
                     context_dictionary = context_dictionary,
                     extra_info = extra_info, freq = freq)
    }else{
      stop("Wrong spec argument")
    }

  }
}
