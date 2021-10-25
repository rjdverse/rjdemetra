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
#' \code{get_dictionary} returns the indicators that can be extracted from \code{"jSA"} objects,
#' \code{get_indicators} extracts a list of indicators
#' \code{jSA2R} returns the corresponding \code{"SA"}.
#'
#' @param x a \code{"jSA"} object.
#' @param ... characters containing the names of the indicators to extract.
#' @param userdefined a userdefined vector containing the names of additional output variables (see \code{\link{user_defined_variables}}).
#' Only used for \code{"SA"} objects.
#'
#' @details
#' A \code{"jSA"} object is a list of three elements:
#' \itemize{
#' \item \code{"result"}: the Java object containing the results of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"spec"}: the Java object containing the specification of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"dictionary"}: the Java object containing the dictionary of a seasonal adjustment or a pre-adjustment method.
#' In particular, it contains all the user-defined regressors.
#'  }
#'
#' \code{get_dictionary} returns the list of indicators that can be extracted from a \code{jSA} object by the function \code{get_indicators}.
#'
#' \code{jSA2R} returns the corresponding formatted seasonally adjusted (\code{"SA"} object) or RegARIMA (\code{"regarima"} object) model.
#'
#' \code{get_jspec} returns the Java object that contains the specification of an object. Such object can be of type
#' \code{"jSA"}, \code{"X13"}, \code{"TRAMO_SEATS"} or \code{"sa_item"}.
#'
#' @return \code{get_dictionary} returns a vector of characters,
#' \code{get_indicators} returns a list containing the indicators that are extracted,
#' \code{jSA2R} returns a \code{"SA"} or a \code{"regarima"} object and
#' \code{get_jspec} returns a Java object.
#'
#' @examples
#' myseries <- ipi_c_eu[, "FR"]
#' mysa <- jx13(myseries, spec = "RSA5c")
#' get_dictionary(mysa)
#'
#' get_indicators(mysa, "decomposition.b2", "decomposition.d10")
#'
#' # To convert the Java object to an R object
#' jSA2R(mysa)
#'
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
                             extra_info = TRUE, freq = frequency(y_ts))
  }else{
    if(.jinstanceof(jspec, "jdr/spec/tramoseats/TramoSpec")){
      # TRAMOSEATS-RegARIMA object
      model <- regarima_defTS(jrobj = x[["result"]], spec = jspec,
                              context_dictionary = context_dictionary,
                              extra_info = TRUE, freq = frequency(y_ts))
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
                          extra_info = extra_info, freq = freq)
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
