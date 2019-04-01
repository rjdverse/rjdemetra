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
#' \code{get_dictionary} returns the indicators that can be extracted from \code{jSA} and \code{get_indicators} returns a list of indicators.
#' \code{jSA} constructs a object of class \code{jSA}.
#'
#' @param x a \code{"jSA"} object.
#' @param ... characters containing the names of the indicators to extract.
#'
#' @details
#' A \code{jSA} object is a list with three elements: \itemize{
#' \item \code{"result"}: the Java object with the results of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"spec"}: the Java object with the specification of a seasonal adjustment or a pre-adjustment method.
#' \item \code{"dictionary"}: the Java object with dictionnary of a seasonal adjustment or a pre-adjustment method. In particular, it contains all the user-defined regressors.
#'  }
#'
#' \code{get_dictionary} returns the list of indicators that can be extracted from a \code{jSA} object by the function \code{get_indicators}.
#'
#' @return \code{get_dictionary} a vector of characters and \code{get_indicators} returns a list containing the indicators that are extracted.
#'
#' @examples
#' myseries <- ipi_c_eu[, "FR"]
#' mysa <- jx13(myseries, spec = "RSA5c")
#' get_dictionary(mysa)
#'
#' get_indicators(mysa, "decomposition.b2", "decomposition.d10")
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
