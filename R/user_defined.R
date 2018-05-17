
user_defined <- function(namedvector,jd_clobj, jrobct){

  result <- lapply(namedvector, function(x) result(jd_clobj,jrobct,x))
  if (is.null(names(namedvector)))
    names(result) <- namedvector

  class(result) <-c("user_defined")
  result
}
