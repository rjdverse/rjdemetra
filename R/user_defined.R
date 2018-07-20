
user_defined <- function(namedvector, jrobct){
  if(is.null(namedvector)){
    result <- list()
  }else{
    result <- lapply(namedvector, function(x) result(jrobct, x))
    if (is.null(names(namedvector)))
      names(result) <- namedvector
  }
  class(result) <- c("user_defined")
  result
}
