#' @rdname get_model
#' @name get_model
#' @export
get_jmodel <- function(x, workspace,
                      userdefined = NULL,
                      progress_bar = TRUE){
  UseMethod("get_jmodel", x)
}
#' @export
get_jmodel.workspace <- function(x, workspace,
                                userdefined = NULL,
                                progress_bar = TRUE){
  multiprocessings <- get_all_objects(x)
  nb_mp <- length(multiprocessings)

  result <- lapply(seq_len(nb_mp), function(i){
    if (progress_bar)
      cat(sprintf("Multiprocessing %i on %i:\n", i, nb_mp))
    get_jmodel(multiprocessings[[i]],
              workspace = x, userdefined = userdefined,
              progress_bar = progress_bar)
  })
  names(result) <- names(multiprocessings)
  result

}
#' @export
get_jmodel.multiprocessing <- function(x, workspace,
                                      userdefined = NULL,
                                      progress_bar = TRUE){
  all_sa_objects <- get_all_objects(x)
  nb_sa_objs <- length(all_sa_objects)

  if (progress_bar)
    pb <- txtProgressBar(min = 0, max = nb_sa_objs, style = 3)

  result <- lapply(seq_len(nb_sa_objs), function(i){
    res <- get_jmodel(all_sa_objects[[i]],
                     workspace = workspace, userdefined = userdefined)
    if (progress_bar)
      setTxtProgressBar(pb, i)
    res
  })
  names(result) <- names(all_sa_objects)
  if (progress_bar)
    close(pb)
  result
}
#' @export
get_jmodel.sa_item <- function(x, workspace,
                              userdefined = NULL,
                              progress_bar = TRUE){

  jspec <- get_jspec(x)
  jresult <- sa_results(x)
  if(is.null(jresult))
    return(NULL)
  if (.jinstanceof(jspec, "jdr/spec/x13/X13Spec")) {
    jresult <- new(Class = "X13_java", internal = jresult)
  }else{
    jresult <- new(Class = "TramoSeats_java", internal = jresult)
  }
  dictionary <- .jcall(workspace, "Ljdr/spec/ts/Utility$Dictionary;", "dictionary")
  jSA(result = jresult, spec = jspec, dictionary = dictionary)
}
