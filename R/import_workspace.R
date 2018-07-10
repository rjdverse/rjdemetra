setClass("multiprocessing", contains = "jobjRef")
setClass("sa_item", contains = "jobjRef")
setClass("workspace", contains = "jobjRef")


#' @export
# Open a workspace
load_workspace <- function(file){
  if(missing(file) || is.null(file)){
    if(Sys.info()[['sysname']] == "Windows"){
      file <- utils::choose.files(caption = "Select a workspace",
                                  filters = c("JDemetra+ workspace (.xml)","*.xml"))
    }else{
      file <- base::file.choose()
    }
    if(length(file) == 0)
      stop("You have to choose a file !")
  }
  if(!file.exists(file)|length(grep("\\.xml$",file))==0)
    stop("The file doesn't exist or isn't a .xml file !")

  workspace <- .jcall("ec/tstoolkit/jdr/ws/Workspace", "Lec/tstoolkit/jdr/ws/Workspace;", "open", file)
  workspace <- new("workspace", workspace)
  return(workspace)
}

#' @export
count <- function(x, ...){
  UseMethod("count", x)
}
#' @export
count.multiprocessing <- function(x, ...){
  return(.jcall(x, "I", "getMultiProcessingCount"))
}
#' @export
count.workspace <- function(x, ...){
  return(.jcall(x, "I", "size"))
}


#' @export
# Get the given processing (from 1 to n )
get_object <- function(x, pos = 1, ...){
  UseMethod("get_object", x)
}
#' @export
get_object.multiprocessing <- function(x, pos = 1, ...){
  sa_item_obj <- .jcall(x, "Lec/tstoolkit/jdr/ws/SaItem;", "get", as.integer(pos - 1))
  sa_item_obj <- new("sa_item", sa_item_obj)
  return(sa_item_obj)
}
#' @export
get_object.workspace <- function(x, pos = 1, ...){
  multiproc <- .jcall(x, "Lec/tstoolkit/jdr/ws/MultiProcessing;", "getMultiProcessing", as.integer(pos - 1))
  multiproc <- new("multiprocessing", multiproc)
  return(multiproc)
}

#' @export
get_all_objects <- function(x, ...){
  UseMethod("get_all_objects", x)
}
#' @export
get_all_objects.multiprocessing <- function(x, ...){
  nb_sa_objects <- count(x)
  all_sa_object <- lapply(seq_len(nb_sa_objects),
                          function(i) {
                            get_object(x, i)
                          })
  names(all_sa_object) <- sapply(all_sa_object, get_name)
  all_sa_object
}
#' @export
get_all_objects.workspace <- function(x, ...){
  nb_multiprocessing <- count(x)
  all_multiprocessings <- lapply(seq_len(nb_multiprocessing),
                                 function(i) {
                                   get_object(x, i)
                                 })
  names(all_multiprocessings) <- sapply(all_multiprocessings, get_name)
  all_multiprocessings
}

#' @export
get_name <- function(x, ...){
  UseMethod("get_name", x)
}
#' @export
get_name.multiprocessing <- function(x, ...){
  return(.jcall(x, "S", "getName"))
}
#' @export
get_name.sa_item <- function(x, ...){
  jt <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ldemetra/datatypes/Ts;", "getTs")
  name <- .jcall(jts, "S", "getName")
  # Remove the name of the file link to the saitem
  name <- gsub("^.*\\n", "", name)
  return(name)
}

# Get the ts (java) of an saitem
get_ts <- function(x, ...){
  UseMethod("get_ts", x)
}
get_ts.workspace <- function(x, ...){
  multiprocessings <- get_all_objects(x)
  lapply(multiprocessings, get_ts)
}
get_ts.multiprocessing <- function(x, ...){
  all_sa_objects <- get_all_objects(x)
  sa <- do.call(ts.union, lapply(all_sa_objects, get_ts))
  if(is.mts(sa)){
    colnames(sa) <- names(all_sa_objects)
  }
  sa
}
get_ts.sa_item <- function(x, ...){
  jt <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ldemetra/datatypes/Ts;", "getTs")
  j_ts_series <- .jcall(jts, "Lec/tstoolkit/timeseries/simplets/TsData;", "getData")
  return(ts_jd2r(j_ts_series))
}



#' @export
# Compute all the multi-processing or a given one
compute <- function(workspace, name = NULL) {
  if (is.null(name)) {
    .jcall(workspace, "V", "computeAll")
  } else {
    .jcall(workspace, "V", "compute", name)
  }
  invisible()
}

#' @export
get_model <- function(x, userdefined = NULL){
  UseMethod("get_model", x)
}
get_model.workspace <- function(x, userdefined = NULL){
  multiprocessings <- get_all_objects(x)
  lapply(multiprocessings, get_model, userdefined = userdefined)
}
get_model.multiprocessing <- function(x, userdefined = NULL){
  all_sa_objects <- get_all_objects(x)
  lapply(all_sa_objects, get_model, userdefined = userdefined)
}
get_model.sa_item <- function(x, userdefined = NULL){
  jspec <- get_jspec(x = x)
  jresult <- sa_results(x)
  sa_jd2r(jrslt = jresult, spec = jspec, userdefined = userdefined)
}

# Get the results of an saitem
sa_results <- function(jsa) {
  jresult <- .jcall(jsa, "Ldemetra/algorithm/IProcResults;", "getResults")
  if(is.null(jresult))
    warning("The result of the object is NULL : have you compute the workspace importing?\n",
            "See ?compute for more information")
  return(jresult)
}

# Get the specification of an saitem (possible type: Domain, Estimation, Point)
sa_spec <- function(jsa, type = "Domain") {
  jt <- .jcall(jsa, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  if (type == "Domain") {
    return(.jcall(jt, "Lec/satoolkit/ISaSpecification;", "getDomainSpec"))
  }
  if (type == "Estimation") {
    return(.jcall(jt, "Lec/satoolkit/ISaSpecification;", "getEstimationSpec"))
  }
  if (type == "Point") {
    return(.jcall(jt, "Lec/satoolkit/ISaSpecification;", "getPointSpec"))
  }
  return(NULL)
}
