setClass("multiprocessing", contains = "jobjRef")
setClass("sa_item", contains = "jobjRef")
setClass("workspace", contains = "jobjRef")

is.multiprocessing <- function(x){
    inherits(x, "multiprocessing")
}
is.sa_item <- function(x){
    inherits(x, "sa_item")
}
is.workspace <- function(x){
    inherits(x, "workspace")
}

#' Load a JDemetra+ workpace
#'
#' Function to load a JDemetra+ workspace.
#'
#' @param file the path to the JDemetra+ workspace to load.
#' By default a dialog box opens.
#'
#' @return An object of class \code{"workspace"}.
#'
#' @seealso \code{\link{save_workspace}}, \code{\link{get_model}}
#'
#' @export
load_workspace <- function(file){
  if (missing(file) || is.null(file)) {
    if (Sys.info()[['sysname']] == "Windows") {
      file <- utils::choose.files(caption = "Select a workspace",
                                  filters = c("JDemetra+ workspace (.xml)", "*.xml"))
    }else{
      file <- base::file.choose()
    }
    if (length(file) == 0)
      stop("You have to choose a file !")
  }
  if (!file.exists(file) | length(grep("\\.xml$",file)) == 0)
    stop("The file doesn't exist or isn't a .xml file !")

  workspace <- .jcall("ec/tstoolkit/jdr/ws/Workspace", "Lec/tstoolkit/jdr/ws/Workspace;", "open", file)
  workspace <- new("workspace", workspace)
  return(workspace)
}


#' Get objects inside a workspace or multiprocessing
#'
#' Generics functions to get all (\code{get_all_objects()}) \code{multiprocessing} (respectively \code{sa_item})
#' from a \code{workspace} (respectively \code{multiprocessing})  or to get a given one (\code{get_object()}) .
#'
#' @param x the object where to extract the \code{multiprocessing} or the \code{sa_item}.
#' @param pos the index of the object to extract.
#'
#' @return An object of class \code{multiprocessing} or \code{sa_item} (for \code{get_object()}) or a list
#' of objects of class \code{multiprocessing} or \code{sa_item} (for \code{get_all_objects()}).
#'
#' @family functions to get informations from a workspace, multiprocessing or sa_item
#'
#' @examples
#' \dontrun{
#' sa_x13 <- x13_def(ipi_c_eu[, "FR"], spec = "RSA5c")
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#'
#' # Other way to get the multiprocessing:
#' mp <- get_object(wk, 1)
#' # To get the sa_item object:
#' sa_item <- get_object(mp, 1)
#' }
#' @name get_object
#' @rdname get_object
#' @export
get_object <- function(x, pos = 1){
  UseMethod("get_object", x)
}
#' @export
get_object.workspace <- function(x, pos = 1){
  multiproc <- .jcall(x, "Lec/tstoolkit/jdr/ws/MultiProcessing;", "getMultiProcessing", as.integer(pos - 1))
  multiproc <- new("multiprocessing", multiproc)
  return(multiproc)
}
#' @export
get_object.multiprocessing <- function(x, pos = 1){
  sa_item_obj <- .jcall(x, "Lec/tstoolkit/jdr/ws/SaItem;", "get", as.integer(pos - 1))
  sa_item_obj <- new("sa_item", sa_item_obj)
  return(sa_item_obj)
}
#' @name get_object
#' @rdname get_object
#' @export
get_all_objects <- function(x){
  UseMethod("get_all_objects", x)
}
#' @export
get_all_objects.multiprocessing <- function(x){
  nb_sa_objects <- count(x)
  all_sa_object <- lapply(seq_len(nb_sa_objects),
                          function(i) {
                            get_object(x, i)
                          })
  names(all_sa_object) <- sapply(all_sa_object, get_name)
  all_sa_object
}
#' @export
get_all_objects.workspace <- function(x){
  nb_multiprocessing <- count(x)
  all_multiprocessings <- lapply(seq_len(nb_multiprocessing),
                                 function(i) {
                                   get_object(x, i)
                                 })
  names(all_multiprocessings) <- sapply(all_multiprocessings, get_name)
  all_multiprocessings
}

#' Get the Java name of a multiprocessing or a sa_item
#'
#' Generics functions to get the Java name of a \code{multiprocessing} or a \code{sa_item}.
#'
#' @param x the object to get the name from.
#'
#' @return A \code{character}.
#'
#' @family functions to get informations from a workspace, multiprocessing or sa_item
#'
#' @examples \dontrun{
#' spec_x13 <- x13_spec_def(spec = c("RSA5c"), easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec_def(spec = c("RSA5"))
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
#'
#' sa_item1 <- get_object(mp, 1)
#' sa_item2 <- get_object(mp, 2)
#'
#' get_name(sa_item1) # returns "X13"
#' get_name(sa_item2) # returns "TramoSeats"
#'
#' get_name(mp) # returns "sa1"
#'
#' # To get all the name of the sa_items inside a multiprocessing:
#' sapply(get_all_objects(mp), get_name)
#'
#' # To get all the name of the multiprocessings inside a workspace:
#' sapply(get_all_objects(wk), get_name)
#'
#' # To get all the name of the sa_items inside a workspace:
#' lapply(get_all_objects(wk),function(mp){
#'   sapply(get_all_objects(mp), get_name)
#' })
#' }
#' @export
get_name <- function(x){
  UseMethod("get_name", x)
}
#' @export
get_name.multiprocessing <- function(x){
  return(.jcall(x, "S", "getName"))
}
#' @export
get_name.sa_item <- function(x){
  jt <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ldemetra/datatypes/Ts;", "getTs")
  name <- .jcall(jts, "S", "getName")
  # Remove the name of the file link to the saitem
  name <- gsub("^.*\\n", "", name)
  return(name)
}

#' Count the number of objects inside a workspace or multiprocessing
#'
#' Generics functions to count the number of \code{multiprocessing} (respectively \code{sa_item})
#' inside a \code{workspace} (respectively \code{multiprocessing}).
#'
#' @param x the \code{workspace} or the code{multiprocessing}.
#'
#' @family functions to get informations from a workspace, multiprocessing or sa_item
#'
#' @examples
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' count(wk) # 1 multiprocessing inside the workspace wk
#' count(mp) # 0 sa_item inside the multiprocessing mp
#' 
#'
#' @export
count <- function(x){
  UseMethod("count", x)
}
#' @export
count.multiprocessing <- function(x){
  return(.jcall(x, "I", "size"))
}
#' @export
count.workspace <- function(x){
  return(.jcall(x, "I", "getMultiProcessingCount"))
}

#' Get the input raw time series
#'
#' Generics functions to get the input raw time series of a \code{workspace}, \code{multiprocessing},
#' \code{sa_item} or \code{SA} object.
#'
#' @param x the object where to get the time series.
#'
#' @return \code{get_ts()} returns a \code{\link[stats]{ts}} object or list of \code{\link[stats]{ts}} objects:
#' \itemize{
#'  \item if \code{x} is a \code{sa_item} or a \code{SA} object, \code{get_ts(x)} returns a single \code{ts} object;
#'  \item if \code{x} is a \code{multiprocessing} object, \code{get_ts(x)} returns list of length the number
#'  of sa_items, each a \code{ts} object;
#'  \item if \code{x} is a \code{workspace} object, \code{get_ts(x)} returns list of length the number of multiprocessing,
#'  each element containing a list of \code{ts} object.
#'}
#' @family functions to get informations from a workspace, multiprocessing or sa_item
#'
#' @examples \dontrun{
#' sa_x13 <- x13_def(ipi_c_eu[, "FR"], spec = "RSA5c")
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' sa_item <- get_object(mp, 1)
#' 
#'   # Extracting from a SA:
#' get_ts(sa_x13) # Returns the ts object ipi_c_eu[, "FR"]
#' 
#'   # Extracting from a sa_item:
#' get_ts(sa_item) # Returns the ts object ipi_c_eu[, "FR"]
#'
#'   # Extracting from a multiprocessing:
#' # Returns a list of length 1 named "X13" containing the ts object ipi_c_eu[, "FR"]:
#' get_ts(mp)
#'
#'
#'   # Extracting from a workspace:
#' # Returns a list of length 1 named "sa1" containing a list
#' # of length 1 named "X13" containing the ts object ipi_c_eu[, "FR"]
#' get_ts(wk)
#' }
#' @export
get_ts <- function(x){
  UseMethod("get_ts", x)
}
#' @export
get_ts.workspace <- function(x){
  multiprocessings <- get_all_objects(x)
  lapply(multiprocessings, get_ts)
}
#' @export
get_ts.multiprocessing <- function(x){
  all_sa_objects <- get_all_objects(x)
  lapply(all_sa_objects, get_ts)
}
#' @export
get_ts.sa_item <- function(x){
  jt <- .jcall(x, "Ldemetra/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ldemetra/datatypes/Ts;", "getTs")
  j_ts_series <- .jcall(jts, "Lec/tstoolkit/timeseries/simplets/TsData;", "getData")
  return(ts_jd2r(j_ts_series))
}
#' @export
get_ts.SA <- function(x){
  return(x$final$series[,"y"])
}

#' Compute the multi-processing from a workspace
#'
#' Function to compute all the multi-processings or a given one from a workspace.
#' By default the workspace only contains definitions: computation is needed to get the seasonal adjustment model
#' (with \code{\link{get_model}}).
#'
#' @param workspace the workspace to compute.
#' @param i a \code{character} or \code{numeric} indicating the name or the index of the multiprocessing to compute.
#' By default all the multi-processings are compute.
#'
#' @seealso \code{\link{get_model}}
#'
#' @examples 
#' spec_x13 <- x13_spec_def(spec = c("RSA5c"), easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' sa_item1 <- get_object(mp, 1)
#'
#' get_model(sa_item1, wk) # Returns NULL
#'
#' compute(wk)
#'
#' get_model(sa_item1, wk) # Returns the SA model sa_x13
#' 
#'
#'
#' @export
compute <- function(workspace, i) {

  if (missing(i)) {
    return(.jcall(workspace, "V", "computeAll"))
  }
  if (is.numeric(i)) {
    nb_mp_objects <- count(workspace)
    mp_names <- sapply(seq_len(nb_mp_objects),
                       function(i) {
                         get_name(get_object(workspace, i))
                       })
    if (i < 1 || i > nb_mp_objects)
      stop("The index ",i," is incorrect !\n",
           "It must be beetween 1 and ", nb_mp_objects)
    i <- mp_names[i]
  }
  if (!is.character(i))
    stop("The parameter i must be a character or a numeric")

  .jcall(workspace, "V", "compute", i)
}


#' Get the seasonally adjusted model from a workspace
#'
#' Generics functions to get seasonally adjusted model(s) from \code{workspace},
#' \code{multiprocessing} or \code{sa_item} object.
#'
#' @param x the object to get the seasonnaly adjusted model.
#' @param workspace the workspace object where models are stored. If \code{x} is a \code{workspace} object this parameter is not used.
#' @param userdefined vector with characters for additional output variables.
#' (see \code{\link{x13}} or \code{\link{tramoseats}}).
#' @param progress_bar boolean: if \code{TRUE} a progress bar is printed.
#'
#' @return \code{get_model()} returns a seasonnaly adjust object (class \code{c("SA", "X13")} or \code{c("SA", "TRAMO_SEATS"}) or list of seasonnaly adjust objects:
#' \itemize{
#'  \item if \code{x} is a \code{sa_item} object, \code{get_model(x)} returns a \code{"SA"} object;
#'  \item if \code{x} is a \code{multiprocessing} object, \code{get_ts(x)} returns list of length the number
#'  of sa_items, each element containing a \code{"SA"} object;
#'  \item if \code{x} is a \code{workspace} object, \code{get_ts(x)} returns list of length the number of multiprocessing,
#'  each element containing a list of a \code{"SA"} object.
#'}
#' @family functions to get informations from a workspace, multiprocessing or sa_item
#' @seealso \code{\link{compute}}
#'
#' @examples\dontrun{
#' spec_x13 <- x13_spec_def(spec = c("RSA5c"), easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec_def(spec = c("RSA5"))
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
#'
#' compute(wk) # It's important to compute the workspace to get the SA model
#' sa_item1 <- get_object(mp, 1)
#'
#' get_model(sa_item1, wk) # Extract the model of the sa_item1: its the object sa_x13
#' 
#' # To get all the models of the multiprocessing mp:
#' get_model(mp, wk)
#'
#' # To get all the models of the workspace wk:
#' get_model(wk)
#' }
#'
#' @export
get_model <- function(x, workspace,
                      userdefined = NULL,
                      progress_bar = TRUE){
  UseMethod("get_model", x)
}
#' @export
get_model.workspace <- function(x, workspace,
                                userdefined = NULL,
                                progress_bar = TRUE){
  multiprocessings <- get_all_objects(x)
  nb_mp <- length(multiprocessings)
  
  result <- lapply(1:nb_mp, function(i){
    if (progress_bar)
      cat(sprintf("Multiprocessing %i on %i:\n", i, nb_mp))
    get_model(multiprocessings[[i]],
                     workspace = x, userdefined = userdefined,
                     progress_bar = progress_bar)
  })
  names(result) <- names(multiprocessings)
  result
  
}
#' @export
get_model.multiprocessing <- function(x, workspace,
                                      userdefined = NULL,
                                      progress_bar = TRUE){
  all_sa_objects <- get_all_objects(x)
  nb_sa_objs <- length(all_sa_objects)
  
  if (progress_bar)
    pb <- txtProgressBar(min = 0, max = nb_sa_objs, style = 3)
  
  result <- lapply(1:nb_sa_objs, function(i){
    res <- get_model(all_sa_objects[[i]],
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
get_model.sa_item <- function(x, workspace,
                              userdefined = NULL,
                              progress_bar = TRUE){
  jspec <- get_jspec(x)
  jresult <- sa_results(x)
  y_ts <- get_ts(x)
  
  context_dictionnary <- .jcall(workspace,
                                "Lec/tstoolkit/algorithm/ProcessingContext;",
                                "getContext")
  
  result <- tryCatch({
    sa_jd2r(jrslt = jresult, spec = jspec, userdefined = userdefined,
            context_dictionnary = context_dictionnary,
            extra_info = TRUE, freq = frequency(y_ts))
  },error = function(e){
    warning("Error while importing a model: NULL object will be returned")
    NULL
  })
  
  result
}

# Get the results of an saitem
sa_results <- function(jsa) {
  jresult <- .jcall(jsa, "Ldemetra/algorithm/IProcResults;", "getResults")
  if (is.null(jresult))
    warning("The result of the object is NULL: have you compute the workspace importing?\n",
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
