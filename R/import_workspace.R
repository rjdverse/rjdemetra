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

#' Load a 'JDemetra+' workspace
#'
#' Function to load a 'JDemetra+' workspace.
#'
#' @param file the path to the 'JDemetra+' workspace to load.
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

  full_file_name <- full_path(file)
  workspace <- .jcall("ec/tstoolkit/jdr/ws/Workspace", "Lec/tstoolkit/jdr/ws/Workspace;", "open",
                      full_file_name)
  workspace <- new("workspace", workspace)
  return(workspace)
}


#' Get objects inside a workspace or multiprocessing
#'
#' Generic functions to retrieve all (\code{get_all_objects()}) \code{multiprocessing} (respectively \code{sa_item})
#' from a \code{workspace} (respectively \code{multiprocessing}) or to retrieve a single one (\code{get_object()}) .
#'
#' @param x the object in which to store the extracted \code{multiprocessing} or \code{sa_item}.
#' @param pos the index of the object to extract.
#'
#' @return An object of class \code{multiprocessing} or \code{sa_item} (for \code{get_object()}) or a list
#' of objects of class \code{multiprocessing} or \code{sa_item} (for \code{get_all_objects()}).
#'
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{count}}, \code{\link{get_model}}, \code{\link{get_name}}, \code{\link{get_ts}}.
#'
#' @examples\donttest{
#'
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = "RSA5c")
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#'
#' # A way to retrieve the multiprocessing:
#' mp <- get_object(wk, 1)
#' # And the sa_item object:
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
#' Generic functions to retrieve the Java name of a \code{multiprocessing} or a \code{sa_item}.
#'
#' @param x the object to retrieve the name from.
#'
#' @return A \code{character}.
#'
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{count}}, \code{\link{get_model}}, \code{\link{get_ts}}.
#'
#' @examples \donttest{
#' spec_x13 <- x13_spec(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec(spec = "RSA5")
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' add_sa_item(wk, "sap1", sa_ts, "TramoSeats")
#'
#' sa_item1 <- get_object(mp, 1)
#' sa_item2 <- get_object(mp, 2)
#'
#' get_name(sa_item1) # returns "X13"
#' get_name(sa_item2) # returns "TramoSeats"
#'
#' get_name(mp) # returns "sap1"
#'
#' # To retrieve the name of every sa_item in a given multiprocessing:
#' sapply(get_all_objects(mp), get_name)
#'
#' # To retrieve the name of every multiprocessing in a given workspace:
#' sapply(get_all_objects(wk), get_name)
#'
#' # To retrieve the name of every sa_item in a given workspace:
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
  jt <- .jcall(x, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ljd2/datatypes/Ts;", "getTs")
  name <- .jcall(jts, "S", "getName")
  # Remove the name of the file link to the saitem
  name <- gsub("^.*\\n", "", name)
  return(name)
}


#' Get the Java name of all the contained object
#'
#' Generic functions to retrieve the Java name of the contained \code{multiprocessings} or the contained  \code{sa_items}.
#'
#' @param x An object containing other objects whose names we want to know
#'
#' @return A \code{character} vector containing all the names.
#'
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{get_name}}, \code{\link{get_position}}, \code{\link{count}}, \code{\link{get_model}}, \code{\link{get_ts}}.
#'
#' @examples \donttest{
#' spec_x13 <- x13_spec(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec(spec = "RSA5")
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' mp2 <- new_multiprocessing(wk, "sap2")
#'
#' get_all_names(wk)
#'
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' add_sa_item(wk, "sap1", sa_ts, "TramoSeats")
#'
#' get_all_names(mp)
#' }
#'
#' @export
get_all_names <- function(x){
  UseMethod("get_all_names", x)
}
#' @export
get_all_names.workspace <- function(x){
  saps <- get_all_objects(x)
  names_saps <- names(saps)
  return(names_saps)
}
#' @export
get_all_names.multiprocessing <- function(x){
  sa_items <- get_all_objects(x)
  names_sa_items <- names(sa_items)
  return(names_sa_items)
}

#' Get the position of an object
#'
#' Generic functions to retrieve the position of the contained \code{multiprocessings} or the contained  \code{sa_items}.
#'
#' @param x An object containing other objects whose names we want to know
#' @param name a\code{character} specifiing an object
#'
#' @return A \code{integer}
#'
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{get_name}}, \code{\link{get_all_names}}, \code{\link{count}}, \code{\link{get_model}}, \code{\link{get_ts}}.
#'
#' @examples \donttest{
#' spec_x13 <- x13_spec(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec(spec = "RSA5")
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' mp2 <- new_multiprocessing(wk, "sap2")
#'
#' get_position(wk, "sap1")
#' get_position(wk, "sap2")
#'
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' add_sa_item(wk, "sap1", sa_ts, "TramoSeats")
#'
#' get_position(mp, "TramoSeats")
#' get_position(mp, "X13")
#' }
#'
#' @export
get_position <- function(x, name){
  UseMethod("get_position", x)
}
#' @export
get_position.workspace <- function(x, name){
  all_names <- get_all_names(x)
  position <- which(all_names == name)
  if (length(position) == 1L && position == 0L) {
    warning("No SA-Processing have this name.")
    return(0L)
  } else if (length(position) > 1L) {
    warning("Several SA-Processings have this name.")
    return(position)
  } else {
    return(position)
  }
}
#' @export
get_position.multiprocessing <- function(x, name){
  all_names <- get_all_names(x)
  position <- which(all_names == name)
  if (length(position) == 1L && position == 0L) {
    warning("No SA-Item have this name.")
    return(0L)
  } else if (length(position) > 1L) {
    warning("Several SA-Items have this name.")
    return(position)
  } else {
    return(position)
  }
}

#' Count the number of objects inside a workspace or multiprocessing
#'
#' Generic functions to count the number of \code{multiprocessing} (respectively \code{sa_item})
#' inside a \code{workspace} (respectively \code{multiprocessing}).
#'
#' @param x the \code{workspace} or the \code{multiprocessing}.
#'
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{get_model}}, \code{\link{get_name}}, \code{\link{get_ts}}.
#'
#' @examples
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
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
#' Generic functions to retrieve the input raw time series of a \code{workspace}, \code{multiprocessing},
#' \code{sa_item} or \code{SA} object.
#'
#' @param x the object from which to retrieve the time series.
#'
#' @return \code{get_ts()} returns a \code{\link[stats]{ts}} object or list of \code{\link[stats]{ts}} objects:
#' \itemize{
#'  \item if \code{x} is a \code{sa_item} or a \code{SA} object, \code{get_ts(x)} returns a single \code{ts} object;
#'  \item if \code{x} is a \code{multiprocessing} object, \code{get_ts(x)} returns a list of length the number
#'  of sa_items, each element being a \code{ts} object;
#'  \item if \code{x} is a \code{workspace} object, \code{get_ts(x)} returns a list of length the number of multiprocessings,
#'  each element being a list of \code{ts} objects.
#'}
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{count}}, \code{\link{get_model}}, \code{\link{get_name}}.
#'
#' @examples\donttest{
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = "RSA5c")
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' sa_item <- get_object(mp, 1)
#'
#'   # Extracting the raw time series from an adjusted series:
#' get_ts(sa_x13) # Returns the ts object ipi_c_eu[, "FR"]
#'
#'   # Extracting the raw time series from a sa_item:
#' get_ts(sa_item) # Returns the ts object ipi_c_eu[, "FR"]
#'
#'   # Extracting all raw time series from a multiprocessing:
#' # Returns a list of length 1 named "X13" containing the ts object ipi_c_eu[, "FR"]:
#' get_ts(mp)
#'
#'
#'   # Extracting all raw time series from a workspace:
#' # Returns a list of length 1 named "sap1" containing a list
#' # of length 1 named "X13", containing the ts object ipi_c_eu[, "FR"]
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
  jt <- .jcall(x, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
  jts <- .jcall(jt, "Ljd2/datatypes/Ts;", "getTs")
  j_ts_series <- .jcall(jts, "Lec/tstoolkit/timeseries/simplets/TsData;", "getData")
  return(ts_jd2r(j_ts_series))
}
#' @export
get_ts.SA <- function(x){
  return(x$final$series[,"y"])
}
#' @export
get_ts.jSA <- function(x){
  return(get_indicators(x, "y")[[1]])
}
#' @export
get_ts.regarima <- function(x){
  mts <- x[["model"]][["effects"]]
  y <- mts[,"y_lin"] + mts[,"tde"] + mts[,"ee"] + mts[,"omhe"] + mts[,"out"]
  if (x$model$spec_rslt[1, "Log transformation"]) {
    y <- exp(y)
  }

  return(y)
}

#' Compute a workspace multi-processing(s)
#'
#' Function to compute all the multiprocessings or only a given one from a workspace.
#' By default, the workspace only contains definitions: computation is needed to recalculate and access the adjusted model
#' (with \code{\link{get_model}}).
#'
#' @param workspace the workspace to compute.
#' @param i a \code{character} or \code{numeric} indicating the name or the index of the multiprocessing to compute.
#' By default, all multiprocessings are computed.
#'
#' @seealso \code{\link{get_model}}
#'
#' @examples
#' \donttest{
#' spec_x13 <- x13_spec(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' sa_item1 <- get_object(mp, 1)
#'
#' get_model(sa_item1, wk) # Returns NULL
#'
#' compute(wk)
#'
#' get_model(sa_item1, wk) # Returns the SA model sa_x13
#' }
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
#' Generic functions to retrieve seasonally adjusted model(s) from \code{workspace},
#' \code{multiprocessing} or \code{sa_item} object. \code{get_model} returns a \code{"SA"} object while \code{get_jmodel} returns the Java objects of the models.
#'
#' @param x the object from which to retrieve the seasonally adjusted model.
#' @param workspace the workspace object where models are stored. If \code{x} is a \code{workspace} object, this parameter is not used.
#' @param userdefined a vector containing the names of additional output variables.
#' (see \code{\link{x13}} or \code{\link{tramoseats}}).
#' @param progress_bar Boolean: if \code{TRUE}, a progress bar is printed.
#'
#' @return \code{get_model()} returns a seasonally adjusted object (class \code{c("SA", "X13")} or \code{c("SA", "TRAMO_SEATS"}) or a list of seasonally adjusted objects:
#' \itemize{
#'  \item if \code{x} is a \code{sa_item} object, \code{get_model(x)} returns a \code{"SA"} object (or a \code{\link{jSA}} object with \code{get_jmodel(x)});
#'  \item if \code{x} is a \code{multiprocessing} object, \code{get_ts(x)} returns a list of length the number
#'  of sa_items, each element containing a \code{"SA"} object (or a \code{\link{jSA}} object with \code{get_jmodel(x)});
#'  \item if \code{x} is a \code{workspace} object, \code{get_ts(x)} returns list of length the number of multiprocessings,
#'  each element containing a list of \code{"SA"} object(s) (or \code{\link{jSA}} object's) with \code{get_jmodel(x)}).
#'}
#' @seealso Other functions to retrieve information from a workspace, multiprocessing or sa_item: \code{\link{count}}, \code{\link{get_name}}, \code{\link{get_ts}}.
#' @seealso \code{\link{compute}}
#'
#' @examples\donttest{
#' spec_x13 <- x13_spec(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec(spec = "RSA5")
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sap1")
#' add_sa_item(wk, "sap1", sa_x13, "X13")
#' add_sa_item(wk, "sap1", sa_ts, "TramoSeats")
#'
#' compute(wk) # It's important to compute the workspace before retrieving the SA model
#' sa_item1 <- get_object(mp, 1)
#'
#' get_model(sa_item1, wk) # To extract the model of the sa_item1: its the object sa_x13
#'
#' # To get all models from the multiprocessing mp:
#' get_model(mp, wk)
#'
#' # To get all models from the workspace wk:
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

  result <- lapply(seq_len(nb_mp), function(i){
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

  result <- lapply(seq_len(nb_sa_objs), function(i){
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
  jsa_result <- get_jmodel.sa_item(x, workspace)
  if(is.null(jsa_result))
    return(NULL)
  jspec <- jsa_result[["spec"]]
  jresult <- jsa_result[["result"]]@internal
  y_ts <- get_ts(x)

  context_dictionary <- .jcall(workspace,
                                "Lec/tstoolkit/algorithm/ProcessingContext;",
                                "getContext")

  result <- tryCatch({
    sa_jd2r(jrslt = jresult, spec = jspec, userdefined = userdefined,
            context_dictionary = context_dictionary,
            extra_info = TRUE, freq = frequency(y_ts))
  },error = function(e){
    warning(e, "Error while importing a model: NULL object will be returned",
            call. = FALSE)
    NULL
  })

  result
}

# To retrieve the results of an sa_item
sa_results <- function(jsa) {
  jresult <- .jcall(jsa, "Ljd2/algorithm/IProcResults;", "getResults")
  if (is.null(jresult))
    warning("The result of the object is NULL: have you computed the workspace after importing it?\n",
            "See ?compute for more information.")
  return(jresult)
}

# To retrieve the specifications of a sa_item (possible values for type: Domain, Estimation, Point)
sa_spec <- function(jsa, type = "Domain") {
  jt <- .jcall(jsa, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
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
