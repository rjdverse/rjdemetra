#' Create a workspace or a multi-processing
#'
#' Functions to create a JDemetra+ workspace (\code{new_workspace()})
#' add a multi-processing to it (\code{new_multiprocessing()}).
#'
#' @param workspace a workspace object
#' @param name character name of the new multiprocessing
#'
#' @return \code{new_workspace()} returns an object of class \code{workspace} and
#' \code{new_multiprocessing()} returns an object of class \code{multiprocessing}.
#'
#' @seealso \code{\link{load_workspace}}, \code{\link{save_workspace}},
#' \code{\link{add_sa_item}}
#'
#' @examples
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' 
#'
#' @name new_workspace
#' @rdname new_workspace
#' @export
new_workspace <- function() {
  dictionary = .jnull("jdr/spec/ts/Utility$Dictionary")
  wk <- .jcall("ec/tstoolkit/jdr/ws/Workspace",
               "Lec/tstoolkit/jdr/ws/Workspace;",
               "create", dictionary)
  wk <- new("workspace", wk)
  return(wk)
}
#' @name new_workspace
#' @rdname new_workspace
#' @export
new_multiprocessing <- function(workspace, name) {
  mp <- .jcall(workspace, "Lec/tstoolkit/jdr/ws/MultiProcessing;", "newMultiProcessing", name)
  mp <- new("multiprocessing", mp)
  return(invisible(mp))
}



#' Save a workspace
#'
#' Functions save a \code{workspace} object into a JDemetra+ workspace.
#'
#' @param workspace a workspace object to export
#' @param file the path to the export JDemetra+ workspace (.xml file).
#' By default a dialog box opens.
#'
#' @seealso \code{\link{load_workspace}}
#'
#' @examples \donttest{
#' dir <- tempdir()
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' new_multiprocessing(wk, "sa1")
#' save_workspace(wk, file.path(dir, "workspace.xml"))
#'}
#'
#' @return A boolean indicating whether the export has suceed.
#' @export
save_workspace <- function(workspace, file) {
  if (missing(file) || is.null(file)) {
    if (Sys.info()[['sysname']] == "Windows") {
      file <- utils::choose.files(default = "demetra_m.xml",
                                  caption = "Select a workspace for the output",
                                  filters = c("JDemetra+ workspace (.xml)", "*.xml"))
    }else{
      file <- NULL # base::file.choose()
    }
    if (length(file) == 0)
      stop("You have to choose a file !")
  }
  if (length(grep("\\.xml$",file)) == 0)
    stop("The file must be a .xml !")

  full_file_name <- normalizePath(file, winslash = "/", mustWork = FALSE)
  folder_wk_name <- sub(".xml$","", full_file_name)
  workspace_name <- basename(folder_wk_name)
  
  
  result <- .jcall(workspace, "Z", "save", full_file_name)
  # To change the name of the workspace
  wk_txt <- readLines(full_file_name)
  wk_txt <- sub(folder_wk_name, workspace_name, wk_txt)
  writeLines(wk_txt, full_file_name)

  invisible(result)
}


#' Add a seasonnaly adjust model to a multi-processing
#'
#' Function to add a new seasonnaly adjust object (class \code{c("SA", "X13")} or \code{c("SA", "TRAMO_SEATS"}) in a \code{workspace} object.
#'
#' @param workspace the workspace to add the seasonnaly adjust model.
#' @param multiprocessing the name or index of the multiprocessing to add the seasonnaly adjust model.
#' @param sa_obj the seasonnaly adjust object to export.
#' @param name The name of the seasonnaly adjust model in the multiprocessing. 
#' By default the name of the \code{sa_obj} is used.
#'
#' @seealso \code{\link{load_workspace}}, \code{\link{save_workspace}}
#'
#' @examples\donttest{
#' dir <- tempdir()
#' spec_x13 <- x13_spec_def(spec = "RSA5c", easter.enabled = FALSE)
#' sa_x13 <- x13(ipi_c_eu[, "FR"], spec = spec_x13)
#' spec_ts <- tramoseats_spec_def(spec = "RSA5")
#' sa_ts <- tramoseats(ipi_c_eu[, "FR"], spec = spec_ts)
#'
#' wk <- new_workspace()
#' new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", sa_x13, "X13")
#' add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
#'
#' save_workspace(wk, file.path(dir, "workspace.xml"))
#' }
#'
#' @export
add_sa_item <- function(workspace, multiprocessing, sa_obj, name){
  if (is.character(multiprocessing)) {
    nb_mp_objects <- count(workspace)
    mp_objects <- lapply(seq_len(nb_mp_objects),
                         function(i) {
                           get_object(workspace, i)
                         })
    mp_names <- sapply(mp_objects, get_name)
    multiprocessing <- match(multiprocessing, mp_names)
    if (is.na(multiprocessing))
      stop("The multiprocessing ",multiprocessing," doesn't exist !")
  }
  if (!is.numeric(multiprocessing))
    stop("The parameter multiprocessing must be a character or a numeric")

  if (missing(name))
    name <- deparse(substitute(sa_obj))
  
  sa_obj <- complete_dictionnary(workspace, sa_obj)
  jspec <- get_jspec(sa_obj)
  y <- sa_obj$final$series[, "y"]


  if (!is.character(name) || length(name) != 1)
    stop("The name of the SA element to add is mispecified")

  mp_obj <- get_object(workspace, multiprocessing)
  .jcall(mp_obj, "V", "add", name, ts_r2jd(y), jspec)
}

# Extract jspec from a R SA object or from a sa_item
get_jspec <- function(x, ...){
  UseMethod("get_jspec", x)
}
get_jspec.X13 <- function(x, ...){
  spec <- x13_spec(x, ...)
  jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  jdictionary <- specX13_r2jd(spec,jrspec)
  seasma <- specX11_r2jd(spec,jrspec, freq = frequency(x$final$series))
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jspec
}
get_jspec.TRAMO_SEATS <- function(x, ...){
  spec <- tramoseats_spec(x, ...)
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- specTS_r2jd(spec,jrspec)
  spec_seats <- specSeats_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jspec
}
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

complete_dictionnary <- function(workspace, sa_obj){
  userdef <- sa_obj$regarima$specification$regression$userdef
  ud_var <- userdef$variables
  if (!userdef$specification["variables"] || is.na(ud_var$series))
    return(sa_obj)

  context_dictionnary <- .jcall(workspace,"Lec/tstoolkit/algorithm/ProcessingContext;", "getContext")
  ts_variable_managers <- context_dictionnary$getTsVariableManagers()
  ts_variables <- .jnew("ec/tstoolkit/timeseries/regression/TsVariables")
  jd_r_variables <- ts_variable_managers$get("r")
  if (is.null(jd_r_variables)) {
    ts_variable_managers$set("r",
                             .jnew("ec/tstoolkit/timeseries/regression/TsVariables"))
    jd_r_variables <- ts_variable_managers$get("r")
  }
  jd_var_names <- jd_r_variables$getNames()

  model_var_names <-  base::make.unique(c(jd_var_names,
                                          rownames(ud_var$description)),
                                        sep = "_")
  model_var_names <- tail(model_var_names,
                          length(rownames(ud_var$description)))

  rownames(sa_obj$regarima$specification$regression$userdef$variables$description) <-
    model_var_names


  if (is.mts(ud_var$series)) {
    new_vars <- lapply(1:length(model_var_names), function(i){
      .jnew("ec/tstoolkit/timeseries/regression/TsVariable",
            model_var_names[i], ts_r2jd(ud_var$series[,i]))
    })
  }else{
    new_vars <- list(.jnew("ec/tstoolkit/timeseries/regression/TsVariable",
                           model_var_names,
                           ts_r2jd(ud_var$series))
    )
  }
  names(new_vars) <- model_var_names

  for (name in names(new_vars)) {
    jd_r_variables$set(name, new_vars[[name]])
  }

  return(sa_obj)
}
