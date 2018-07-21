#' Create a workspace or a multi-processing
#'
#' Functions to create a JDemetra+ workspace (\code{new_workspace()})
#' add a multi-processing to it (\code{new_multiprocessing}).
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
#' @examples \dontrun{
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' }
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
  return(mp)
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
#' @examples \dontrun{
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' save_workspace(wk, "workspace.xml")
#' }
#'
#'
#' @return A boolean indicating whether the export has suceed.
#' @export
save_workspace <- function(workspace, file) {
  if(missing(file) || is.null(file)){
    if(Sys.info()[['sysname']] == "Windows"){
      file <- utils::choose.files(default = "demetra_m.xml",
                                  caption = "Select a workspace for the output",
                                  filters = c("JDemetra+ workspace (.xml)","*.xml"))
    }else{
      file <- NULL # base::file.choose()
    }
    if(length(file) == 0)
      stop("You have to choose a file !")
  }
  if(length(grep("\\.xml$",file))==0)
    stop("The file must be a .xml !")

  # actual_wd <- getwd()
  # file_export_wd <- dirname(file)

  invisible(.jcall(workspace, "Z", "save", file))
}


#' Add a seasonnaly adjust model to a multi-processing
#'
#' Function to add a new seasonnaly adjust object (class \code{c("SA","X13")} or \code{c("SA","TRAMO_SEATS"}) in a \code{multiprocessing} object.
#'
#' @param multiprocessing the multiprocessing object to add the seasonnaly adjust model.
#' @param sa_obj the seasonnaly adjust object to export.
#' @param name The name of the seasonnaly adjust model in the multiprocessing
#' By default the name of the \code{sa_obj} is used.
#'
#' @seealso \code{\link{load_workspace}}, \code{\link{save_workspace}}
#'
#' @examples \dontrun{
#' spec_x13 <-x13_spec_def(spec = c("RSA5c"), easter.enabled = FALSE)
#' sa_x13 <- x13(myseries, spec = spec_x13)
#' spec_ts <-tramoseats_spec_def(spec = c("RSA5"))
#' sa_ts <- tramoseats(myseries, spec = spec_ts)
#'
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(mp, sa_x13, "X13")
#' add_sa_item(mp, sa_ts, "TramoSeats")
#'
#' save_workspace(wk, "workspace.xml")
#' }
#'
#'
#' @export
add_sa_item <- function(multiprocessing, sa_obj, name){

  if(! is.multiprocessing(multiprocessing))
      stop("Use multiprocessing object !")

  jspec <- get_jspec(sa_obj)
  y <- sa_obj$final$series[, "y"]

  if(missing(name))
    name <- deparse(substitute(sa_obj))

  if(!is.character(name) || length(name) != 1)
    stop("The name of the SA element to add is mispecified")

  .jcall(multiprocessing, "V", "add", name, ts_r2jd(y), jspec)
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
  if(.jinstanceof(spec, "ec/satoolkit/tramoseats/TramoSeatsSpecification")){
    spec <- .jcast(spec, "ec/satoolkit/tramoseats/TramoSeatsSpecification")
    spec <- .jnew("jdr/spec/tramoseats/TramoSeatsSpec",spec)
  }else{
    if(.jinstanceof(spec, "ec/satoolkit/x13/X13Specification")){
      spec <- .jcast(spec, "ec/satoolkit/x13/X13Specification")
      spec <- .jnew("jdr/spec/x13/X13Spec", spec)
    }else{
      stop("Error loading the specification")
    }
  }
  spec
}
