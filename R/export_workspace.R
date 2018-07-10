#' Create a workspace or a multiprocessing
#'
#' Functions to create a JDemetra+ workspace (\code{new_workspace()})
#' add a multiprocessing to it (\code{new_multiprocessing})
#'
#' @param workspace a workspace object
#' @param name character name of the new multiprocessing
#'
#' @return \code{new_workspace()} returns an object of class \code{workspace} and
#' \code{new_multiprocessing()} returns an object of class \code{multiprocessing}.
#'
#' @example \dontrun{
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' save_workspace(wk, "workspace.xml")
#' }
#'
#' @family Functions to manipulate workspace
#'
#' @name new_workspace
#' @rdname new_workspace
#' @export
new_workspace <- function() {
  dictionary = .jnull("jdr/spec/ts/Utility$Dictionary")
  wk <- .jcall("ec/tstoolkit/jdr/ws/Workspace",
               "Lec/tstoolkit/jdr/ws/Workspace;",
               "create", dictionary)
  wk <- new("workspace", multiproc)
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




#' Create a workspace or a multiprocessing
#'
#' Functions to create a JDemetra+ workspace (\code{new_workspace()})
#' add a multiprocessing to it (\code{new_multiprocessing})
#'
#' @param workspace a workspace object to export
#' @param file the path to the export JDemetra+ workspace (.xml file).
#' If no specified a dialog box opens.
#'
#'
#' @example \dontrun{
#' # Create and export a empty JDemetra+ workspace
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' save_workspace(wk, "workspace.xml")
#' }
#'
#' @family Functions to manipulate workspace
#'
#' @export
save_workspace <- function(workspace, file) {
  if(missing(file) || is.null(file)){
    if(Sys.info()[['sysname']] == "Windows"){
      file <- utils::choose.files(default = "demetra_m.xml",
                                  caption = "Select a workspace for the output",
                                  filters = c("JDemetra+ workspace (.xml)","*.xml"))
    }else{
      file <- base::file.choose()
    }
    if(length(file) == 0)
      stop("You have to choose a file !")
  }
  if(length(grep("\\.xml$",file))==0)
    stop("The file must be a .xml !")

  .jcall(workspace, "Z", "save", file)
}


#' @export
# Add a new element in a multiprocessing, jmp == multiprocessing,
add_saitem <- function(multiprocessing, sa_obj, name){
  jspec <- get_jspec(sa_obj)
  y <- sa_obj$final[,"y"]

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
  spec <- x13_spec(x)
  jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  jdictionary <- specX13_r2jd(spec,jrspec)
  seasma <- specX11_r2jd(spec,jrspec, freq = frequency(x$final))
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jspec
}
get_jspec.TRAMO_SEATS <- function(x, ...){
  spec <- tramoseats_spec(x)
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- specTS_r2jd(spec,jrspec)
  spec_seats <- specSeats_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jspec
}
get_jspec.sa_item <- function(x, ...){
  spec <- sa_spec(x, ...)
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
