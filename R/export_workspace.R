
# Extract jspec of a R object
extract_spec <- function(x, ...){
  UseMethod("extract_spec", x)
}
extract_spec.X13 <- function(x, ...){
  spec <- x13_spec(x)
  jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
  jdictionary <- RJDemetra:::specX13_r2jd(spec,jrspec)
  seasma <- RJDemetra:::specX11_r2jd(spec,jrspec, freq = frequency(x$final))
  jspec <- .jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
  jspec
}
extract_spec.TRAMO_SEATS <- function(x, ...){
  spec <- tramoseats_spec(x)
  jrspec <- .jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
  jdictionary <- RJDemetra:::specTS_r2jd(spec,jrspec)
  spec_seats <- RJDemetra:::specSeats_r2jd(spec,jrspec)
  jspec <- .jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
  jspec
}
extract_spec.sa_item <- function(x, ...){
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


# Creates a new workspace
new_workspace <- function(dictionary = .jnull("jdr/spec/ts/Utility$Dictionary")) {
  wk <- .jcall("ec/tstoolkit/jdr/ws/Workspace", "Lec/tstoolkit/jdr/ws/Workspace;", "create", dictionary)
  wk <- new("workspace", multiproc)
  return(wk)
}

# Creates a new multi-doument
new_multiprocessing <- function(workspace, name) {
  mp <- .jcall(workspace, "Lec/tstoolkit/jdr/ws/MultiProcessing;", "newMultiProcessing", name)
  mp <- new("multiprocessing", mp)
  return(mp)
}

# Add a new element in a multiprocessing, jmp == multiprocessing,
add_saitem <- function(multiprocessing, sa_obj, name){
  jspec <- extract_spec(sa_obj)
  y <- sa_obj$final[,"y"]

  if(missing(name))
    name <- deparse(substitute(sa_obj))

  if(!is.character(name) || length(name) != 1)
    stop("The name of the SA element to add is mispecified")

  add_saitem_(jmp = multiprocessing, name = name, jts = ts_r2jd(y), jspec = jspec)
}

add_saitem_ <- function(jmp, name, jts, jspec) {
  .jcall(jmp, "V", "add", name, jts, jspec)
}

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
  if(length(grep("\\.xml$",workspace_path))==0)
    stop("The file must be a .xml !")

  .jcall(workspace, "Z", "save", file)
}
