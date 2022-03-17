.onLoad <- function(libname, pkgname){
  if(is.null(getOption("enable_print_style")))
    options(enable_print_style = TRUE)

  if(is.na(getOption("java_ncore", default = NA))){
    # limit to 2 the cores for CRAN policies
    jopts_core <- "-XX:ActiveProcessorCount=2"
  }else{
    if (is.null(getOption("java_ncore", default = NA))){
      jopts_core <- NULL
    } else{
      jopts_core <- sprintf("-XX:ActiveProcessorCount=%i", getOption("java_ncore"))
    }
  }

  .jpackage(pkgname, lib.loc = libname,
            parameters = c(getOption("java.parameters"), jopts_core))

  if (!check_valid_java_version())
    stop("You need Java JRE >= 8 to use 'RJDemetra'")

}

