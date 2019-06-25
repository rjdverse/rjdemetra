.onLoad <- function(libname, pkgname){
  if(is.null(getOption("enable_print_style")))
    options(enable_print_style = TRUE)

  .jpackage(pkgname, lib.loc = libname)

  if (!check_valid_java_version())
    stop("You need Java 8 or higher to use 'RJDemetra'")
}

