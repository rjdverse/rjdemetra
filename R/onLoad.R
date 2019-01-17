.onLoad <- function(libname, pkgname){
  if(is.null(getOption("enable_print_style")))
    options(enable_print_style = TRUE)
    
  .jpackage(pkgname, lib.loc = libname)
}
