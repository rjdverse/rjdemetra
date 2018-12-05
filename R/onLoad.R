.onLoad <- function(libname, pkgname){
  if(is.null(getOption("enable_print_style")))
    options(enable_print_style = FALSE)
    
  .jpackage(pkgname, lib.loc = libname)
}

.onAttach <- function(libname, pkgname){
  rjdemetra_java$clobject <- .jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
}
