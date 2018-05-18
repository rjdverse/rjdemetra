.onLoad <- function(libname, pkgname){
  .jpackage(pkgname, lib.loc = libname)
}

.onAttach <- function(libname, pkgname){
  rjdemetra_java$clobject <- .jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
}
