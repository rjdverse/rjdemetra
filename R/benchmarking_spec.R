benchmarking_spec_def <- function(spec,
                                  benchmarking.enabled = NA,
                                  benchmarking.target = c(NA, "Original", "CalendarAdjusted"),
                                  benchmarking.useforecast = NA,
                                  benchmarking.rho = NA_real_,
                                  benchmarking.lambda = NA_real_)

{
  default_spec <- data.frame(benchmarking.enabled = FALSE, benchmarking.target = "CalendarAdjusted",
                             benchmarking.useforecast = FALSE, benchmarking.rho = 1, benchmarking.lambda = 1)
  if(identical(spec, "X11")) {
    benchmarking.mod <- rbind(
      default_spec,
      default_spec,
      NA)
    return(spec_benchmarking(benchmarking.mod))
  }

  benchmarking.target <- match.arg(benchmarking.target)


  list.logical <- list("benchmarking.enabled", "benchmarking.useforecast")
  list.numeric <- list("benchmarking.rho", "benchmarking.lambda")

  var.list <- list()
  for (i in 1:length(list.logical)) {
    eval(parse(text = paste("if( !is.logical(",list.logical[i],")) {",
                            list.logical[i],
                            " = NA; var.list=append(var.list,'",
                            list.logical[i],
                            "')}",
                            sep = "")))
  }
  if (length(var.list) > 0) {
    warning(paste("Variable(s)",
                  deparse(as.character(var.list)),
                  " should be logical. They are ignored."),
            call. = FALSE)
  }

  var.list <- list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text = paste("if( !is.numeric(",
                            list.numeric[i],
                            ")) {",
                            list.numeric[i],
                            " = NA; var.list=append(var.list,'",
                            list.numeric[i],
                            "')}",
                            sep = "")))
  }
  if (length(var.list) > 0) {
    warning(paste("Variable(s)",
                  deparse(as.character(var.list)),
                  " should be numeric. They are ignored."),
            call. = FALSE)
  }

  benchmarking <- data.frame(
    benchmarking.enabled = benchmarking.enabled, benchmarking.target = benchmarking.target,
    benchmarking.useforecast = benchmarking.useforecast, benchmarking.rho = benchmarking.rho,
    benchmarking.lambda = benchmarking.lambda)
  benchmarking.mod <- rbind(
    default_spec,
    benchmarking,
    NA)
  return(spec_benchmarking(benchmarking.mod))
}

spec_benchmarking <- function(benchmarking){

  for (i in c("benchmarking.enabled", "benchmarking.target", "benchmarking.useforecast",
              "benchmarking.rho", "benchmarking.lambda")){
    benchmarking[3,i] <- if (!is.na(benchmarking[2,i])) {benchmarking[2,i]} else {benchmarking[1,i]}
  }
  if (!benchmarking[3,"benchmarking.enabled"]) {
    benchmarking[3, "benchmarking.target"] <- "CalendarAdjusted"
    benchmarking[3, "benchmarking.useforecast"] <- FALSE
    benchmarking[3, "benchmarking.rho"] <- 1
    benchmarking[3, "benchmarking.lambda"] <- 1
  }

  rownames(benchmarking) <- c("Predefined","User_modif","Final")
  class(benchmarking) <- c("benchmarking_spec", "data.frame")
  return(benchmarking)
}

benchmarking_spec<- function(spec,
            benchmarking.enabled = NA,
            benchmarking.target = c(NA, "Original", "CalendarAdjusted"),
            benchmarking.useforecast = NA,
            benchmarking.rho = NA_real_,
            benchmarking.lambda = NA_real_)

{
  benchmarking.target <- match.arg(benchmarking.target)

  list.logical <- list("benchmarking.enabled", "benchmarking.useforecast")
  list.numeric <- list("benchmarking.rho", "benchmarking.lambda")

  var.list <- list()
  for (i in 1:length(list.logical)) {
    eval(parse(text = paste("if( !is.logical(",list.logical[i],")) {",
                            list.logical[i],
                            " = NA; var.list=append(var.list,'",
                            list.logical[i],
                            "')}",
                            sep = "")))
  }
  if (length(var.list) > 0) {
    warning(paste("Variable(s)",
                  deparse(as.character(var.list)),
                  " should be logical. They are ignored."),
            call. = FALSE)
  }

  var.list <- list()
  for (i in 1:length(list.numeric)) {
    eval(parse(text = paste("if( !is.numeric(",
                            list.numeric[i],
                            ")) {",
                            list.numeric[i],
                            " = NA; var.list=append(var.list,'",
                            list.numeric[i],
                            "')}",
                            sep = "")))
  }
  if (length(var.list) > 0) {
    warning(paste("Variable(s)",
                  deparse(as.character(var.list)),
                  " should be numeric. They are ignored."),
            call. = FALSE)
  }

  benchmarking <- data.frame(
    benchmarking.enabled = benchmarking.enabled, benchmarking.target = benchmarking.target,
    benchmarking.useforecast = benchmarking.useforecast, benchmarking.rho = benchmarking.rho,
    benchmarking.lambda = benchmarking.lambda)
  benchmarking.spec <- s_benchmarking(spec)
  benchmarking.mod <- rbind(benchmarking.spec, benchmarking, NA)
  return(spec_benchmarking(benchmarking.mod))
}


spec_benchmarking_r2jd <- function(rspec = NA, jdspec = NA){
  benchmarking <- s_benchmarking(rspec)
  jbench <- .jcall(jdspec,"Ljdr/spec/sa/SaBenchmarkingSpec;","getBenchmarking")

  .jcall(jbench, "V", "setEnabled", benchmarking[["benchmarking.enabled"]])
  if (benchmarking[["benchmarking.enabled"]]) {
    .jcall(jbench, "V", "setTarget", benchmarking[["benchmarking.target"]])
    .jcall(jbench, "V", "setUseForecast", benchmarking[["benchmarking.useforecast"]])
    .jcall(jbench, "V", "setRho", benchmarking[["benchmarking.rho"]])
    .jcall(jbench, "V", "setLambda", benchmarking[["benchmarking.lambda"]])
  }

  return(jbench)
}

spec_benchmarking_jd2r <- function(jrobj){
  jbench <- .jcall(jrobj, "Ljdr/spec/sa/SaBenchmarkingSpec;", "getBenchmarking")
  benchmarking.target <- .jcall(jbench, "Ljava/lang/String;", "getTarget")
  benchmarking.enabled <- .jcall(jbench, "Z", "isEnabled")
  benchmarking.useforecast <- .jcall(jbench, "Z", "isUseForecast")
  benchmarking.rho <- .jcall(jbench, "D", "getRho")
  benchmarking.lambda <- .jcall(jbench, "D", "getLambda")

  data.frame(
    benchmarking.enabled = benchmarking.enabled, benchmarking.target = benchmarking.target,
    benchmarking.useforecast = benchmarking.useforecast, benchmarking.rho = benchmarking.rho,
    benchmarking.lambda = benchmarking.lambda)
}

benchmarking <- function(jrobj,spec){
  specification <- spec[3,]
  rownames(specification) <- ""
  if(specification[["benchmarking.enabled"]]) {
    original <- result(jrobj, "benchmarking.original")
    result <- result(jrobj, "benchmarking.result")
    Differences <- original - result
    bench_res <- ts.union(original, result, Differences)
  } else {
    bench_res <- NULL
  }
  z <- list(specification = specification, benchmarking = bench_res)
  class(z) <- c("benchmarking")
  return(z)
}

benchmarking_def <- function(jrobj,jspec){
  specification <- spec_benchmarking_jd2r(jspec)
  rownames(specification) <- ""
  if(specification[["benchmarking.enabled"]]) {
    original <- result(jrobj, "benchmarking.original")
    result <- result(jrobj, "benchmarking.result")
    Differences <- original - result
    bench_res <- ts.union(original, result, Differences)
  } else {
    bench_res <- NULL
  }
  z <- list(specification = specification, benchmarking = bench_res)
  class(z) <- c("benchmarking")
  return(z)
}
