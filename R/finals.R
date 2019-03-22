final <- function(jrobj){

  finals_ts_names1 <- c("y", "sa", "t", "s", "i")
  finals_ts_names2 <- paste0(finals_ts_names1, "_f")
  finals_ts1 <- lapply(finals_ts_names1,
                       function(series) result(jrobj, series))
  finals_ts1 <- ts(simplify2array(finals_ts1),
                   start = start(finals_ts1[[1]]), frequency = frequency(finals_ts1[[1]]))
  colnames(finals_ts1) <- c(finals_ts_names1)

  finals_ts2 <- lapply(finals_ts_names2,
                       function(series) result(jrobj, series))
  if(!is.null(finals_ts2[[1]])){
    finals_ts2 <- ts(matrix(simplify2array(finals_ts2),ncol = length(finals_ts_names2)),
                     start = start(finals_ts2[[1]]), frequency = frequency(finals_ts2[[1]]))
    colnames(finals_ts2) <- c(finals_ts_names2)
  }else{
    # No forecast
    finals_ts2 <- NULL
  }

  finals_ts <- list(series = finals_ts1, forecasts = finals_ts2)
  class(finals_ts) <- c("final","list")
  finals_ts
}
