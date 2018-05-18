final <- function(jrobj){
  finals_ts_names <- c("y", "y_f", "t", "t_f", "sa", "sa_f", "s", "s_f", "i", "i_f")
  finals_ts <- do.call(ts.union,
                       lapply(finals_ts_names,
                              function(series) result(jrobj, series)))
  colnames(finals_ts) <- finals_ts_names
  class(finals_ts) <- c("final","mts","ts","matrix")
  finals_ts
}
