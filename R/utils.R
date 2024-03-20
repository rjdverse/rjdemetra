identical_na <- function(x){
  identical(x, NA) ||
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}


ramp <- function(start = 1900, end = 2100,
                 start_ramp, end_ramp, frequency = 12){
  start <- format_ts_date(start, frequency)
  end <- format_ts_date(end, frequency)
  # start_ramp and end_ramp are defined in month and not in the frequency of the model
  new_period <-  rep(seq.int(from = 1, to = frequency), each = 12 / frequency)
  if (length(start_ramp) == 2) {
    start_ramp[2] <- new_period[start_ramp[2]]
  }
  if (length(end_ramp) == 2) {
    end_ramp[2] <- new_period[end_ramp[2]]
  }
  start_ramp <- format_ts_date(start_ramp, frequency)
  end_ramp <- format_ts_date(end_ramp, frequency)

  if (start_ramp >= end_ramp)
    return(NULL)
  if (missing(start) || missing(end)) {
    # if start and end not specified by hand
    start <- min(start_ramp - 1, start)
    end <- max(end_ramp + 1, end)
  }

  x <- ts(-1, start = start, end = end,
          frequency = frequency)
  t <- ts(1:length(x), start = start, end = end,
          frequency = frequency)
  t0 <- as.numeric(window(t, start = start_ramp, end = start_ramp))
  t1 <- as.numeric(window(t, start = end_ramp, end = end_ramp))
  x <- -1 * (t <= t0) + ((t - t0) / (t1-t0) - 1) * (t > t0) * (t < t1)
  x
}

format_ts_date <- function(date, frequency){
  if (length(date) == 2)
    date <- date[1] + (date[2] - 1) / frequency
  date
}
