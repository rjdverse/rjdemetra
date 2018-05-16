finals <- function(jrobct, jd_clobj){
  finals_ts_names <- c("y", "y_f", "t", "t_f", "sa", "sa_f", "s", "s_f", "i", "i_f")
  finals_ts <- do.call(ts.union,
                       lapply(finals_ts_names,
                              function(series) result(jd_clobj, jrobct, series)))
  colnames(finals_ts) <- finals_ts_names
  class(finals_ts) <- c("finals","mts","ts","matrix")
  finals_ts
}

#' @export
plot.finals <- function(x, first_date = start(x), last_date = end(x), forecast = TRUE,
                        type_chart = c("sa-trend", "cal-seas-irr"),
                        ask =  length(type_chart) > 1 && dev.interactive(),
                        ...){
  type_chart <- match.arg(type_chart, several.ok = TRUE)


  data_plot <- window(x, start = first_date, end = last_date)
  general_colors <- c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                      cal = "#F0B400", s = "#1E6C0B", i = "#155692")

  forecast <- forecast & tail(time(data_plot), 1) > time(na.omit(x[,"y_f"]))[1]

  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if("sa-trend" %in% type_chart){
    # Graph 1 : Sa, trend, and y
    series_graph <- c("y","t","sa")
    if(forecast){
      last_obs_date <- end(na.omit(x[,"y"]))
      window(data_plot[, paste0(series_graph,"_f")],
             start = last_obs_date,
             end = last_obs_date) <- window(data_plot[, series_graph],
                                            start = last_obs_date,
                                            end = last_obs_date)
      series_graph <- c(series_graph,
                        paste0(series_graph,"_f"))
    }
    lty <- rep(2, length(series_graph))
    lty[grep("_f$", series_graph, invert = TRUE)] <- 1
    col <- general_colors[gsub("_.*$", "", series_graph)]
    par(mar = c(5, 4, 4, 2) + 0.1)
    ts.plot(data_plot[, series_graph],
            col = col,
            main = "Sa, trend",lty = lty)
    legend("bottomleft", legend = c("Series", "Trend","Seasonally adjusted"),
           col = general_colors[c("y", "t", "sa")], lty = 1,
           pch = NA_integer_,
           inset=c(0,1), xpd=TRUE, bty="n")
    dev.flush()
  }

  if("cal-seas-irr" %in% type_chart){
    # Graph 2 : Calendar, seasonal and irregular
    series_graph <- c("s", "i")
    if(forecast){
      last_obs_date <- end(na.omit(x[,"y"]))
      window(data_plot[, paste0(series_graph,"_f")],
             start = last_obs_date,
             end = last_obs_date) <- window(data_plot[, series_graph],
                                            start = last_obs_date,
                                            end = last_obs_date)
      series_graph <- c(series_graph,
                        paste0(series_graph,"_f"))
    }
    lty <- rep(2, length(series_graph))
    lty[grep("_f$", series_graph, invert = TRUE)] <- 1
    col <- general_colors[gsub("_.*$", "", series_graph)]
    ts.plot(data_plot[, series_graph],
            col = col,
            main = "Cal., sea., irr.",lty = lty)
    legend("bottomleft", legend = c("Calendar effects",
                                    "Seas (component)",
                                    "Irregular"),
           col= general_colors[c("cal", "s", "i")], lty = 1,
           pch = NA_integer_,
           inset=c(0,1), xpd=TRUE, bty="n")
    dev.flush()
  }

  invisible()
}
