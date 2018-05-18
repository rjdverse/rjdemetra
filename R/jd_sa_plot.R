#' Plotting decomposition or final results of a seasonal adjustment
#'
#' Plotting methods for objects around the seasonal adjustment : "JD_RegArima" for regARIMA,"Decomp_X13" and "Decomp_TS" for the decomposition with X13 and TRAMO-SEATS, "final" for the final results and "SA" for the entire seasonal adjusment object. The function \code{plot.SA} just calls the function \code{plot.final}.
#'
#' @param x the object to plot.
#' @param first_date the first date to start the plot. If missing the plot starts at the beginning of the time-series.
#' @param last_date the last date to end the plot. If missing the plot ends at the end of the time-series (eventually, including forecast).
#' @param type_chart character vector indicating which type of chart to plot.
#' @param forecast logical indicating if forecasts should be included in the plot.
#' @param ask logical value. If TRUE, the user will in future be prompted before a new graphical page is started.
#' @param ... other parameters.
#' @param which XXXXXX
#' @param dec_zoom XXXXXX
#' @param caption XXXXXX
#' @param sub.caption XXXXXX
#' @param main main title.
#' @param qqline XXXXXX
#' @param cex.caption XXXXXX
#' @param cex.oma.main XXXXXX
#' @name plot
#' @rdname plot
#' @export
plot.Decomp_X13 = function(x, first_date, last_date, ...){
  if (!inherits(x, "Decomp_X13"))
    stop("use only with \"Decomp_X13\" object")

  if(!missing(first_date)){
    x$si_ratio <- window(x$si_ratio, start = first_date)
  }
  if(!missing(last_date)){
    x$si_ratio <- window(x$si_ratio, end = last_date)
  }

  d8  <- x$si_ratio[,1]
  d10 <- x$si_ratio[,2]

  op <- par()
  freq <- frequency(d10)
  m <- matrix(c(1:freq), nrow = 1, ncol = freq)
  layout(m,  widths = c(1,rep.int(0.8, ncol(m)-1)))
  par(cex=0.7, mai=c(0.4,0.3,0.1,0))
  y_min <- min(d8,d10)
  y_max <- max(d8,d10)
  y_lim <- c(y_min - abs(y_min)*0.2, y_max + abs(y_max)*0.2)
  if (freq==12){
    leg<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  }else if (freq==4){
    leg<- c("Q1","Q2","Q3","Q4")
  }else if (freq==2) {
    leg<-c("H1","H2")
  } else {
    leg=c("")
  }
  yaxt="s"
  for (i in 1:freq){
    mm <- cycle(d10)==i
    xd10 <- d10[mm]
    xd8 <- d8[mm]
    plot(xd10, type="l", col="blue", lty=1, xaxt="n", yaxt = yaxt, ylim=y_lim)
    points(xd8, col="red", pch="*", xlab="")
    lines(rep(mean(xd10),length(xd10)), col="black", type="l", lty=1)
    legend(x = "bottom", legend=leg[i],inset = 0, bty="n")
    yaxt = "n"
    par(cex=0.7, mai=c(0.4,0,0.1,0))
    ylab=""
  }
  mtext("S-I ratio", side = 1, cex = 0.7, col = "blue")
  par(cex = op$cex, mai = op$mai, mfcol = op$mfcol, mfrow = op$mfrow)
}

#' @name plot
#' @rdname plot
#' @export
plot.Decomp_TS = function(x, first_date, last_date, ...){
  if (!inherits(x, "Decomp_TS"))
    stop("use only with \"Decomp_TS\" object")

  if(!missing(first_date)){
    x$components <- window(x$components, start = first_date)
  }
  if(!missing(last_date)){
    x$components <- window(x$components, end = last_date)
  }

  sln  <- x$components[,4]
  iln <- x$components[,5]
  mode <- x$mode

  z <- if (mode == "Additive") {sln+iln} else {sln*iln}

  op <- par()
  freq <- frequency(z)
  m <- matrix(c(1:freq), nrow = 1, ncol = freq)
  layout(m,  widths = c(1,rep.int(0.8, ncol(m)-1)))
  par(cex=0.7, mai=c(0.4,0.3,0.1,0))
  y_min <- min(z)
  y_max <- max(z)
  y_lim <- c(y_min - abs(y_min)*0.2, y_max + abs(y_max)*0.2)
  if (freq==12){
    leg<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  }else if (freq==4){
    leg<- c("Q1","Q2","Q3","Q4")
  }else if (freq==2) {
    leg<-c("H1","H2")
  } else {
    leg=c("")
  }
  yaxt="s"
  for (i in 1:freq){
    mm <- cycle(z)==i
    zm <- z[mm]
    plot(zm, type="l", col="blue", lty=1, xaxt="n", yaxt = yaxt, ylim=y_lim)
    lines(rep(mean(zm),length(zm)), col="black", type="l", lty=1)
    points(zm, col="red", pch="*", xlab="")
    legend(x = "bottom", legend=leg[i],inset = 0, bty="n")
    yaxt = "n"
    par(cex=0.7, mai=c(0.4,0,0.1,0))
    ylab=""
  }
  mtext("S-I ratio", side = 1, cex = 0.7, col = "blue")
  par(cex = op$cex, mai = op$mai, mfcol = op$mfcol, mfrow = op$mfrow)
  invisible()
}
#' @name plot
#' @rdname plot
#' @export
plot.final <- function(x, first_date, last_date, forecast = TRUE,
                       type_chart = c("sa-trend", "cal-seas-irr"),
                       ask =  length(type_chart) > 1 && dev.interactive(),
                       ...){
  type_chart <- match.arg(type_chart, several.ok = TRUE)


  data_plot <- x
  if(!missing(first_date)){
    data_plot <- window(data_plot, start = first_date)
  }
  if(!missing(last_date)){
    data_plot <- window(data_plot, end = last_date)
  }
  general_colors <- c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                      cal = "#F0B400", s = "#1E6C0B", i = "#155692")

  forecast <- forecast & tail(time(data_plot), 1) > time(na.omit(x[,"y_f"]))[1]


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

  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
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

#' @name plot
#' @rdname plot
#' @export
plot.SA <- function(x, ...){
  plot(x$final, ...)
  invisible()
}

