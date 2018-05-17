plot.Decomp_X13 = function(x, ...){
  if (!inherits(x, "Decomp_X13"))
    stop("use only with \"Decomp_X13\" object")
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

plot.Decomp_TS = function(x, ...){
  if (!inherits(x, "Decomp_TS"))
    stop("use only with \"Decomp_TS\" object")
  sln  <- x$linearized[,4]
  iln <- x$linearized[,5]
  mode <- "Additive"

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
}
