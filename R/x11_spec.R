
x11_spec <- function(spec,
                     x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive", "PseudoAdditive"),
                     x11.seasonalComp = NA,
                     x11.lsigma = NA_integer_,
                     x11.usigma = NA_integer_,
                     x11.trendAuto = NA,
                     x11.trendma = NA_integer_,
                     x11.seasonalma = NA_character_,
                     x11.fcasts = NA_integer_,
                     x11.bcasts = NA_integer_,
                     x11.calendarSigma = NA,
                     x11.sigmaVector = NA,
                     x11.excludeFcasts = NA)
{
    x11.mode <- match.arg(x11.mode)

    x11.seasonalma <- spec_seasma(x11.seasonalma)
    x11.trendma <- spec_trendma(x11.trendma)
    calendarSigma_spec <- spec_calendar_sigma(calendarSigma = x11.calendarSigma,
                                              sigmaVector = x11.sigmaVector)
    x11.calendarSigma <- calendarSigma_spec$calendarSigma
    x11.sigmaVector <- calendarSigma_spec$sigmaVector

    list.logical <- list("x11.seasonalComp", "x11.trendAuto", "x11.excludeFcasts")
    list.numeric <- list("x11.lsigma", "x11.usigma", "x11.fcasts", "x11.bcasts")

    var.list <- list()
    for (i in 1:length(list.logical)) {
        eval(parse(text = paste("if( !is.logical(",
                                list.logical[i],
                                ")) {",
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
                                list.numeric[i],"')}",
                                sep = "")))
    }
    if (length(var.list) > 0) {
        warning(paste("Variable(s)",
                      deparse(as.character(var.list)),
                      " should be numeric. They are ignored."),
                call. = FALSE)
    }

    # modifed values
    # x11 <- do.call(data.frame, as.list(match.call()[c(-1, -2)]))
    x11 <- data.frame(x11.mode = x11.mode, x11.seasonalComp = x11.seasonalComp,
                      x11.lsigma = x11.lsigma,  x11.usigma = x11.usigma,
                      x11.trendAuto = x11.trendAuto, x11.trendma = x11.trendma,
                      x11.seasonalma = x11.seasonalma,
                      x11.fcasts = x11.fcasts, x11.bcasts = x11.bcasts,
                      x11.calendarSigma = x11.calendarSigma, x11.sigmaVector = x11.sigmaVector,
                      x11.excludeFcasts = x11.excludeFcasts)
    x11.spec <- s_x11(spec)
    x11.mod <- rbind(x11.spec, x11, rep(NA, length(x11.spec)))
    z <- spec_x11(x11.mod)

    class(z) <- c("X11_spec","data.frame")
    return(z)
}

x11_spec_def <- function(spec = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c","X11"),
                         x11.mode = c(NA_character_,"Undefined","Additive","Multiplicative","LogAdditive", "PseudoAdditive"),
                         x11.seasonalComp = NA,
                         x11.lsigma = NA_integer_,
                         x11.usigma = NA_integer_,
                         x11.trendAuto = NA,
                         x11.trendma = NA_integer_,
                         x11.seasonalma = NA_character_,
                         x11.fcasts = NA_integer_,
                         x11.bcasts = NA_integer_,
                         x11.calendarSigma = NA,
                         x11.sigmaVector = NA,
                         x11.excludeFcasts = NA)
{
    spec <- match.arg(spec)
    x11.mode <- match.arg(x11.mode)

    x11.seasonalma <- spec_seasma(x11.seasonalma)
    x11.trendma <- spec_trendma(x11.trendma)
    calendarSigma_spec <- spec_calendar_sigma(calendarSigma = x11.calendarSigma,
                                              sigmaVector = x11.sigmaVector)
    x11.calendarSigma <- calendarSigma_spec$calendarSigma
    x11.sigmaVector <- calendarSigma_spec$sigmaVector

    list.logical <- list("x11.seasonalComp", "x11.trendAuto", "x11.excludeFcasts")
    list.numeric <- list("x11.lsigma", "x11.usigma", "x11.fcasts", "x11.bcasts")

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

    # modifed values
    # x11 <- do.call(data.frame, as.list(match.call()[c(-1, -2)]))
    x11 <- data.frame(x11.mode = x11.mode, x11.seasonalComp = x11.seasonalComp,
                      x11.lsigma = x11.lsigma,  x11.usigma = x11.usigma,
                      x11.trendAuto = x11.trendAuto, x11.trendma = x11.trendma,
                      x11.seasonalma = x11.seasonalma,
                      x11.fcasts = x11.fcasts, x11.bcasts = x11.bcasts,
                      x11.calendarSigma = x11.calendarSigma, x11.sigmaVector = x11.sigmaVector,
                      x11.excludeFcasts = x11.excludeFcasts)
    # create the java object
    jrspec <- .jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", spec)
    x11.spec <- specX11_jd2r(spec = jrspec)
    # x11.spec <- do.call(data.frame, rspec)
    # names(x11.spec) <- paste0("x11.",names(x11.spec))
    x11.mod <- rbind(x11.spec, x11, rep(NA, length(x11.spec)))
    z <- spec_x11(x11.mod)

    class(z) <- c("X11_spec", "data.frame")
    return(z)
}
