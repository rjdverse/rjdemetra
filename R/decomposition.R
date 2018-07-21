decomp_defTS <- function(jrobj,spec){
  # extract model specification from the java object
  rspec <- specSeats_jd2r( spec = spec)
  specification <- do.call(data.frame, rspec)
  names(specification) <- paste0("seats.",names(specification))
  rownames(specification) <- ""
  # results
  jd_results <- decomp_rsltsTS(jrobj)
  # new S3 class ("Decomp","TRAMO_SEATS")
  z<- list(specification = specification,
           mode = jd_results$mode,
           model = jd_results$model,
           linearized=jd_results$lin,
           components=jd_results$cmp)
  class(z) <- c("decomposition_SEATS")
  return(z)
}

decomp_TS <- function(jrobj, spec){
  # specification
  specification <- spec[3,]
  rownames(specification) <- ""
  # results
  jd_results <- decomp_rsltsTS(jrobj)
  # new S3 class ("Decomp","TRAMO_SEATS")
  z<- list(specification = specification,
           mode = jd_results$mode,
           model = jd_results$model,
           linearized=jd_results$lin,
           components=jd_results$cmp)
  class(z) <- c("decomposition_SEATS")
  return(z)
}

decomp_defX13 <- function(jrobj,spec){

  # extract model specification from the java object
  specification <- specX11_jd2r( spec = spec)
  # specification <- do.call(data.frame, rspec)
  # names(specification) <- sprintf("x11.%s",names(specification))
  rownames(specification) <- ""
  # results
  jd_results <- decomp_rsltsX13(jrobj)
  # new S3 class ("Decomp","X13")
  z<- list(specification = specification,
           mode = jd_results$mode,
           mstats =  jd_results$mstats,
           si_ratio = jd_results$si_ratio,
           s_filter = jd_results$s_filter,
           t_filter = jd_results$t_filter)
  class(z) <- c("decomposition_X11")
  return(z)
}
decomp_X13 <- function(jrobj,spec,seasma){

  # specification
  specification <- spec[3,]
  specification[[7]] <- seasma
  rownames(specification) <- ""
  # results
  jd_results <- decomp_rsltsX13(jrobj)
  # new S3 class ("Decomp","X13")
  z<- list(specification = specification,
           mode = jd_results$mode,
           mstats =  jd_results$mstats,
           si_ratio = jd_results$si_ratio,
           s_filter = jd_results$s_filter,
           t_filter = jd_results$t_filter)
  class(z) <- c("decomposition_X11")
  return(z)
}

decomp_rsltsX13 <- function(jrobj){

  mode <- result(jrobj,"mode")

  mstats_rownames <- c(sprintf("M(%s)", 1:10),
                       "Q", "Q-M2")
  mstats_names <- sprintf("mstats.%s", mstats_rownames)
  mstats <- lapply(mstats_names,
                   function(diag) {
                     result(jrobj, diag)})
  mstats <- matrix(unlist(mstats), ncol=1)

  rownames(mstats) <- mstats_rownames
  colnames(mstats) <- c("M stats")

  d8 <- result(jrobj,"decomposition.d8")
  d10 <- result(jrobj,"decomposition.d10")
  si_ratio <- cbind(d8=d8, d10=d10)

  s_filter <- result(jrobj,"decomposition.d9filter")
  t_filter <- result(jrobj,"decomposition.d12filter")

  z <- list(mode = mode, mstats =  mstats, si_ratio = si_ratio,
            s_filter = s_filter, t_filter = t_filter)
  return(z)
}

decomp_rsltsTS <- function( jrobj){

  mode <- result(jrobj,"mode")

  lin_colnames <- sprintf("%s_lin", c("y","sa","t","s","i"))
  cmp_colnames <- sprintf("%s_cmp", c("y","sa","t","s","i"))
  lin_names <- sprintf("decomposition.%s", lin_colnames)
  cmp_names <- sprintf("decomposition.%s", cmp_colnames)

  lin <- lapply(lin_names,
                function(diag) {
                  result(jrobj,diag)})
  lin <- do.call(cbind, lin)
  colnames(lin) <- lin_colnames

  cmp <- lapply(cmp_names,
                function(diag) {
                  result(jrobj,diag)})
  cmp <- do.call(cbind, cmp)
  colnames(cmp) <- cmp_colnames

  fmodel_names <- paste0("decomposition.model.",c("ar","diff","ma","innovationvariance"))
  samodel_names <- paste0("decomposition.samodel.",c("ar","diff","ma","innovationvariance"))
  tmodel_names <- paste0("decomposition.tmodel.",c("ar","diff","ma","innovationvariance"))
  smodel_names <- paste0("decomposition.smodel.",c("ar","diff","ma","innovationvariance"))
  trans_model_names <- paste0("decomposition.transitorymodel.",c("ar","diff","ma","innovationvariance"))
  imodel_names <- paste0("decomposition.imodel.",c("ar","diff","ma","innovationvariance"))
  rdsc <- c("AR","D","MA","Innovation variance")

  fmodel <- lapply(fmodel_names,
                   function(diag) {
                     res <- result(jrobj,diag)})
  if (!all(sapply(fmodel, is.null))){
    n <- max(length(fmodel[[1]]),length(fmodel[[2]]), length(fmodel[[3]]), length(fmodel[[4]]))
    for (i in 1:4) {length(fmodel[[i]]) <- n}
    fmodel <- do.call(rbind, fmodel)
    rownames(fmodel) <- rdsc
    colnames(fmodel) <- as.character(c(0:(dim(fmodel)[2]-1)))
  }

  samodel <- lapply(samodel_names,
                    function(diag) {
                      res <- result(jrobj,diag)})
  if (!all(sapply(samodel, is.null))){
    n <- max(length(samodel[[1]]),length(samodel[[2]]), length(samodel[[3]]), length(samodel[[4]]))
    for (i in 1:4) {length(samodel[[i]]) <- n}
    samodel <- do.call(rbind, samodel)
    rownames(samodel) <- rdsc
    colnames(samodel) <- as.character(c(0:(dim(samodel)[2]-1)))
  }

  tmodel <- lapply(tmodel_names,
                   function(diag) {
                     res <- result(jrobj,diag)})
  if (!all(sapply(tmodel, is.null))){
    n <- max(length(tmodel[[1]]),length(tmodel[[2]]), length(tmodel[[3]]), length(tmodel[[4]]))
    for (i in 1:4) {length(tmodel[[i]]) <- n}
    tmodel <- do.call(rbind, tmodel)
    rownames(tmodel) <- rdsc
    colnames(tmodel) <- as.character(c(0:(dim(tmodel)[2]-1)))
  }

  smodel <- lapply(smodel_names,
                   function(diag) {
                     res <- result(jrobj,diag)})
  if (!all(sapply(smodel, is.null))){
    n <- max(length(smodel[[1]]),length(smodel[[2]]), length(smodel[[3]]), length(smodel[[4]]))
    for (i in 1:4) {length(smodel[[i]]) <- n}
    smodel <- do.call(rbind, smodel)
    rownames(smodel) <- rdsc
    colnames(smodel) <- as.character(c(0:(dim(smodel)[2]-1)))
  }

  trans_model <- lapply(trans_model_names,
                        function(diag) {
                          res <- result(jrobj,diag)})
  if (!all(sapply(trans_model, is.null))){
    n <- max(length(trans_model[[1]]),length(trans_model[[2]]), length(trans_model[[3]]), length(trans_model[[4]]))
    for (i in 1:4) {length(trans_model[[i]]) <- n}
    trans_model <- do.call(rbind, trans_model)
    rownames(trans_model) <- rdsc
    colnames(trans_model) <- as.character(c(0:(dim(trans_model)[2]-1)))
  }

  imodel <- lapply(imodel_names,
                   function(diag) {
                     res <- result(jrobj,diag)})
  if (!all(sapply(imodel, is.null))){
    n <- max(length(imodel[[1]]),length(imodel[[2]]), length(imodel[[3]]), length(imodel[[4]]))
    for (i in 1:4) {length(imodel[[i]]) <- n}
    imodel <- do.call(rbind, imodel)
    rownames(imodel) <- rdsc
    colnames(imodel) <- as.character(c(0:(dim(imodel)[2]-1)))
  }
  model <- list(model = fmodel, sa = samodel, trend = tmodel, seasonal = smodel,
                transitory = trans_model, irregular = imodel)

  z <- list(mode = mode, lin = lin, cmp = cmp, model = model)
  return(z)
}

