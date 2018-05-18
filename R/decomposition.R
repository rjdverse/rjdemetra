decomp_defTS <- function(jrobj,spec){
  # extract model specification from the java object
  #  rspec <- specDecompTS_jd2r( spec = spec)
  # specification
  specification <- list
  # results
  jd_results <- decomp_rsltsTS(jrobj)
  # new S3 class ("Decomp","TRAMO_SEATS")
  z<- list(specification = specification,
           mode = jd_results$mode,
           model = jd_results$model,
           linearized=jd_results$lin,
           components=jd_results$cmp)
  class(z) <- c("Decomp_TS")
  return(z)
}

decomp_defX13 <- function(jrobj,spec){
  # extract model specification from the java object
  #  rspec <- specDecompX13_jd2r( spec = spec)
  # specification
  specification <- list
  # results
  jd_results <- decomp_rsltsX13(jrobj)
  # new S3 class ("Decomp","X13")
  z<- list(specification = specification,
           mode = jd_results$mode,
           mstats =  jd_results$mstats,
           si_ratio = jd_results$si_ratio,
           s_filter = jd_results$s_filter,
           t_filter = jd_results$t_filter)
  class(z) <- c("Decomp_X13")
  return(z)
}
decomp_rsltsX13 <- function(jrobj){

  mode <- result(jrobj,"mode")

  mstats_names <- c(paste0("mstats.M(",as.character(c(1:10)),")"),
                    paste0("mstats.",c("Q","Q-M2")))
  mstats <- sapply(mstats_names,
                   function(diag) {
                     res <- result(jrobj, diag)})
  mstats <- matrix(mstats, ncol=1)
  rownames(mstats) <- c(paste0("M",as.character(c(1:10))),
                        c("Q","Q-M2"))
  colnames(mstats) <- c("M stats")

  d8 <- result(jrobj,"decomposition.d8")
  d10 <- result(jrobj,"decomposition.d10")
  si_ratio <- cbind(d8=d8, d10=d10)

  s_filter <- result(jrobj,"decomposition.d9filter")
  t_filter <- result(jrobj,"decomposition.d12filter")

  z <- list(mode = mode, mstats =  mstats, si_ratio = si_ratio, s_filter = s_filter, t_filter = t_filter)
  return(z)
}

decomp_rsltsTS <- function( jrobj){

  mode <- result(jrobj,"mode")

  lin_names <- paste0("decomposition.", c("y","sa","t","s","i"),"_lin")
  cmp_names <- paste0("decomposition.", c("y","sa","t","s","i"),"_cmp")

  lin <- lapply(lin_names,
                function(diag) {
                  res <- result(jrobj,diag)})
  lin <- do.call(cbind,lin)
  colnames(lin) <- paste0(c("y","sa","t","s","i"),"_lin")

  cmp <- lapply(cmp_names,
                function(diag) {
                  res <- result(jrobj,diag)})
  cmp <- do.call(cbind,cmp)
  colnames(cmp) <- paste0(c("y","sa","t","s","i"),"_cmp")

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

