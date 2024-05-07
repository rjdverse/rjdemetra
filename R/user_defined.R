user_defined <- function(namedvector, jrobct){
  if(is.null(namedvector)){
    result <- list()
  }else{
    result <- lapply(namedvector, function(x) result(jrobct, x))
    if (is.null(names(namedvector)))
      names(result) <- namedvector
  }
  class(result) <- c("user_defined")
  result
}
#' Display a list of all the available output objects (series, parameters, diagnostics)
#'
#'@description
#' Function generating a comprehensive list of available output variables (series, parameters, diagnostics) from the estimation process
#' with \code{\link{x13}} and \code{\link{tramoseats}}.
#' Some items are available in the default estimation output but the remainder can be added
#' using the \code{userdefined} parameter.
#'
#' @param sa_object a character: \code{"X13-ARIMA"} to retrieve the additional output variables available for the X13-ARIMA method
#' and \code{"TRAMO-SEATS"} for the TRAMO-SEATS method.
#'
#' @examples
#' y<- ipi_c_eu[, "FR"]
#' user_defined_variables("X13-ARIMA")
#' m <- x13(y,"RSA5c", userdefined=c("b20","ycal","residuals.kurtosis" ))
#' m$user_defined$b20
#' m$user_defined$ycal
#' m$user_defined$residuals.kurtosis
#' user_defined_variables("TRAMO-SEATS")
#' m <- tramoseats(y,"RSAfull", userdefined=c("ycal","variancedecomposition.seasonality"))
#' m$user_defined$ycal
#' m$user_defined$variancedecomposition.seasonality
#'
#' @return  a vector containing the names of all the available output objects (series, diagnostics, parameters)
#' @references
#' More information and examples related to 'JDemetra+' features in the online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
user_defined_variables <- function(sa_object = c("X13-ARIMA", "TRAMO-SEATS")){
  sa_object <- match.arg(sa_object)
  if(sa_object == "X13-ARIMA"){
    ## To get the variables:
    # library(rJava)
    # jrspec<-.jcall("jdr/spec/x13/X13Spec", "Ljdr/spec/x13/X13Spec;", "of", "RSA0")
    # jspec<-.jcall(jrspec, "Lec/satoolkit/x13/X13Specification;", "getCore")
    # jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
    # jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/X13Results;", "x13",
    #               RJDemetra:::ts_r2jd(ipi_c_eu[, "FR"]), jspec, jdictionary)
    # jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
    # jrobct_arima <- new (Class = "RegArima_java",internal = jrarima)
    # jrobct <- new (Class = "X13_java", internal = jrslt)
    #
    # dput(RJDemetra:::dictionary(jrobct))
    #

    vars <- c(
      "y", "y_f", "t", "t_f", "sa", "sa_f", "s", "s_f",
      "i", "i_f", "mode", "preprocessing.model.span.start", "preprocessing.model.span.end",
      "preprocessing.model.span.n", "preprocessing.model.espan.start",
      "preprocessing.model.espan.end", "preprocessing.model.espan.n",
      "preprocessing.model.log", "preprocessing.model.adjust", "preprocessing.model.y",
      "preprocessing.model.y_f", "preprocessing.model.y_ef", "preprocessing.model.yc",
      "preprocessing.model.yc_f", "preprocessing.model.yc_ef", "preprocessing.model.l",
      "preprocessing.model.y_lin", "preprocessing.model.y_lin_f", "preprocessing.model.ycal",
      "preprocessing.model.ycal_f", "preprocessing.model.det", "preprocessing.model.det_f",
      "preprocessing.model.l_f", "preprocessing.model.l_b", "preprocessing.model.cal",
      "preprocessing.model.cal_f", "preprocessing.model.tde", "preprocessing.model.tde_f",
      "preprocessing.model.mhe", "preprocessing.model.mhe_f", "preprocessing.model.ee",
      "preprocessing.model.ee_f", "preprocessing.model.omhe", "preprocessing.model.omhe_f",
      "preprocessing.model.out(*)", "preprocessing.model.out_f", "preprocessing.model.out_i",
      "preprocessing.model.out_i_f", "preprocessing.model.out_t", "preprocessing.model.out_t_f",
      "preprocessing.model.out_s", "preprocessing.model.out_s_f", "preprocessing.model.reg",
      "preprocessing.model.reg_f", "preprocessing.model.reg_t", "preprocessing.model.reg_t_f",
      "preprocessing.model.reg_s", "preprocessing.model.reg_s_f", "preprocessing.model.reg_i",
      "preprocessing.model.reg_i_f", "preprocessing.model.reg_sa",
      "preprocessing.model.reg_sa_f", "preprocessing.model.reg_y",
      "preprocessing.model.reg_y_f", "preprocessing.model.reg_u", "preprocessing.model.reg_u_f",
      "preprocessing.model.fullresiduals", "preprocessing.model.lp",
      "preprocessing.model.ntd", "preprocessing.model.nmh", "preprocessing.model.td(*)",
      "preprocessing.model.easter", "preprocessing.model.nout", "preprocessing.model.noutao",
      "preprocessing.model.noutls", "preprocessing.model.nouttc", "preprocessing.model.noutso",
      "preprocessing.model.coefficients", "preprocessing.model.description",
      "preprocessing.model.covar", "preprocessing.model.pcovar", "preprocessing.model.fcasts(?)",
      "preprocessing.model.bcasts(?)", "preprocessing.model.lin_fcasts(?)",
      "preprocessing.model.lin_bcasts(?)", "preprocessing.model.efcasts(?)",
      "preprocessing.arima.parameters", "preprocessing.arima.p", "preprocessing.arima.d",
      "preprocessing.arima.q", "preprocessing.arima.bp", "preprocessing.arima.bd",
      "preprocessing.arima.bq", "preprocessing.likelihood.neffectiveobs",
      "preprocessing.likelihood.np", "preprocessing.likelihood.logvalue",
      "preprocessing.likelihood.adjustedlogvalue", "preprocessing.likelihood.ssqerr",
      "preprocessing.likelihood.aic", "preprocessing.likelihood.aicc",
      "preprocessing.likelihood.bic", "preprocessing.likelihood.bicc",
      "preprocessing.likelihood.ser", "preprocessing.likelihood.ser-ml",
      "preprocessing.residuals.res", "preprocessing.residuals.mean",
      "preprocessing.residuals.skewness", "preprocessing.residuals.kurtosis",
      "preprocessing.residuals.dh", "preprocessing.residuals.lb", "preprocessing.residuals.lb2",
      "preprocessing.residuals.seaslb", "preprocessing.residuals.bp",
      "preprocessing.residuals.bp2", "preprocessing.residuals.seasbp",
      "preprocessing.residuals.nruns", "preprocessing.residuals.lruns",
      "mstats.M(*)", "mstats.Q", "mstats.Q-M2", "decomposition.a1",
      "decomposition.a1a", "decomposition.a1b", "decomposition.a6",
      "decomposition.a7", "decomposition.a8", "decomposition.a8t",
      "decomposition.a8s", "decomposition.a8i", "decomposition.a9",
      "decomposition.a9sa", "decomposition.a9u", "decomposition.a9ser",
      "decomposition.b1", "decomposition.b2", "decomposition.b3", "decomposition.b4",
      "decomposition.b5", "decomposition.b6", "decomposition.b7", "decomposition.b8",
      "decomposition.b9", "decomposition.b10", "decomposition.b11",
      "decomposition.b12", "decomposition.b13", "decomposition.b14",
      "decomposition.b15", "decomposition.b16", "decomposition.b17",
      "decomposition.b18", "decomposition.b19", "decomposition.b20",
      "decomposition.c1", "decomposition.c2", "decomposition.c3", "decomposition.c4",
      "decomposition.c5", "decomposition.c6", "decomposition.c7", "decomposition.c8",
      "decomposition.c9", "decomposition.c10", "decomposition.c11",
      "decomposition.c12", "decomposition.c13", "decomposition.c14",
      "decomposition.c15", "decomposition.c16", "decomposition.c17",
      "decomposition.c18", "decomposition.c19", "decomposition.c20",
      "decomposition.d1", "decomposition.d2", "decomposition.d3", "decomposition.d4",
      "decomposition.d5", "decomposition.d6", "decomposition.d7", "decomposition.d8",
      "decomposition.d9", "decomposition.d10", "decomposition.d10a",
      "decomposition.d10b", "decomposition.d11", "decomposition.d11a",
      "decomposition.d12", "decomposition.d12a", "decomposition.d13",
      "decomposition.d14", "decomposition.d15", "decomposition.d16",
      "decomposition.d16a", "decomposition.d16b", "decomposition.d18",
      "decomposition.d19", "decomposition.d20", "decomposition.e1",
      "decomposition.e2", "decomposition.e3", "decomposition.e11",
      "decomposition.y_cmp", "decomposition.t_cmp", "decomposition.i_cmp",
      "decomposition.s_cmp", "decomposition.sa_cmp", "decomposition.y_cmp_f",
      "decomposition.t_cmp_f", "decomposition.i_cmp_f", "decomposition.s_cmp_f",
      "decomposition.sa_cmp_f", "decomposition.d9filter", "decomposition.slen",
      "decomposition.d12filter", "decomposition.tlen", "diagnostics.qs",
      "diagnostics.ftest", "diagnostics.qs.on.i", "diagnostics.ftest.on.i",
      "diagnostics.combined.all.kruskalwallis", "diagnostics.combined.all.stable",
      "diagnostics.combined.all.evolutive", "diagnostics.combined.all.summary",
      "diagnostics.combined.all.stable.ssm", "diagnostics.combined.all.stable.ssr",
      "diagnostics.combined.all.stable.ssq", "diagnostics.combined.all.evolutive.ssm",
      "diagnostics.combined.all.evolutive.ssr", "diagnostics.combined.all.evolutive.ssq",
      "diagnostics.combined.end.kruskalwallis", "diagnostics.combined.end.stable",
      "diagnostics.combined.end.evolutive", "diagnostics.combined.end.summary",
      "diagnostics.combined.end.stable.ssm", "diagnostics.combined.end.stable.ssr",
      "diagnostics.combined.end.stable.ssq", "diagnostics.combined.end.evolutive.ssm",
      "diagnostics.combined.end.evolutive.ssr", "diagnostics.combined.end.evolutive.ssq",
      "diagnostics.combined.residual.all.kruskalwallis", "diagnostics.combined.residual.all.stable",
      "diagnostics.combined.residual.all.evolutive", "diagnostics.combined.residual.all.summary",
      "diagnostics.combined.residual.all.stable.ssm", "diagnostics.combined.residual.all.stable.ssr",
      "diagnostics.combined.residual.all.stable.ssq", "diagnostics.combined.residual.all.evolutive.ssm",
      "diagnostics.combined.residual.all.evolutive.ssr", "diagnostics.combined.residual.all.evolutive.ssq",
      "diagnostics.combined.residual.end.kruskalwallis", "diagnostics.combined.residual.end.stable",
      "diagnostics.combined.residual.end.evolutive", "diagnostics.combined.residual.end.summary",
      "diagnostics.combined.residual.end.stable.ssm", "diagnostics.combined.residual.end.stable.ssr",
      "diagnostics.combined.residual.end.stable.ssq", "diagnostics.combined.residual.end.evolutive.ssm",
      "diagnostics.combined.residual.end.evolutive.ssr", "diagnostics.combined.residual.end.evolutive.ssq",
      "diagnostics.residual.all", "diagnostics.residual.end", "diagnostics.residualtd",
      "diagnostics.residualtd.on.i", "diagnostics.variancedecomposition",
      "diagnostics.logstat", "diagnostics.levelstat", "diagnostics.fcast-insample-mean",
      "diagnostics.fcast-outsample-mean", "diagnostics.fcast-outsample-variance",
      "diagnostics.seas-lin-f", "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw",
      "diagnostics.seas-lin-friedman", "diagnostics.seas-lin-periodogram",
      "diagnostics.seas-lin-spectralpeaks", "diagnostics.seas-si-combined",
      "diagnostics.seas-si-evolutive", "diagnostics.seas-si-stable",
      "diagnostics.seas-res-f", "diagnostics.seas-res-qs", "diagnostics.seas-res-kw",
      "diagnostics.seas-res-friedman", "diagnostics.seas-res-periodogram",
      "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-res-combined",
      "diagnostics.seas-res-combined3", "diagnostics.seas-res-evolutive",
      "diagnostics.seas-res-stable", "diagnostics.seas-i-f", "diagnostics.seas-i-qs",
      "diagnostics.seas-i-kw", "diagnostics.seas-i-periodogram", "diagnostics.seas-i-spectralpeaks",
      "diagnostics.seas-i-combined", "diagnostics.seas-i-combined3",
      "diagnostics.seas-i-evolutive", "diagnostics.seas-i-stable",
      "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw",
      "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram",
      "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-sa-combined",
      "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive",
      "diagnostics.seas-sa-stable", "diagnostics.seas-sa-ac1", "diagnostics.td-sa-all",
      "diagnostics.td-sa-last", "diagnostics.td-i-all", "diagnostics.td-i-last",
      "diagnostics.td-res-all", "diagnostics.td-res-last", "diagnostics.ic-ratio-henderson",
      "diagnostics.ic-ratio", "diagnostics.msr-global", "diagnostics.msr(*)",
      "coherence.annualtotals.value", "coherence.annualtotals", "coherence.definition.value",
      "coherence.definition", "residuals.normality.value", "residuals.normality",
      "residuals.independence.value", "residuals.independence", "residuals.tdpeaks.value",
      "residuals.tdpeaks", "residuals.seaspeaks.value", "residuals.seaspeaks"
    )
  } else {
    # # To get the variables :
    # library(rJava)
    # jrspec<-.jcall("jdr/spec/tramoseats/TramoSeatsSpec", "Ljdr/spec/tramoseats/TramoSeatsSpec;", "of", "RSA0")
    # jspec<-.jcall(jrspec, "Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "getCore")
    # jdictionary <- .jnew("jdr/spec/ts/Utility$Dictionary")
    # jrslt<-.jcall("ec/tstoolkit/jdr/sa/Processor", "Lec/tstoolkit/jdr/sa/TramoSeatsResults;", "tramoseats",
    #               RJDemetra:::ts_r2jd(ipi_c_eu[, "FR"]), jspec, jdictionary)
    # jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;", "regarima")
    # jrobct_arima <- new (Class = "TRAMO_java",internal = jrarima)
    # jrobct <- new (Class = "TramoSeats_java", internal = jrslt)
    #
    # dput(RJDemetra:::dictionary(jrobct))

    vars <- c(
      "y", "y_f", "t", "t_f", "sa", "sa_f", "s", "s_f",
              "i", "i_f", "mode", "preprocessing.model.span.start", "preprocessing.model.span.end",
              "preprocessing.model.span.n", "preprocessing.model.espan.start",
              "preprocessing.model.espan.end", "preprocessing.model.espan.n",
              "preprocessing.model.log", "preprocessing.model.adjust", "preprocessing.model.y",
              "preprocessing.model.y_f", "preprocessing.model.y_ef", "preprocessing.model.yc",
              "preprocessing.model.yc_f", "preprocessing.model.yc_ef", "preprocessing.model.l",
              "preprocessing.model.y_lin", "preprocessing.model.y_lin_f", "preprocessing.model.ycal",
              "preprocessing.model.ycal_f", "preprocessing.model.det", "preprocessing.model.det_f",
              "preprocessing.model.l_f", "preprocessing.model.l_b", "preprocessing.model.cal",
              "preprocessing.model.cal_f", "preprocessing.model.tde", "preprocessing.model.tde_f",
              "preprocessing.model.mhe", "preprocessing.model.mhe_f", "preprocessing.model.ee",
              "preprocessing.model.ee_f", "preprocessing.model.omhe", "preprocessing.model.omhe_f",
              "preprocessing.model.out(*)", "preprocessing.model.out_f", "preprocessing.model.out_i",
              "preprocessing.model.out_i_f", "preprocessing.model.out_t", "preprocessing.model.out_t_f",
              "preprocessing.model.out_s", "preprocessing.model.out_s_f", "preprocessing.model.reg",
              "preprocessing.model.reg_f", "preprocessing.model.reg_t", "preprocessing.model.reg_t_f",
              "preprocessing.model.reg_s", "preprocessing.model.reg_s_f", "preprocessing.model.reg_i",
              "preprocessing.model.reg_i_f", "preprocessing.model.reg_sa",
              "preprocessing.model.reg_sa_f", "preprocessing.model.reg_y",
              "preprocessing.model.reg_y_f", "preprocessing.model.reg_u", "preprocessing.model.reg_u_f",
              "preprocessing.model.fullresiduals", "preprocessing.model.lp",
              "preprocessing.model.ntd", "preprocessing.model.nmh", "preprocessing.model.td(*)",
              "preprocessing.model.easter", "preprocessing.model.nout", "preprocessing.model.noutao",
              "preprocessing.model.noutls", "preprocessing.model.nouttc", "preprocessing.model.noutso",
              "preprocessing.model.coefficients", "preprocessing.model.description",
              "preprocessing.model.covar", "preprocessing.model.pcovar", "preprocessing.model.fcasts(?)",
              "preprocessing.model.bcasts(?)", "preprocessing.model.lin_fcasts(?)",
              "preprocessing.model.lin_bcasts(?)", "preprocessing.model.efcasts(?)",
              "preprocessing.arima.parameters", "preprocessing.arima.p", "preprocessing.arima.d",
              "preprocessing.arima.q", "preprocessing.arima.bp", "preprocessing.arima.bd",
              "preprocessing.arima.bq", "preprocessing.likelihood.neffectiveobs",
              "preprocessing.likelihood.np", "preprocessing.likelihood.logvalue",
              "preprocessing.likelihood.adjustedlogvalue", "preprocessing.likelihood.ssqerr",
              "preprocessing.likelihood.aic", "preprocessing.likelihood.aicc",
              "preprocessing.likelihood.bic", "preprocessing.likelihood.bicc",
              "preprocessing.likelihood.ser", "preprocessing.likelihood.ser-ml",
              "preprocessing.residuals.res", "preprocessing.residuals.mean",
              "preprocessing.residuals.skewness", "preprocessing.residuals.kurtosis",
              "preprocessing.residuals.dh", "preprocessing.residuals.lb", "preprocessing.residuals.lb2",
              "preprocessing.residuals.seaslb", "preprocessing.residuals.bp",
              "preprocessing.residuals.bp2", "preprocessing.residuals.seasbp",
              "preprocessing.residuals.nruns", "preprocessing.residuals.lruns",
              "decomposition.y_lin", "decomposition.y_lin_f", "decomposition.y_lin_ef",
              "decomposition.t_lin", "decomposition.t_lin_f", "decomposition.t_lin_e",
              "decomposition.t_lin_ef", "decomposition.sa_lin", "decomposition.sa_lin_f",
              "decomposition.sa_lin_e", "decomposition.sa_lin_ef", "decomposition.s_lin",
              "decomposition.s_lin_f", "decomposition.s_lin_e", "decomposition.s_lin_ef",
              "decomposition.i_lin", "decomposition.i_lin_f", "decomposition.i_lin_e",
              "decomposition.i_lin_ef", "decomposition.y_cmp", "decomposition.y_cmp_f",
              "decomposition.t_cmp", "decomposition.t_cmp_f", "decomposition.sa_cmp",
              "decomposition.sa_cmp_f", "decomposition.s_cmp", "decomposition.s_cmp_f",
              "decomposition.i_cmp", "decomposition.i_cmp_f", "decomposition.i_cmp_e",
              "decomposition.t_cmp_e", "decomposition.s_cmp_e", "decomposition.sa_cmp_e",
              "decomposition.i_cmp_ef", "decomposition.t_cmp_ef", "decomposition.s_cmp_ef",
              "decomposition.sa_cmp_ef", "decomposition.parameterscutoff",
              "decomposition.modelchanged", "decomposition.model.ar", "decomposition.model.diff",
              "decomposition.model.ma", "decomposition.model.fullar", "decomposition.model.innovationvariance",
              "decomposition.tmodel.ar", "decomposition.tmodel.diff", "decomposition.tmodel.ma",
              "decomposition.tmodel.fullar", "decomposition.tmodel.innovationvariance",
              "decomposition.smodel.ar", "decomposition.smodel.diff", "decomposition.smodel.ma",
              "decomposition.smodel.fullar", "decomposition.smodel.innovationvariance",
              "decomposition.samodel.ar", "decomposition.samodel.diff", "decomposition.samodel.ma",
              "decomposition.samodel.fullar", "decomposition.samodel.innovationvariance",
              "decomposition.transitorymodel.ar", "decomposition.transitorymodel.diff",
              "decomposition.transitorymodel.ma", "decomposition.transitorymodel.fullar",
              "decomposition.transitorymodel.innovationvariance", "decomposition.imodel.ar",
              "decomposition.imodel.diff", "decomposition.imodel.ma", "decomposition.imodel.fullar",
              "decomposition.imodel.innovationvariance", "diagnostics.qs",
              "diagnostics.ftest", "diagnostics.qs.on.i", "diagnostics.ftest.on.i",
              "diagnostics.combined.all.kruskalwallis", "diagnostics.combined.all.stable",
              "diagnostics.combined.all.evolutive", "diagnostics.combined.all.summary",
              "diagnostics.combined.all.stable.ssm", "diagnostics.combined.all.stable.ssr",
              "diagnostics.combined.all.stable.ssq", "diagnostics.combined.all.evolutive.ssm",
              "diagnostics.combined.all.evolutive.ssr", "diagnostics.combined.all.evolutive.ssq",
              "diagnostics.combined.end.kruskalwallis", "diagnostics.combined.end.stable",
              "diagnostics.combined.end.evolutive", "diagnostics.combined.end.summary",
              "diagnostics.combined.end.stable.ssm", "diagnostics.combined.end.stable.ssr",
              "diagnostics.combined.end.stable.ssq", "diagnostics.combined.end.evolutive.ssm",
              "diagnostics.combined.end.evolutive.ssr", "diagnostics.combined.end.evolutive.ssq",
              "diagnostics.combined.residual.all.kruskalwallis", "diagnostics.combined.residual.all.stable",
              "diagnostics.combined.residual.all.evolutive", "diagnostics.combined.residual.all.summary",
              "diagnostics.combined.residual.all.stable.ssm", "diagnostics.combined.residual.all.stable.ssr",
              "diagnostics.combined.residual.all.stable.ssq", "diagnostics.combined.residual.all.evolutive.ssm",
              "diagnostics.combined.residual.all.evolutive.ssr", "diagnostics.combined.residual.all.evolutive.ssq",
              "diagnostics.combined.residual.end.kruskalwallis", "diagnostics.combined.residual.end.stable",
              "diagnostics.combined.residual.end.evolutive", "diagnostics.combined.residual.end.summary",
              "diagnostics.combined.residual.end.stable.ssm", "diagnostics.combined.residual.end.stable.ssr",
              "diagnostics.combined.residual.end.stable.ssq", "diagnostics.combined.residual.end.evolutive.ssm",
              "diagnostics.combined.residual.end.evolutive.ssr", "diagnostics.combined.residual.end.evolutive.ssq",
              "diagnostics.residual.all", "diagnostics.residual.end", "diagnostics.residualtd",
              "diagnostics.residualtd.on.i", "diagnostics.variancedecomposition",
              "diagnostics.logstat", "diagnostics.levelstat", "diagnostics.fcast-insample-mean",
              "diagnostics.fcast-outsample-mean", "diagnostics.fcast-outsample-variance",
              "diagnostics.seas-lin-f", "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw",
              "diagnostics.seas-lin-friedman", "diagnostics.seas-lin-periodogram",
              "diagnostics.seas-lin-spectralpeaks", "diagnostics.seas-si-combined",
              "diagnostics.seas-si-evolutive", "diagnostics.seas-si-stable",
              "diagnostics.seas-res-f", "diagnostics.seas-res-qs", "diagnostics.seas-res-kw",
              "diagnostics.seas-res-friedman", "diagnostics.seas-res-periodogram",
              "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-res-combined",
              "diagnostics.seas-res-combined3", "diagnostics.seas-res-evolutive",
              "diagnostics.seas-res-stable", "diagnostics.seas-i-f", "diagnostics.seas-i-qs",
              "diagnostics.seas-i-kw", "diagnostics.seas-i-periodogram", "diagnostics.seas-i-spectralpeaks",
              "diagnostics.seas-i-combined", "diagnostics.seas-i-combined3",
              "diagnostics.seas-i-evolutive", "diagnostics.seas-i-stable",
              "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw",
              "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram",
              "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-sa-combined",
              "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive",
              "diagnostics.seas-sa-stable", "diagnostics.seas-sa-ac1", "diagnostics.td-sa-all",
              "diagnostics.td-sa-last", "diagnostics.td-i-all", "diagnostics.td-i-last",
              "diagnostics.td-res-all", "diagnostics.td-res-last", "diagnostics.ic-ratio-henderson",
              "diagnostics.ic-ratio", "diagnostics.msr-global", "diagnostics.msr(*)",
              "coherence.annualtotals.value", "coherence.annualtotals", "coherence.definition.value",
              "coherence.definition", "residuals.normality.value", "residuals.normality",
              "residuals.independence.value", "residuals.independence", "residuals.tdpeaks.value",
              "residuals.tdpeaks", "residuals.seaspeaks.value", "residuals.seaspeaks"
    )
  }
  vars
}
