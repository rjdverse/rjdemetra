diagnostics <- function(jrobct, jd_clobj){
  variance_decomposition <- data.frame(Component =
                                         result(jd_clobj, jrobct, "diagnostics.variancedecomposition") * 100)
  rownames(variance_decomposition) <- c("Cycle", "Seasonal",
                                        "Irregular", "TD & Hol.",
                                        "Others", "Total")

  residuals_tests_names <- paste0("diagnostics.",
                                  c("qs", "ftest",
                                    "residual.all", "residual.end", "residualtd"))
  residuals_test <- lapply(residuals_tests_names,
                           function(diag) {
                             res <- result(jd_clobj, jrobct, diag)
                             data.frame(Statistic = res[1], P.value =  res[2],
                                        Description = attr(res, "description")
                             )
                           })
  residuals_test <- do.call(rbind, residuals_test)
  rownames(residuals_test) <- c("qs test on sa","f-test on sa (seasonal dummies)",
                                "Residual seasonality (entire series)",
                                "Residual seasonality (last 3 years)",
                                "f-test on sa (td)")
  combined_test_all <- combined_test(jd_clobj, jrobct, "all")
  combined_test_end <- combined_test(jd_clobj, jrobct, "end")
  diag <- list(variance_decomposition = variance_decomposition,
               residuals_test = residuals_test,
               combined_test_all = combined_test_all,
               combined_test_end = combined_test_end)
  class(diag) <- "diagnostics"
  diag
}

combined_test <- function(jd_clobj, jrobct, on = c("all", "end")){
  on <- match.arg(on)
  tests_names <- paste0("diagnostics.combined.", on, ".",
                        c("summary", "kruskalwallis", "stable", "evolutive"))
  tests <- lapply(tests_names[-1],
                  function(diag) {
                    res <- result(jd_clobj, jrobct, diag)
                    data.frame(Statistic = res[1], P.value =  res[2],
                               Description = attr(res, "description")
                    )
                  })
  tests <- do.call(rbind, tests)

  rownames(tests) <- c("Kruskall-Wallis test",
                       "Test for the presence of seasonality assuming stability",
                       "Evolutive seasonality test")
  combined_result <- result(jd_clobj, jrobct, tests_names[1])

  cb_test <- list(tests_for_stable_seasonality = tests,
                  combined_seasonality_test = combined_result)
  class(cb_test) <- "combined_test"
  cb_test
}

#' @export
summary.combined_test <- function(object, digits = max(3L, getOption("digits") - 3L),
                                  ...){
  tests_pval <- object$tests_for_stable_seasonality[,"P.value", drop = FALSE]

  cat("Non parametric tests for stable seasonality")
  cat("\n")
  cat(paste0(" ",
             utils::capture.output(
               stats::printCoefmat(tests_pval, digits = digits,
                            na.print = "NA", ...)
             ),
             sep ="\n"))

  cat("\n")
  combined_test_result <- ngettext(match(object$combined_seasonality_test,
                                         c("Present","ProbablyNone","None")),
                                   "Identifiable seasonality present",
                                   "Identifiable seasonality probably present",
                                   "Identifiable seasonality not present")
  cat(combined_test_result)
  invisible(object)
}

#' @export
summary.diagnostics = function (object, digits = max(3L, getOption("digits") - 3L),
                                ...){


  residuals_test <- object$residuals_test
  combined_test_all <- object$combined_test_all
  combined_test_end <- object$combined_test_end
  variance_decomposition <- object$variance_decomposition

  cat("\033[1mRelative contribution of the components to the stationary portion of the variance in the original series, after the removal of the long term trend\033[22m")
  cat("\n")
  cat(" Trend computed by Hodrick-Prescott filter (cycle length = 8.0 years)")
  cat("\n")
  cat(paste0(" ",
             utils::capture.output(
               stats::printCoefmat(variance_decomposition, digits = digits, ...)
             )),
             sep ="\n")
  cat("\n")
  cat("\033[1mResidual seasonality tests\033[22m")
  cat("\n")
  cat(paste0(" ",
             utils::capture.output(
               stats::printCoefmat(residuals_test[,"P.value", drop = FALSE], digits = digits,
                            na.print = "NA", ...)
             )
  ),
  sep ="\n")

  cat("\n")

  cat("\033[1mCombined test in the entire series\033[22m")
  cat("\n")
  cat(paste0(" ",
             utils::capture.output(summary.combined_test(combined_test_all, digits = digits,
                                                   ...))
  ),
  sep ="\n")
  cat("\n")

  cat("\033[1mCombined test in the last 3 years\033[22m")
  cat("\n")
  cat(paste0(" ",
             utils::capture.output(
                 summary.combined_test(combined_test_end, digits = digits,
                                       ...)
             )
  ),
  sep ="\n")

  invisible(object)
}
