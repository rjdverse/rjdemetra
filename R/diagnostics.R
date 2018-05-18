diagnostics <- function(jrobj){
  variance_decomposition <- data.frame(Component =
                                         result(jrobj,"diagnostics.variancedecomposition") * 100)
  rownames(variance_decomposition) <- c("Cycle", "Seasonal",
                                        "Irregular", "TD & Hol.",
                                        "Others", "Total")

  residuals_tests_names <- paste0("diagnostics.",
                                  c("qs", "ftest",
                                    "residual.all", "residual.end", "residualtd"))
  residuals_test <- lapply(residuals_tests_names,
                           function(diag) {
                             res <- result(jrobj, diag)
                             data.frame(Statistic = res[1], P.value =  res[2],
                                        Description = attr(res, "description")
                             )
                           })
  residuals_test <- do.call(rbind, residuals_test)
  rownames(residuals_test) <- c("qs test on sa","f-test on sa (seasonal dummies)",
                                "Residual seasonality (entire series)",
                                "Residual seasonality (last 3 years)",
                                "f-test on sa (td)")
  combined_test_all <- combined_test(jrobj, "all")
  combined_test_end <- combined_test(jrobj, "end")
  diag <- list(variance_decomposition = variance_decomposition,
               residuals_test = residuals_test,
               combined_test_all = combined_test_all,
               combined_test_end = combined_test_end)
  class(diag) <- "diagnostics"
  diag
}

combined_test <- function(jrobj, on = c("all", "end")){
  on <- match.arg(on)
  tests_names <- paste0("diagnostics.combined.", on, ".",
                        c("summary", "kruskalwallis", "stable", "evolutive"))
  tests <- lapply(tests_names[-1],
                  function(diag) {
                    res <- result(jrobj,diag)
                    data.frame(Statistic = res[1], P.value =  res[2],
                               Description = attr(res, "description")
                    )
                  })
  tests <- do.call(rbind, tests)

  rownames(tests) <- c("Kruskall-Wallis test",
                       "Test for the presence of seasonality assuming stability",
                       "Evolutive seasonality test")
  combined_result <- result(jrobj,tests_names[1])

  cb_test <- list(tests_for_stable_seasonality = tests,
                  combined_seasonality_test = combined_result)
  class(cb_test) <- "combined_test"
  cb_test
}
