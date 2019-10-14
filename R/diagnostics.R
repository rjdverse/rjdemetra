diagnostics <- function(jrobj){
  variance_decomposition <- data.frame(Component =
                                         result(jrobj,"diagnostics.variancedecomposition") * 100)
  if (nrow(variance_decomposition) > 0)
    rownames(variance_decomposition) <- c("Cycle", "Seasonal",
                                          "Irregular", "TD & Hol.",
                                          "Others", "Total")

  residuals_tests_names <- sprintf("diagnostics.%s",
                                  c("qs","qs.on.i", "ftest", "ftest.on.i",
                                    "residual.all", "residual.end",
                                    "residualtd","residualtd.on.i"))
  residuals_test <- lapply(residuals_tests_names,
                           function(diag) {
                             res <- result(jrobj, diag)
                             if (is.null(res)) {
                               c(NA, NA, NA)
                             }else{
                               c(res[1], res[2], attr(res, "description"))
                             }

                           })
  residuals_test <- data.frame(matrix(unlist(residuals_test), ncol = 3, byrow = TRUE),
                      stringsAsFactors = FALSE)
  residuals_test[,1] <- as.numeric(residuals_test[,1])
  residuals_test[,2] <- as.numeric(residuals_test[,2])
  colnames(residuals_test) <- c("Statistic","P.value","Description")
  rownames(residuals_test) <- c("qs test on sa", "qs test on i",
                                "f-test on sa (seasonal dummies)",
                                "f-test on i (seasonal dummies)",
                                "Residual seasonality (entire series)",
                                "Residual seasonality (last 3 years)",
                                "f-test on sa (td)",
                                "f-test on i (td)")

  combined_test_all <- combined_test(jrobj, "all")
  diag <- list(variance_decomposition = variance_decomposition,
               combined_test = combined_test_all,
               residuals_test = residuals_test)
  class(diag) <- "diagnostics"
  diag
}

combined_test <- function(jrobj, on = c("all", "end")){
  on <- match.arg(on)
  tests_names <- sprintf("diagnostics.combined.%s.%s", on,
                        c("summary", "kruskalwallis", "stable", "evolutive"))
  tests <- lapply(tests_names[-1],
                  function(diag) {
                    res <- result(jrobj,diag)
                    c(res[1], res[2], attr(res, "description"))
                  })
  tests <- data.frame(matrix(unlist(tests), ncol = 3, byrow = TRUE),
                               stringsAsFactors = FALSE)
  tests[,1] <- as.numeric(tests[,1])
  tests[,2] <- as.numeric(tests[,2])
  colnames(tests) <- c("Statistic","P.value","Description")
  rownames(tests) <- c("Kruskall-Wallis test",
                       "Test for the presence of seasonality assuming stability",
                       "Evolutive seasonality test")
  combined_result <- result(jrobj, tests_names[1])

  cb_test <- list(tests_for_stable_seasonality = tests,
                  combined_seasonality_test = combined_result)
  class(cb_test) <- "combined_test"
  cb_test
}
