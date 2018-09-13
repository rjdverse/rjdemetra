#' @export
print.decomposition_X11=function (x, digits = max(3L, getOption("digits") - 3L), ...){
  m <- x$mstats
  s_flt <- x$s_filter
  t_flt <- x$t_filter
  if (!is.null(m)){
    cat("Monitoring and Quality Assessment Statistics:","\n")
    printCoefmat(m, digits = digits, P.values= FALSE, na.print = "NA", ...)
  }
  cat("\n")
  cat("Final filters:","\n")
  cat("Seasonal filter: ",s_flt)
  cat("\n")
  cat("Trend filter: ",t_flt)
  cat("\n")
}
#' @export
print.decomposition_SEATS=function (x, digits = max(3L, getOption("digits") - 3L),
                                    enable_print_style = TRUE,...){
  if(enable_print_style){
    bold_pre_code <- "\033[1m"
    bold_post_code <- "\033[22m"
  }else{
    bold_pre_code <-  bold_post_code <- ""
  }
  model <-x$model$model
  sa <- x$model$sa
  t <- x$model$trend
  s <- x$model$seasonal
  trans <- x$model$transitory
  i <- x$model$irregular

  var <- list(model, sa, t, s, trans, i)
  var_names <- c("Model","SA","Trend","Seasonal","Transitory","Irregular")

  for (ii in 1:length(var_names)){
    if (!all(sapply(var[[ii]],is.null))){
      cat(bold_pre_code,var_names[ii],bold_post_code,"\n", sep="")

      print_formula(var[[ii]][1,-1],"AR")
      print_formula(var[[ii]][2,-1],"D")
      print_formula(var[[ii]][3,-1],"MA")
      if (var[[ii]][4,1]==1) {
        cat("\n\n")
      }else{
        cat("Innovation variance: ",var[[ii]][4,1], "\n\n")
      }
    }
  }
}

print_formula <- function(x, var){
  non_0 <- which(x != 0 )
  if(length(non_0) == 0)
    return(NULL)
  polynome_degre <- paste0("B^", non_0)
  polynome_degre[non_0 == 1] <- "B"

  polynome_coef <- sprintf("%+f", x[non_0], polynome_degre)
  polynome_coef <- gsub("-","- ", polynome_coef)
  polynome_coef <- gsub("\\+","+ ", polynome_coef)
  polynome_coef[x[non_0] == -1] <- "-"
  polynome_coef[x[non_0] == 1] <- "+"

  polynome_formula <- paste(polynome_coef, polynome_degre, collapse = " ")
  polynome_formula <- paste("1", polynome_formula)
  cat(var,": ",polynome_formula,"\n")
}

#' @export
print.combined_test <- function(x, digits = max(3L, getOption("digits") - 3L),
                                  ...){
  tests_pval <- x$tests_for_stable_seasonality[,"P.value", drop = FALSE]

  cat("Non parametric tests for stable seasonality")
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(tests_pval, digits = digits,
                            na.print = "NA", ...)
             ),
             sep ="\n"))

  cat("\n")
  combined_test_result <- ngettext(match(x$combined_seasonality_test,
                                         c("Present","ProbablyNone","None")),
                                   "Identifiable seasonality present",
                                   "Identifiable seasonality probably present",
                                   "Identifiable seasonality not present")
  cat(combined_test_result)
  invisible(x)
}

#' @export
print.diagnostics = function (x, digits = max(3L, getOption("digits") - 3L),
                              enable_print_style = TRUE,
                                ...){

  if(enable_print_style){
    bold_pre_code <- "\033[1m"
    bold_post_code <- "\033[22m"
  }else{
    bold_pre_code <-  bold_post_code <- ""
  }
  residuals_test <- x$residuals_test
  combined_test_all <- x$combined_test_all
  combined_test_end <- x$combined_test_end
  variance_decomposition <- x$variance_decomposition

  cat(bold_pre_code,
      "Relative contribution of the components to the stationary portion of the variance in the original series, after the removal of the long term trend",
      bold_post_code)
  cat("\n")
  cat(" Trend computed by Hodrick-Prescott filter (cycle length = 8.0 years)")
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(variance_decomposition, digits = digits, ...)
             )),
      sep ="\n")
  cat("\n")
  cat(bold_pre_code,
      "Residual seasonality tests",
      bold_post_code)
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(residuals_test[,"P.value", drop = FALSE], digits = digits,
                            na.print = "NA", ...)
             )
  ),
  sep ="\n")

  cat("\n")

  cat(bold_pre_code,
      "Combined test in the entire series",
      bold_post_code)
  cat("\n")
  cat(paste0(" ",
             capture.output(print.combined_test(combined_test_all, digits = digits,
                                                  ...))
  ),
  sep ="\n")
  cat("\n")

  cat(bold_pre_code,
      "Combined test in the last 3 years",
      bold_post_code)
  cat("\n")
  cat(paste0(" ",
             capture.output(
               print.combined_test(combined_test_end, digits = digits,
                                     ...)
             )
  ),
  sep ="\n")

  invisible(x)
}

#' @export
print.final <- function(x, calendar, n_last_obs = frequency(x$series), print_forecasts = TRUE, ...){

  cat("Last observed values\n")
  print(tail(
    .preformat.ts(x[[1]], calendar = calendar),
    n_last_obs
  ))
  if(print_forecasts){
    cat("\nForecasts:\n")
    print(head(
      .preformat.ts(x[[1]], calendar = calendar),
      n_last_obs
    ))
  }
  invisible(x)
}
#' @export
print.user_defined <- function(x, ...){
  if(is.null(x) || length(x) == 0)
    return(invisible(x))
  cat(ngettext((length(x)!= 1) + 1,
               "One additional variable (",
               "Names of additional variables ("))
  cat(length(x),"):","\n", sep="")
  cat(names(x),sep = ", ")
  invisible(x)
}
#' @export
print.SA <- function(x, enable_print_style = TRUE,  ...){
  if(enable_print_style){
    style_pre_code <- "\033[4m\033[1m"
    style_post_code <- "\033[22m\033[24m"
  }else{
    style_pre_code <-  style_post_code <- ""
  }
  cat("\n\n", style_pre_code, "RegARIMA", style_post_code,"\n",sep="")
  print(x$regarima, enable_print_style = enable_print_style)
  cat("\n\n", style_pre_code, "Decomposition", style_post_code,"\n",sep="")
  print(x$decomposition, enable_print_style = enable_print_style)
  cat("\n\n", style_pre_code, "Final", style_post_code,"\n",sep="")
  print(x$final, enable_print_style = enable_print_style)
  cat("\n\n", style_pre_code, "Diagnostics", style_post_code,"\n",sep="")
  print(x$diagnostics, enable_print_style = enable_print_style)
  cat("\n\n", style_pre_code, "Additional output variables", style_post_code,"\n",sep="")
  print(x$user_defined)
}

