#' Doornik-Hansen normality test
#'
#' Function compute the Doornik-Hansen normality test  on RegARIMA residuals
#'
#' @param x  object of class \code{"regarima"} or \code{"SA"}.
#'
#' @examples
#' mysa1 <- tramoseats_def(ipi_c_eu[, "FR"], spec = "RSAfull")
#' 
#' # Actual tests :
#' mysa1$regarima$residuals.stat$tests
#' 
#' #Doornik Hansen test:
#' doornik_hansen_test(mysa1)
#' #Equivalent to:
#' doornik_hansen_test(mysa1$regarima)
#' @export
doornik_hansen_test <- function(x){
  UseMethod("doornik_hansen_test", x)
}
#' @export
doornik_hansen_test.regarima <- function(x){
  tests <- x$residuals.stat[["tests"]]
  dh <- doornik_hansen(n = x$loglik["neffectiveobs",],
                       skewness = tests["skewness", "Statistic"],
                       kurtosis = tests["kurtosis", "Statistic"])
  dh
}
#' @export
doornik_hansen_test.SA <- function(x){
  doornik_hansen_test.regarima(x$regarima)
}
doornik_hansen <- function(n, skewness, kurtosis){
  beta <- (3 * (n^2 + 27*n - 70) * (n+1) * (n+3)) / ((n-2)*(n+5)*(n+7)*(n+9))
  omega2 <- -1 + sqrt(2*(beta - 1))
  y <- skewness * sqrt(((omega2 - 1) * (n+1) * (n+3))/(12*(n-2)))
  delta <- 1/sqrt(log(sqrt(omega2)))
  z1 <- delta * log(y+sqrt(y^2+1))
  
  delta <- (n-3)*(n+1)*(n^2+15*n-4)
  a <- (n-2)*(n+5)*(n+7)*(n^2+27*n-70)/(6*delta)
  c <- (n-7)*(n+5)*(n+7)*(n^2+2*n-5)/(6*delta)
  k <- (n+5)*(n+7)*(n^3+37*n^2+11*n-313)/(12*delta)
  alpha <- a+ c * skewness^2
  chi <- 2 * k *(kurtosis - 1 - skewness^2)
  z2 <- sqrt(9*alpha) *(1/(9*alpha) + (chi/(2*alpha))^(1/3)-1)
  
  DH <- z1^2+z2^2
  p_val <- 1 - pchisq(DH, 2)
  result <- c(DH, p_val)
  names(result) <- c("Estimate", "Pr(>|t|)")
  result
}

