\name{s_arimaCoef}
\alias{s_arimaCoef}
\title{Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS}
\usage{
s_arimaCoef(object)
}
\description{
\code{s_arimaCoef} access the \emph{arima} part of the model specification with the user-specified ARIMA coefficients.
}
\arguments{
\item{object}{object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.}
}
\value{
A data.frame with the user-specified ARIMA coefficients. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
}
\references{
Info on JDemtra+, usage and functions:
\url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
}
\examples{
  myreg <- jd_regarima_defX13(myseries, spec=c("RG5c"))
  myspec1 <-jd_regarima_specX13(myreg, automdl.enabled =FALSE,
           arima.coefEnabled = TRUE, arima.p=1,arima.q=1, arima.bp=1, arima.bq=1,
           arima.coef = rep(0.2,4), arima.coefType = rep("Initial",4))
  myreg1 <- jd_regarima(myseries,myspec1)

  s_arimaCoef(myreg1)
  s_arimaCoef(myspec1)

}
