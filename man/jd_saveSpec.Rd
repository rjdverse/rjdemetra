\name{jd_saveSpec}
\alias{jd_saveSpec}
\title{Saving a RegARIMA model specification, pre-adjustment in X13 and TRAMO/SEATS}
\usage{
jd_saveSpec(object, file = "JD_Spec.RData")
}
\description{
\code{jd_saveSpec} saves a RegARIMA model specification.
}
\arguments{
\item{object}{object of one of the classes: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} and \code{c("JD_RegArima","TRAMO_SEATS")}.}
\item{file}{(path and) name of the file where the model specification will be saved.}
}
\details{
\code{jd_saveSpec} saves the model specification of a \code{"JD_RegArima_Spec"} or \code{"JD_RegArima"} class object. In case of the \code{"JD_RegArima_Spec"} class object only the final values are saved (for details see the values of the function \code{\link{jd_regarima_specX13}} and \code{\link{jd_regarima_specTS}}).
}
\references{
Info on JDemtra+, usage and functions:
\url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
}
\examples{
  myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
  myspec2 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")
  myreg2 <-jd_regarima(myseries,myspec2)

  myreg3 <-jd_regarima_defTS(myseries, spec=c("TRfull"))
  myspec4<-jd_regarima_specTS(myreg3,tradingdays.mauto ="Unused",
                              tradingdays.option = "WorkingDays",
                              easter.type = "Standard", automdl.enabled = FALSE, arima.mu = TRUE)
  myreg4 <-jd_regarima(myseries,myspec4)

  # Save the model specification from a c("JD_RegArima_Spec","X13") class object
  jd_saveSpec(myspec2,"specx13.RData")
  # Save the model specification from a c("JD_RegArima","X13") class object
  jd_saveSpec(myreg2,"regx13.RData")
  # Save the model specification from a c("JD_RegArima_Spec","TRAMO_SEATS") class object
  jd_saveSpec(myspec4,"specTS.RData")
  # Save the model specification from a c("JD_RegArima","TRAMO_SEATS") class object
  jd_saveSpec(myreg4,"regTS.RData")
}
