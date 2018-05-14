\name{jd_loadSpec}
\alias{jd_loadSpec}
\title{Loading a RegARIMA model specification, pre-adjustment in X13 and TRAMO/SEATS}
\usage{
jd_loadSpec(file = "JD_Spec.RData")
}
\description{
\code{jd_loadSpec} loads a previously saved RegARIMA model specification.
}
\arguments{
\item{file}{(path and) name of the file where the model specification was saved.}
}
\details{
\code{jd_loadSpec} loads a RegARIMA model specification that was previously saved using the function \code{\link{jd_saveSpec}}. It creates a \code{c("JD_RegArima_Spec","X13")} or \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class object, depending on the sub-class of the previously saved model specification.
}
\value{
A list of class \code{"JD_RegArima_Spec"}. For details see Value of the function \code{\link{jd_regarima_specX13}} and \code{\link{jd_regarima_specTS}}.
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
  myspec4 <-jd_regarima_specTS(myreg3,tradingdays.mauto ="Unused",tradingdays.option ="WorkingDays",
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

  # Load the model specification
  # c("JD_RegArima_Spec","X13") class object
  myspec2a <- jd_loadSpec("specx13.RData")
  myspec2b <- jd_loadSpec("regx13.RData")
  # c("JD_RegArima_Spec","TRAMO_SEATS") class object
  myspec4a <- jd_loadSpec("specTS.RData")
  myspec4b <- jd_loadSpec("regTS.RData")

  jd_regarima(myseries,myspec2a)
  jd_regarima(myseries,myspec4a)
}
