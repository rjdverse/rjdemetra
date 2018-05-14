\name{s_transform}
\alias{s_transform}
\title{Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS}
\usage{
s_transform(object)
}
\description{
\code{s_transform} access the \code{transform} part of the model specification.
}
\arguments{
\item{object}{object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.}
}
\value{
A data.frame with the \emph{transform} variables. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
}
\references{
Info on JDemtra+, usage and functions:
\url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
}
\examples{
  myreg1 <-jd_regarima_defX13(myseries, spec=c("RG5c"))
  myspec1 <-jd_regarima_specX13(myreg1, estimate.from = "2005-10-01", outlier.from = "2010-03-01")

  s_transform(myreg1)
  s_transform(myspec1)

}
