\name{s_preVar}
\alias{s_preVar}
\title{Access model specification, SA and pre-adjustment in X13 and TRAMO/SEATS}
\usage{
s_preVar(object)
}
\description{
\code{s_preVar} access the \emph{user-defined variables} of the model.
}
\arguments{
\item{object}{object of class: \code{c("JD_RegArima_Spec","X13")}, \code{c("JD_RegArima","X13")}, \code{c("JD_RegArima_Spec","TRAMO_SEATS")} or \code{c("JD_RegArima","TRAMO_SEATS")}.}
}
\value{
A list with the information on the user-defined variables, including: \code{series} - the time series and \code{description} - data.frame with the variable type and coefficients. For details see: \emph{Value} of the \code{c("JD_RegArima_Spec","X13")} and \code{c("JD_RegArima_Spec","TRAMO_SEATS")} class objects.
}
\references{
Info on JDemtra+, usage and functions:
\url{https://ec.europa.eu/eurostat/cros/content/documentation_en}
}
\examples{
  var1 <- ts(rnorm(length(myseries))*10,start = c(2001, 12), frequency = 12)
  var2 <- ts(rnorm(length(myseries))*100,start = c(2001, 12), frequency = 12)
  var3 <-ts(matrix(c(var1,var2), ncol=2),start = c(2001, 12), frequency = 12)
  myspec1 <- jd_regarima_specDefX13(spec="RG5c", usrdef.varEnabled = TRUE, usrdef.var = var3)
  myreg1 <- jd_regarima(myseries,myspec1)

  s_preVar(myspec1)
  s_preVar(myreg1)

}
