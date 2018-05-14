\name{jd_regarima_defX13}
\alias{jd_regarima_defX13}
\title{Pre-specified RegARIMA model, pre-adjustment in X13}
\usage{
jd_regarima_defX13(series, spec = c("RG5c", "RG0", "RG1", "RG2c", "RG3", "RG4c"))
}
\description{
\code{jd_regarima_defX13} decomposes the time series in a linear deterministic component and in a stochastic component. The deterministic part of the series can contain outliers, calendar effects and regression effects. The stochastic part is defined by a seasonal multiplicative ARIMA model, as discussed by BOX, G.E.P., and JENKINS, G.M. (1970).
}
\arguments{
\item{series}{a univariate time series}
\item{spec}{predefined \emph{JDemetra+} model specification (see \emph{Details}). The default is "RG5c".}

}
\details{
In the X13 seasonal adjustment method the first step consists of pre-adjusting the original series with a RegARIMA model, where the original series is corrected for any deterministic effects and missing observations. This step is also referred as linearization of the original series.

The RegARIMA model (model with ARIMA errors) is specified as below.

\deqn{z_t=y_t\beta+x_t}

where:
\itemize{
\item \eqn{z_t} - is the original series;
\item \eqn{\beta = (\beta_1,...,\beta_n)} - a vector of regression coefficients;
\item \eqn{y_t = (y_{1t},...,y_{nt})} - \eqn{n} regression variables (outliers, calendar effects, user-defined variables);
\item \eqn{x_t} - a disturbance that follows the general ARIMA process:
\eqn{\phi(B)\delta(B)x_t=\theta(B)a_t}; \eqn{\phi(B), \delta(B)} and \eqn{\theta(B)} are the finite polynomials in \eqn{B}; \eqn{a_t} is a white-noise variable with zero mean and a constant variance.
}

The polynomial \eqn{\phi(B)} is a stationary autoregressive (AR) polynomial in \eqn{B}, which is a product of the stationary regular AR polynomial in \eqn{B} and the stationary seasonal polynomial in \eqn{B^s}:

\deqn{\phi(B)=\phi_p(B)\Phi_{bp}(B^s)=(1+\phi_1B+...+\phi_pB^p)(1+\Phi_1B^s+...+\Phi_{bp}B^{bps})}

where:
\itemize{
\item \eqn{p} - number of regular AR terms (here and in \emph{JDemetra+} \eqn{p \le 3});
\item \eqn{bp} - number of seasonal AR terms (here and in \emph{JDemetra+} \eqn{bp \le 1});
\item \eqn{s} - number of observations per year (frequency of the time series).
}

The polynomial \eqn{\theta(B)} is an invertible moving average (MA) polynomial in \eqn{B}, which is a product of the invertible regular MA polynomial in \eqn{B} and the invertible seasonal MA polynomial in \eqn{B^s}:

\deqn{\theta(B)=\theta_q(B)\Theta_{bq}(B^s)=(1+\theta_1B+...+\theta_qB^q)(1+\Theta_1B^s+...+\Theta_{bq}B^{bqs})}

where:
\itemize{
\item \eqn{q} - number of regular MA terms (here and in \emph{JDemetra+} \eqn{q \le 3});
\item \eqn{bq} - number of seasonal MA terms (here and in \emph{JDemetra+} \eqn{bq \le 1});
}

The polynomial \eqn{\delta(B)} is the non-stationary AR polynomial in \eqn{B} (unit roots):

\deqn{\delta(B)=(1-B)^d(1-B^s)^{d_s}}

where:
\itemize{
\item \eqn{d} - regular differencing order (here and in \emph{JDemetra+} \eqn{d \le 1});
\item \eqn{d_s} - seasonal differencing order (here and in \emph{JDemetra+} \eqn{d_s \le 1});
}

Notations used for AR and MA processes, model denoted as ARIMA \eqn{(P,D,Q)(BP,BD,BQ)}, are consistent with those in \emph{JDemetra+}.



As regards the available predefined \emph{JDemetra+} model specifications, they are described in the table below.
\tabular{rrrrrrr}{
\strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calender effects} |\tab \strong{ARIMA}\cr
RG0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
RG1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
RG2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
RG3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
RG4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
RG5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
}
}
\value{
\code{jd_regarima_defX13} returns an object of class \code{"JD_RegArima"} and sub-class \code{"X13"} (i.e. \code{c("JD_RegArima","X13")}).

An object of class \code{"JD_RegArima"} is a list containing the following components:

\item{specification}{list with the model specification as provided in the specification (argument spec). See also Value of the \code{\link{jd_regarima_specX13}} function.}

\item{arma}{vector with the orders of the autoregressive (AR), moving average (MA), seasonal AR and seasonal MA processes, as well as with the regular and seasonal differencing orders (P,D,Q) (BP,BD,BQ).}

\item{arima.coefficients}{ matrix with the regular and seasonal AR and MA coefficients. The matrix contains the estimated coefficients, standard errors and t-statistics values. The estimated coefficients can be also extracted with the function \code{\link[stats]{coef}} (the output includes also the regression coefficients).}

\item{regression.coefficients}{ matrix with the regression variables (i.e.: mean, calendar effect, outliers and user-defined regressors) coefficients. The matrix contains the estimated coefficients, standard errors and t-statistics values. The estimated coefficients can be also extracted with the function \code{\link[stats]{coef}} (output includes also the arima coefficients). }

\item{loglik}{ list containing the log-likelihood of the RegARIMA model as well as the associated model selection criteria statistics (AIC, AICC, BIC and BICC) and parameters (\code{n.parameters} = number of parameters in the likelihood, \code{eff.obs} = number of effective observations in the likelihood). These statistics can be also extracted with the function \code{\link[stats]{logLik}}.}

\item{model}{list containing the information on the model specification after its estimation (\code{specification_rst}), as well as the decomposed elements of the input series. The model specification includes the information on the estimation time span (\code{T.span}), whether the original series was log transformed (\code{Log transformation}) and details on the regression part of the RegARIMA model; i.e. if it includes a \code{mean}, \code{trading days} effects (if yes, it provides the number of regressors), \code{leap year} effect, \code{Easter} effect and whether outliers were detected (if yes, it provides the number of outliers). The decomposed elements of the input series contain the linearised series (\code{y_lin}) and the deterministic components; i.e.: trading days effect (\code{td.effect}), Easter effect (\code{easter.effect}), other moving holidays effect (\code{omh.effect}) and outliers effect (\code{out.effect}).}

\item{residuals}{ the residuals (time series). They can be also extracted with the function \code{\link[stats]{residuals}}.}

\item{residuals.stat}{ List containing statistics on the RegARIMA residuals. It provides residuals standard error (st.error) and results for the tests on the normality, independence and linearity of the residuals. See also function \code{\link{jd_regarima_rtest}}.}

\item{forecast}{List containing two time series, the forecast of the original series (\code{forecast}) and it's standard error (\code{st.error}).}

}
\references{
Info on JDemtra+, usage and functions:
\url{https://ec.europa.eu/eurostat/cros/content/documentation_en}

BOX G.E.P. and JENKINS G.M. (1970), "Time Series Analysis: Forecasting and Control", Holden-Day, San Francisco.

BOX G.E.P., JENKINS G.M., REINSEL G.C. and LJUNG G.M. (2015), "Time Series Analysis: Forecasting and Control", John Wiley & Sons, Hoboken, N. J., 5th edition.

}
\examples{

  myreg1 <- jd_regarima_defX13(myseries, spec=c("RG5c"))
  myreg2 <- jd_regarima_defX13(myseries, spec=c("RG4c"))
  myreg1
  summary(myreg2)
  plot(myreg2)

}
