% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bard.R
\name{bard}
\alias{bard}
\title{Detection of multivariate anomalous segments using BARD.}
\usage{
bard(
  x,
  p_N = 1/(nrow(x) + 1),
  p_A = 5/nrow(x),
  k_N = 1,
  k_A = (5 * p_A)/(1 - p_A),
  pi_N = 0.9,
  paffected = 0.05,
  lower = 2 * sqrt(log(nrow(x))/nrow(x)),
  upper = max(x),
  alpha = 1e-04,
  h = 0.25
)
}
\arguments{
\item{x}{A numeric matrix with n rows and p columns containing the data which is to be inspected. The time series data classes ts, xts, and zoo are also supported.}

\item{p_N}{Hyper-parameter of the negative binomial distribution for the length of non-anomalous segments (probability of success). Defaults to \eqn{\frac{1}{n+1}.}}

\item{p_A}{Hyper-parameter of the negative binomial distribution for the length of anomalous segments (probability of success). Defaults to \eqn{\frac{5}{n}.}}

\item{k_N}{Hyper-parameter of the negative binomial distribution for the length of non-anomalous segments (size). Defaults to 1.}

\item{k_A}{Hyper-parameter of the negative binomial distribution for the length of anomalous segments (size). Defaults to \eqn{\frac{5p_A}{1- p_A}.}}

\item{pi_N}{Probability that an anomalous segment is followed by a non-anomalous segment. Defaults to 0.9.}

\item{paffected}{Proportion of the variates believed to be affected by any given anomalous segment. Defaults to 5\%. 
This parameter is relatively robust to being mis-specified and is studied empirically in Section 5.1 of \insertCite{bardwell2017;textual}{anomaly}.}

\item{lower}{The lower limit of the the prior uniform distribution for the mean of an anomalous segment \eqn{\mu}. Defaults to \eqn{2\sqrt{\frac{\log(n)}{n}}.}}

\item{upper}{The upper limit of the prior uniform distribution for the mean of an anomalous segment \eqn{\mu}. 
Defaults to the largest value of x.}

\item{alpha}{Threshold used to control the resampling in the approximation of the posterior distribution at each time step. A sensible default is 1e-4.
Decreasing alpha increases the accuracy of the posterior distribution but also increases the computational complexity of the algorithm.}

\item{h}{The step size in the numerical integration used to find the marginal likelihood. 
The quadrature points are located from \code{lower} to \code{upper} in steps of \code{h}. Defaults to 0.25. 
Decreasing this parameter increases the accuracy of the calculation for the marginal likelihood but increases computational complexity.}
}
\value{
An instance of the S4 object of type \code{.bard.class} containing the data \code{x}, procedure parameter values, and the results.
}
\description{
Implements the BARD (Bayesian Abnormal Region Detector) procedure of Bardwell and Fearnhead (2017). 
BARD is a fully Bayesian inference procedure which is able to give measures of uncertainty about the 
number and location of anomalous regions. It uses negative binomial prior distributions on the lengths 
of anomalous and non-anomalous regions as well as a uniform prior for the means of anomalous regions. 
Inference is conducted by solving a set of recursions. To reduce computational and storage costs a resampling 
step is included.
}
\section{Notes on default hyper-parameters}{

This function gives certain default hyper-parameters for the two segment length distributions.
We chose these to be quite flexible for a range of problems. For non-anomalous segments a geometric distribution
was selected having an average segment length of \eqn{n} with the standard deviation being of the same order. 
For anomalous segments we chose parameters that gave an average length of 5 and a variance of \eqn{n}. 
These may not be suitable for all problems and the user is encouraged to tune these parameters.
}

\examples{

library(anomaly)
data(simulated)
# run bard
bard.res<-bard(sim.data, alpha = 1e-3, h = 0.5)
sampler.res<-sampler(bard.res)
collective_anomalies(sampler.res)
\donttest{
plot(sampler.res,marginals=TRUE)
}
}
\references{
\insertRef{bardwell2017}{anomaly}

\insertRef{JSS-anomaly-paper-final}{anomaly}
}
\seealso{
\code{\link{sampler}}
}
