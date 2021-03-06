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
  upper = max(transform(x)),
  alpha = 1e-04,
  h = 0.25,
  transform = robustscale
)
}
\arguments{
\item{x}{An n x p real matrix representing n observations of p variates.}

\item{p_N}{Hyper-parameter of the negative binomial distribution for the length of non-anomalous segments (probability of success). Defaults to \deqn{\frac{1}{n+1}.}}

\item{p_A}{Hyper-parameter of the negative binomial distribution for the length of anomalous segments (probability of success). Defaults to \deqn{\frac{5}{n}.}}

\item{k_N}{Hyper-parameter of the negative binomial distribution for the length of non-anomalous segments (size). Defaults to 1.}

\item{k_A}{Hyper-parameter of the negative binomial distribution for the length of anomalous segments (size). Defaults to \deqn{\frac{5p_A}{1- p_A}.}}

\item{pi_N}{Probability that an anomalous segment is followed by a non-anomalous segment. Defaults to 0.9.}

\item{paffected}{Proportion of the variates believed to be affected by any given anomalous segment. Defaults to 5\%. 
This parameter is relatively robust to being mis-specified and is studied empirically in Section 5.1 of \insertCite{bardwell2017;textual}{anomaly}.}

\item{lower}{The lower limit of the the prior uniform distribution for the mean of an anomalous segment \eqn{\mu}. Defaults to \deqn{2\sqrt{\frac{\log(n)}{n}}.}}

\item{upper}{The upper limit of the prior uniform distribution for the mean of an anomalous segment \eqn{\mu}. 
Defaults to the largest standardised value of x, i.e. \code{max(transform(x))}.}

\item{alpha}{Threshold used to control the resampling in the approximation of the posterior distribution at each time step. A sensible default is 1e-4.
Decreasing alpha increases the accuracy of the posterior distribution but also increases the computational complexity of the algorithm.}

\item{h}{The step size in the numerical integration used to find the marginal likelihood. 
The quadrature points are located from \code{lower} to \code{upper} in steps of \code{h}. Defaults to 0.25. 
Decreasing this parameter increases the accuracy of the calculation for the marginal likelihood but increases computational complexity.}

\item{transform}{A function used to transform the data prior to analysis. The default value is to scale the data using the median and the median absolute deviation.}
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
set.seed(0)
sim.data<-simulate(n=500,p=50,mu=2,locations=c(100,200,300),
                   duration=6,proportions=c(0.04,0.06,0.08))
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
}
\seealso{
\code{\link{sampler}}
}
