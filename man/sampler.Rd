% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bard.R
\name{sampler}
\alias{sampler}
\title{Post processing of BARD results.}
\usage{
sampler(bard_result, gamma = 1/3, num_draws = 1000)
}
\arguments{
\item{bard_result}{An instance of the S4 class \code{.bard.class} containing a result returned by the \code{bard} function.}

\item{gamma}{Parameter of loss function giving the cost of a false negative i.e. incorrectly allocating an anomalous point as being non-anomalous. 
For more details see Section 3.5 of \insertCite{bardwell2017;textual}{anomaly}.}

\item{num_draws}{Number of samples to draw from the posterior distribution.}
}
\value{
Returns an S4 class of type \code{bard.sampler.class}.
}
\description{
Draw samples from the posterior distribution to give the locations of anomalous segments.
}
\examples{
library(anomaly)
data(simulated)
# run bard
res<-bard(sim.data, alpha = 1e-3, h = 0.5)
# sample 
sampler(res)

}
\references{
\insertRef{bardwell2017}{anomaly}

\insertRef{JSS-anomaly-paper-final}{anomaly}
}
\seealso{
\code{\link{bard}}
}
