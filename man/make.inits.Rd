% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inits.R
\name{make.inits}
\alias{make.inits}
\title{Make initial values for multiple chains}
\usage{
make.inits(
  J,
  n.z,
  n.chains,
  monitor,
  coef.init.limit = 2,
  tau2.init.min = 1e-04,
  tau2.init.max = 2
)
}
\arguments{
\item{J}{Number of clusters}

\item{n.z}{Number of covariates (i.e. excluding the exposure)}

\item{n.chains}{Number of MCMC chains}

\item{monitor}{Names of parameters for which to generate initial values}

\item{coef.init.limit}{For coefficients, initial values are generated
between -coef.init.limit and coef.init.limit.}

\item{tau2.init.min}{Minimum for initial values of tau2 heterogeneity
parameters.}

\item{tau2.init.max}{Maximum for initial values of tau2 heterogeneity
parameters.}
}
\value{
List of vectors with named initial values.
}
\description{
Initial values for multiple JAGS chains by the \link{misclass} function.
}
\examples{
make.inits(
  J = 5,
  n.z = 2,
  n.chains = 3,
  monitor = c("eta.0.0", "phi.y.0", "tau2.beta.x.j")
)

}
