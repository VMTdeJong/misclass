% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misclass.R
\name{print.misclass}
\alias{print.misclass}
\title{Print a misclass object}
\usage{
\method{print}{misclass}(x, digits = 3, vars = NA, ...)
}
\arguments{
\item{x}{A misclass object}

\item{digits}{Number of digits used for rounding}

\item{vars}{Names of variables to print. Ignored for models with few variables.
Defaults to all variables (which may be undesirable for models with many
monitored variables).}

\item{...}{Arguments passed to \code{add.summary} from the \code{runjags}
package, for models with many monitors.}
}
\description{
Print a misclass object
}
