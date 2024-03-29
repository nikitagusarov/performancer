% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr_curve.R
\name{plot.pr_curve}
\alias{plot.pr_curve}
\title{Plot PR coordinates}
\usage{
\method{plot}{pr_curve}(x, ...)
}
\arguments{
\item{x}{A PR coordinates object.
A simple data.frame containing respective `x` and `y` coordinates.
The coordinates should be PR space compliant.}

\item{...}{Other parameters passed to base plot function.}
}
\value{
plot of a PR curve for given `pr_curve`
}
\description{
Plot PR points using provided coordinates.
}
