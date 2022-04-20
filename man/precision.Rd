% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precision.R
\name{precision}
\alias{precision}
\title{Precision}
\usage{
precision(confusion_matrix)
}
\arguments{
\item{confusion_matrix}{Confusion matrix to be used.}
}
\value{
integer value of Precision
}
\description{
Compute Precision using confusion matrix.
"Precision" is another term to determine "Positive Predicted Values" (PPV).
In this package the `precision()` function is defined as a wrapper for `positive_predicted_values()`.
}