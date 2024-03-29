% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msle.R
\name{msle}
\alias{msle}
\title{Mean Squared Logarithmic Error (MSLE)}
\usage{
msle(y_real, y_predicted, rowsums = TRUE)
}
\arguments{
\item{y_real}{Observed values (integers) to compare with
(in matrix format for multiclass classification).}

\item{y_predicted}{Predicte values (probabiblities by class).}

\item{rowsums}{Whether in case of multiple classes in dataframe the metrics should be summed across classes before calculating the mean.}
}
\value{
integer value of Mean Squared Logarithmic Error (MSLE)
}
\description{
Compute Mean Squared Logarithmic Error (MSLE).
#'
}
