% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion_matrix.R
\name{confusion_matrix}
\alias{confusion_matrix}
\title{Confusion matrix construction}
\usage{
confusion_matrix(y_real, y_predicted)
}
\arguments{
\item{y_real}{Observed values to compare with in matrix format.}

\item{y_predicted}{Predicte values (probabiblities by class).}
}
\value{
matrix Confusion matrix.
}
\description{
Construct confusion matrix based on matrix form input.
The confusion matrix is tipically defined with respect to some learning algorithm.
The diagonal entries denote the correctly classified examples for respective class.
All the nondiagonal entries denote misclassifications.
}