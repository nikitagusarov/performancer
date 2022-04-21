#############
# Precision #
#############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Precision
#' @description Compute Precision using confusion matrix.
#' "Precision" is another term to determine "Positive Predicted Values" (PPV).
#' In this package the `precision()` function is defined as a wrapper for `positive_predicted_values()`.
#' It's possible to calculate the skew-sensitive formulation of the precision by adjusting `r` parameter.
#'
#' @param confusion_matrix Confusion matrix to be used.
#' @param r Skew Ratio parameter, which is required to calculate skew-sensitive form of Precision,
#' Deafult to NULL, which calculates a non-weighted Precision.
#'
#' @return integer value of Precision
#'
#'

precision <- function(confusion_matrix,
                      r = NULL) {
  if (is.null(r)) {
    # Compute Precision
    Prec <- positive_predicted_values(confusion_matrix)
  } else if (is.numeric(r)) {
    # Compute skew-sensitive accuracy
    Prec <- true_positive(confusion_matrix) /
      (true_positive(confusion_matrix) + (r * false_positive(confusion_matrix)))
  } else {
    stop("An incorrect Skew Ratio value provided.")
  }

  # Output
  return(Prec)
}
