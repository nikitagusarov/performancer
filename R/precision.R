#############
# Precision #
#############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Precision
#' @description Compute Precision using confusion matrix.
#' "Precision" is another term to determine "Positive Predicted Values" (PPV).
#' In this package the `precision()` function is defined as a wrapper for `positive_predicted_values()`.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Precision
#'
#'

precision <- function(confusion_matrix) {
  # Compute Precision
  PPV <- positive_predicted_values(confusion_matrix)

  # Output
  return(PPV)
}
