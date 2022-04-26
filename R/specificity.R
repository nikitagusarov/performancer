###############
# Specificity #
###############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Specificity
#' @description Calculate Specificity using confusion matrix.
#' "Specificity" is just another term to designate "True Negative Ratio".
#' This is the complement metric to "Sensitivity" metric.
#' In this package the function is just a wrapper for `true_negative_ratio()`.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of Specificity values.
#'
#'

specificity <- function(confusion_matrix) {
  # True-positive rate (or Specificity)
  specificity <- true_negative_rate(confusion_matrix)

  # Output
  return(specificity)
}
