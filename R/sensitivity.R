###############
# Sensitivity #
###############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Sensitivity
#' @description Calculate Sensitivity using confusion matrix.
#' "Sensitivity" is just another term to designate "True Positive Ratio".
#' The term has its origin in the medical domain, in which the metric is typically used to study the effectiveness of a clinical test in detecting a disease (Japkowitcz, 2011).
#' In the multiclass scenario, sensitivity measurements essentially study the accuracy of the classifier over individual classes.
#' In this package the function is just a wrapper for `true_negative_ratio()`.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of Sensitivity values.
#'
#'

sensitivity <- function(confusion_matrix) {
  # True-positive rate (or Sensitivity)
  sensitivity <- true_positive_rate(confusion_matrix)

  # Output
  return(sensitivity)
}
