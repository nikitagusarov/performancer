#############
# Recall #
#############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Recall
#' @description Compute Recall using confusion matrix.
#' "Recall" is another term to determine "True Positive Ratio" (TPR).
#' In this package the `recall()` function is defined as a wrapper for `true_positive_ratio()`.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Recall
#'
#' @export
#'
#'

recall <- function(confusion_matrix) {
  # Compute Recall
  Recall <- true_positive_rate(confusion_matrix)

  # Output
  return(Recall)
}
