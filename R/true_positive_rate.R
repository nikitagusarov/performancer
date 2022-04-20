######################
# True Positive Rate #
######################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title True Positive Rate
#' @description Calculate True Positive Rate using confusion matrix.
#' The proportion of the examples of some class i of interest actually assigned to class i by the learning algorithm.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of True Positives Rate values.
#'
#'

true_positive_rate <- function(confusion_matrix) {
  TP <- true_positive(confusion_matrix)
  FN <- false_negative(confusion_matrix)

  # True-positive rate (or Sensitivity)
  TPR <- TP / (TP + FN)

  # Output
  return(TPR)
}
