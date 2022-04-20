######################
# True Negative Rate #
######################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title True Negative Rate
#' @description Calculate True Negative Rate using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of True Negatives Rate values.
#'
#'

true_negative_rate <- function(confusion_matrix) {
  TN <- true_negative(confusion_matrix)
  FP <- false_positive(confusion_matrix)

  # True-negative rate (or Specificity)
  TNR <- TN / (TN + FP)

  # Output
  return(TNR)
}
