#######################
# False Positive Rate #
#######################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title False Positive Rate
#' @description Calculate False Positive Rate using confusion matrix.
#' Proportion of instances assigned to class i of interest that actually do not belong to this class.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of False Positives Rate values.
#'
#' @export
#'
#'

false_positive_rate <- function(confusion_matrix) {
  FP <- false_positive(confusion_matrix)
  TN <- true_negative(confusion_matrix)

  # False-positive rate
  FPR <- FP / (FP + TN)

  # Output
  return(FPR)
}
