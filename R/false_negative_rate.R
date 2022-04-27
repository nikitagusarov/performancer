#######################
# False Negative Rate #
#######################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title False Negative Rate
#' @description Calculate False Negative Rate using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of False Negatives Rate values.
#'
#' @export
#'
#'

false_negative_rate <- function(confusion_matrix) {
  FN <- false_negative(confusion_matrix)
  TP <- true_positive(confusion_matrix)

  # False-negative rate
  FNR <- FN / (FN + TP)

  # Output
  return(FNR)
}
