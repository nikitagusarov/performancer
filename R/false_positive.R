##################
# False Positive #
##################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title False Positive
#' @description Count False Positives using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of False Positives count
#'
#' @export
#'
#'

false_positive <- function(confusion_matrix) {
  # Compute False Positive
  FP <- colSums(confusion_matrix) - diag(confusion_matrix)

  # Output
  return(FP)
}
