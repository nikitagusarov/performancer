##################
# False Negative #
##################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title False Negative
#' @description Count False Negatives using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of False Negatives count
#'
#' @export
#'
#'

false_negative <- function(confusion_matrix) {
  # Compute False Negative
  FN <- rowSums(confusion_matrix) - diag(confusion_matrix)

  # Output
  return(FN)
}
