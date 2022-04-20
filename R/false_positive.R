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
#'

false_positive <- function(confusion_matrix) {
  # Compute False Positive
  FP <- colSums(conf_m) - diag(conf_m)

  # Output
  return(FP)
}
