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
#'

false_negative <- function(confusion_matrix) {
  # Compute False Negative
  FN <- rowSums(conf_m) - diag(conf_m)

  # Output
  return(FN)
}
