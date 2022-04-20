#################
# True Positive #
#################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title True Positive
#' @description Count True Positives using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of True Positives count
#'
#'

true_positive <- function(confusion_matrix) {
  # Compute True Positive
  TP <- diag(confusion_matrix)

  # Output
  return(TP)
}
