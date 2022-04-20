############
# Accuracy #
############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Accuracy
#' @description Compute Accuracy using confusion matrix.
#' Accuracy the describes the fraction of correctly classified instances in the test set.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Accuracy
#'
#'

accuracy <- function(confusion_matrix) {
  # Compute Accuracy
  AC <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

  # Output
  return(AC)
}
