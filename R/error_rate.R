##############
# Error Rate #
##############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Epirical risk
#' @description Compute Error Rate using confusion matrix.
#' Error rate measures the fraction of the instances from the test set that are misclassified by the learning algorithm.
#' This measurement further includes the instances from all classes.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Error Rate
#'
#'

error_rate <- function(confusion_matrix) {
  # Compute Error Rate
  ER <- (sum(confusion_matrix) - sum(diag(confusion_matrix))) /
    sum(confusion_matrix)

  # Output
  return(ER)
}
