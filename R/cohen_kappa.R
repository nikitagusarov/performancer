#################
# Cohen's Kappa #
#################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Cohen's Kappa
#' @description Calculate Cohen's Kappa using confusion matrix.
#' Contrary to Scott's Pi, here the chance agreement is defined considering the two processes to be fixed.
#' For more information see Cohen (1960).
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer Cohen's Kappa value.
#'
#' @export
#'
#'

cohen_kappa <- function(confusion_matrix) {
  # Get total observations
  N <- sum(confusion_matrix)

  # Get marginal values
  row_n <- rowSums(confusion_matrix)
  col_n <- colSums(confusion_matrix)

  # Compute the probability of overall agreement over the label
  # assignments between the classifier and the true process
  Po <- sum(diag(confusion_matrix)) / N
  # Compute the probability of chance agreement
  Pe <- sum((row_n * col_n) / N^2)

  # Compute Cohen's kappa (k)
  k <- (Po - Pe) / (1 - Pe)

  # Output
  return(k)
}
