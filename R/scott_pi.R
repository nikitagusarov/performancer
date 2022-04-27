##############
# Scott's Pi #
##############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Scott's Pi
#' @description Calculate Scott's Pi using confusion matrix.
#' It estimates the chance that a label (positive or negative) is assigned given a random instance irrespective of the label-assigning process.
#' For more information see Scott (1955)
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer Scott's Pi value.
#'
#' @export
#'
#'

scott_pi <- function(confusion_matrix) {
  # Get total observations
  N <- sum(confusion_matrix)

  # Get marginal values
  row_n <- rowSums(confusion_matrix)
  col_n <- colSums(confusion_matrix)

  # Compute the probability of overall agreement over the label
  # assignments between the classifier and the true process
  Po <- sum(diag(confusion_matrix)) / N
  # Compute the probability of chance agreement
  Pe <- sum(((row_n + col_n) / (2 * N))^2)

  # Compute Scott's Pi (k)
  sp <- (Po - Pe) / (1 - Pe)

  # Output
  return(sp)
}
