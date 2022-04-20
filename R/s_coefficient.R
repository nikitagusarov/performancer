#################
# S Coefficient #
#################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title S Coefficient
#' @description Calculate S Coefficient using confusion matrix.
#' For more information see Bennett et al. (1954).
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer S Coefficient value.
#'
#'

s_coefficient <- function(confusion_matrix) {
  # Get total observations
  N <- sum(confusion_matrix)

  # Compute the probability of overall agreement over the label
  # assignments between the classifier and the true process
  Po <- sum(diag(confusion_matrix)) / N

  # Compute S Coefficient (k)
  s <- (2 * Po) - 1

  # Output
  return(s)
}
