####################
# Likelihood Ratio #
####################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Likelihood Ratio
#' @description Likelihood Ratio using confusion matrix.
#' An important measure related to the sensitivity and specificity of the classifier.
#' A higher positive likelihood and a lower negative likelihood mean better performance on positive and negative classes, respectively (Japkowitcz, 2011).
#' For more information Deeks and Altman (2004).
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return matrix of Likelihood Ratio values.
#'
#' @export
#'
#'

likelihood_ratio <- function(confusion_matrix) {
  # Calculate for positive part
  lr_positive <- sensitivity(confusion_matrix) /
    (1 - specificity(confusion_matrix))
  # Calculate for negative part
  lr_negative <- (1 - sensitivity(confusion_matrix)) /
    specificity(confusion_matrix)

  # Bind
  LR <- rbind(lr_positive, lr_negative)
  rownames(LR) <- c("+", "-")

  # Output
  return(LR)
}
