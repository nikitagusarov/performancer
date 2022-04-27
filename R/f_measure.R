#############
# F-measure #
#############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title F-measure
#' @description Compute F-measure using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#' @param alpha Alpha value for weighted harmonic mean calculation.
#' Defaults to 1, which produces a balanced F-measure.
#'
#' @return integer value of F-measure
#'
#' @export
#'
#'

f_measure <- function(confusion_matrix,
                      alpha = 1) {
  # Compute values to be reused
  Precision <- precision(confusion_matrix)
  Recall <- recall(confusion_matrix)

  # Compute F-measure
  F <- (
    (1 + alpha) * (Precision * Recall)
  ) / (
    (alpha * Precision) + Recall
  )

  # Output
  return(F)
}
