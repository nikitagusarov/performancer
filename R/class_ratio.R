###############
# Class Ratio #
###############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Class Ratio
#' @description Compute Class Ratio using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#' @param alpha Alpha value for weighted harmonic mean calculation.
#'
#' @return integer value of Class Ratio
#'
#'

class_ratio <- function(confusion_matrix) {
  # Compute Class Ratio
  CR <- (true_positive() + false_negative()) /
    (false_positive() + true_negative())

  # Output
  return(CR)
}
