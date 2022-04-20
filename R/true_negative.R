#################
# True Negative #
#################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title True Negative
#' @description Count True Negatives using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return vector of True Negatives count
#'
#'

true_negative <- function(confusion_matrix) {
  # Compute True Negative
  TN <- sapply(
    seq(nrow(confusion_matrix)),
    function(i) {
      sum(confusion_matrix[-i, -i])
    }
  )

  # Conserve naming convention
  names(TN) <- colnames(confusion_matrix)

  # Output
  return(TN)
}
