#############################
# Negative Predicted Values #
#############################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Negative Predicted Values
#' @description Compute Negative Predicted Values using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Negative Predicted Values
#'
#' @export
#'
#'

negative_predicted_values <- function(confusion_matrix) {
  # Compute Negative Predicted Values
  NPV <- true_negative(confusion_matrix) /
    (true_negative(confusion_matrix) + false_negative(confusion_matrix))

  # Output
  return(NPV)
}
