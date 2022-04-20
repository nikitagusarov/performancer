#############################
# Positive Predicted Values #
#############################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Positive Predicted Values
#' @description Compute Positive Predicted Values using confusion matrix.
#' Indicates what is the proportion of examples that truly belong to class i, from among all the examples assigned to (or classified as) class i.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return integer value of Positive Predicted Values
#'
#'

positive_predicted_values <- function(confusion_matrix) {
  # Compute Positive Predicted Values
  PPV <- true_positive(confusion_matrix) /
    (true_positive(confusion_matrix) + false_positive(confusion_matrix))

  # Output
  return(PPV)
}
