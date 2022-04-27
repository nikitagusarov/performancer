############
# Accuracy #
############

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Accuracy
#' @description Compute Accuracy using confusion matrix.
#' Accuracy the describes the fraction of correctly classified instances in the test set.
#'
#' @param confusion_matrix Confusion matrix to be used.
#' @param r Skew Ratio parameter, which is required to calculate skew-sensitive form of Accuracy,
#' which is a class specific measure.
#' In the case of symmetric misclassification cost (in absence of knowledge about costs),
#' `r` can represent the class ratio.
#' In the event of `r` representing the class ratio,
#' it can be seen that a value of `r` less than unity weighs the positive examples more than the negative ones and vice versa.
#' Default to NULL, which calculates overall (class-insensitive) accuracy.
#'
#' @return integer value of Accuracy
#'
#' @export
#'
#'

accuracy <- function(confusion_matrix,
                     r = NULL) {
  if (is.null(r)) {
    # Compute Accuracy
    AC <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  } else if (is.numeric(r)) {
    # Compute skew-sensitive accuracy
    AC <- (
      true_positive_rate(confusion_matrix) +
        ((1 - r) * false_positive_rate(confusion_matrix))
    ) / (1 + r)
  } else {
    stop("An incorrect Skew Ratio value provided.")
  }

  # Output
  return(AC)
}
