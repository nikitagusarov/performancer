#################################
# Confusion matrix construction #
#################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Confusion matrix construction
#' @description Construct confusion matrix based on matrix form input.
#' The confusion matrix is tipically defined with respect to some learning algorithm.
#' The diagonal entries denote the correctly classified examples for respective class.
#' All the nondiagonal entries denote misclassifications.
#'
#' @param y_real Observed values to compare with in matrix format.
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return matrix Confusion matrix.
#'
#'

confusion_matrix <- function(y_real,
                             y_predicted) {
  # Check dimensions
  if (
    !all(
      dim(y_real),
      dim(y_predicted)
    )
  ) {
    stop("The dimensions of inputs differ. Aborting ...")
  }

  # Get predicted classes
  y_ch_predicted <- max.col(y_predicted)
  # Get real classes
  y_ch_real <- max.col(y_real)

  # Confusion matrix generation
  conf_m <- table(y_ch_real, y_ch_predicted)

  # Output
  return(conf_m)
}
