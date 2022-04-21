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
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#' @param threshold Used in binary classification case.
#' Defines the threshold of success in the classification output.
#' The decision rule applied is greater or equal.
#'
#' @return matrix Confusion matrix.
#'
#'

confusion_matrix <- function(y_real,
                             y_predicted,
                             threshold = 0.5) {
  # Check for binary suitability
  if (is.integer(y_real) & is.integer(y_predicted)) {
    # Get predicted classes
    y_ch_predicted <- as.integer(y_predicted >= threshold)
    # Confusion matrix generation
    conf_m <- table(y_ch_real, y_ch_predicted)
  } else
  # Checl for multiclass case suitability
  if (is.data.frame(y_real) & is.data.frame(y_predicted)) {
    # Check dimensions
    if (
      !all(
        dim(y_real),
        dim(y_predicted)
      )
    ) {
      stop("The dimensions of inputs differ. Aborting ...")
    }

    # Case of single column data.frame
    if (ncol(y_real) == 1) {
      # Get predicted classes
      y_ch_predicted <- as.integer(y_predicted >= threshold)
      # Confusion matrix generation
      conf_m <- table(y_ch_real, y_ch_predicted)
    } else {
      # Get predicted classes
      y_ch_predicted <- max.col(y_predicted)
      # Get real classes
      y_ch_real <- max.col(y_real)

      # Confusion matrix generation
      conf_m <- table(y_ch_real, y_ch_predicted)
    }

    # Output
    return(conf_m)
  }
}
