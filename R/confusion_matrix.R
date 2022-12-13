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
#' @export
#'
#' @examples
#' # Assuming data is:
#' y_real <- matrix(
#'   c(
#'     1, 0, 0,
#'     0, 1, 0,
#'     0, 0, 1
#'   ),
#'   ncol = 3, byrow = TRUE
#' )
#' y_predicted <- matrix(
#'   c(
#'     0.6, 0.1, 0.3,
#'     0.7, 0.2, 0.1,
#'     0.2, 0.3, 0.5
#'   ),
#'   ncol = 3, byrow = TRUE
#' )
#'
#' # Resulting confusion matrix is:
#' confusion_matrix(y_real, y_predicted)
#'
confusion_matrix <- function(y_real,
                             y_predicted,
                             threshold = 0.5,
                             usNA = "ifany") {
  # Check for binary suitability
  if (is.vector(y_real) & is.vector(y_predicted)) {
    # Get predicted classes
    y_ch_predicted <- as.integer(y_predicted >= threshold)
    # Confusion matrix generation
    conf_m <- table(y_real, y_ch_predicted)

    # Output
    return(conf_m)
  } else
  # Checl for multiclass case suitability
  if (
    (is.data.frame(y_real) & is.data.frame(y_predicted)) |
      (is.matrix(y_real) & is.matrix(y_predicted))
  ) {
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
      conf_m <- table(y_real, y_ch_predicted)
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
