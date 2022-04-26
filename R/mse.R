###########################
# Mean Square Error (MSE) #
###########################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Mean Square Error (MSE)
#' @description Compute Mean Square Error (MSE).
#' The squared loss, in a sense, quantifies the error (or alternatively closeness) of the predicted label to the true label.
#' When specialized to the case of probabilistic classifiers, this then can be interpreted as a reliability measure.
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#' @param rowsums Whether in case of multiple classes in dataframe the metrics should be summed across classes before calculating the mean.
#'
#' @return integer value of Mean Square Error (MSE)
#'
#'

mse <- function(y_real,
                y_predicted,
                rowsusm = TRUE) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  if (is.vector(y_real) | (rowsums == FALSE)) {
    # Compute MSE
    mse <- mean(
      (y_real - y_predicted)^2
    )
  } else
  if (rowsums == TRUE) {
    # Compute MSE
    mse <- mean(
      rowSums((y_real - y_predicted)^2)
    )
  }

  # Output
  return(mse)
}
