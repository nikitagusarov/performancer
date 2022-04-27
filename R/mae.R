#############################
# Mean Absolute Error (MAE) #
#############################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Mean Absolute Error (MAE)
#' @description Compute Mean Absolute Error (MAE).
#' #'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicted values (probabiblities by class).
#' @param rowsums Whether in case of multiple classes in dataframe the metrics should be summed across classes before calculating the mean.
#'
#' @return integer value of Mean Absolute Error (MAE)
#'
#' @export
#'
#'

mae <- function(y_real,
                y_predicted,
                rowsums = TRUE) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  if (is.vector(y_real) | (rowsums == FALSE)) {
    # Compute MAE
    mae <- mean(
      abs(y_real - y_predicted)
    )
  } else
  if (rowsums == TRUE) {
    # Compute MAE
    mae <- mean(
      abs(y_real - y_predicted)
    )
  }

  # Output
  return(mae)
}
