#########################################
# Mean Squared Logarithmic Error (MSLE) #
#########################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Mean Squared Logarithmic Error (MSLE)
#' @description Compute Mean Squared Logarithmic Error (MSLE).
#' #'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#' @param rowsums Whether in case of multiple classes in dataframe the metrics should be summed across classes before calculating the mean.
#'
#' @return integer value of Mean Squared Logarithmic Error (MSLE)
#'
#'

msle <- function(y_real,
                 y_predicted,
                 rowsusm = TRUE) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  if (is.vector(y_real) | (rowsums == FALSE)) {
    # Compute MSLE
    msle <- mean(
      (log(y_real + 1) - log(y_predicted + 1))^2
    )
  } else
  if (rowsums == TRUE) {
    # Compute MSLE
    msle <- mean(
      (log(y_real + 1) - log(y_predicted + 1))^2
    )
  }

  # Output
  return(msle)
}
