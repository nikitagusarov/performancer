#########################################
# Mean Absolute Percentage Error (MAPE) #
#########################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Mean Absolute Percentage Error (MAPE)
#' @description Compute Mean Absolute Percentage Error (MAPE).
#' This metric is not suitable for discrete analysis and probably will be removed from the package contents in near future.
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return integer value of Mean Absolute Percentage Error (MAPE)
#'
#' @export
#'
#'

mape <- function(y_real,
                 y_predicted) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  # Compute MAPE
  mape <- 100 * mean(
    abs((y_real - y_predicted) / y_real)
  )

  # Output
  return(mape)
}
