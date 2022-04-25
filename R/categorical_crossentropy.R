############################
# Categorical Crossentropy #
############################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Categorical Crossentropy
#' @description Compute Categorical Crossentropy.
#' The squared loss, in a sense, quantifies the error (or alternatively closeness) of the predicted label to the true label.
#' When specialized to the case of probabilistic classifiers, this then can be interpreted as a reliability measure.
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return integer value of Categorical Crossentropy
#'
#'

categorical_crossentropy <- function(y_real,
                                y_predicted) {
  # Check for Categorical suitability
  if (
    is.integer(y_real) & is.integer(y_predicted)
  ) {
    # Work on Categorical case
    y_real <- cbind(
      y_real,
      1 - y_real
    )
    y_predicted <- cbind(
      y_predicted,
      1 - y_predicted
    )
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
      # Work on Categorical case
      y_real <- cbind(
        y_real,
        1 - y_real
      )
      y_predicted <- cbind(
        y_predicted,
        1 - y_predicted
      )
    }

    # Compute MSE
    CC <- mean(
        - rowSums(y_real * log(y_predicted))
    )

    # Output
    return(CC)
  }
}
