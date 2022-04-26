#######################
# Binary Crossentropy #
#######################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Binary Crossentropy
#' @description Compute Binary Crossentropy.
#' The squared loss, in a sense, quantifies the error (or alternatively closeness) of the predicted label to the true label.
#' When specialized to the case of probabilistic classifiers, this then can be interpreted as a reliability measure.
#' This function assumes that inputs are given as probability vectors (and not matrices).
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return integer value of Binary Crossentropy
#'
#'

binary_crossentropy <- function(y_real,
                                y_predicted) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  # Vectorial case
  if (is.vector(y_real)) {
    # Compute MSE
    BC <- -mean(
      ((y_real * log(y_predicted))) +
        ((1 - y_real) * log(1 - y_predicted))
    )
  } else {
    BC <- mean(
      -rowSums(y_real * log(y_predicted))
    )
  }

  # Output
  return(BC)
}
