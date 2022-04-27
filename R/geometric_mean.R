##################
# Geometric Mean #
##################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Geometric Mean
#' @description Compute Geometric Mean using confusion matrix.
#'
#' @param confusion_matrix Confusion matrix to be used.
#' @param type Specifies whether True Negative Rate ("tnr") or Precision ("prec") should be used for calculation of the geometric mean.
#' Defaults to "tnr".
#'
#' @return integer value of Geometric Mean
#'
#' @export
#'
#'

geometric_mean <- function(confusion_matrix,
                           type = "tnr") {
  if (type == "tnr") {
    # Compute Geometric Mean
    GM <- sqrt(
      true_positive_rate(confusion_matrix) *
        true_negative_rate(confusion_matrix)
    )
  } else if (type == "prec") {
    # Compute Geometric Mean
    GM <- sqrt(
      true_positive_rate(confusion_matrix) *
        precision(confusion_matrix)
    )
  } else {
    stop("An incorrect type specification was provided.")
  }

  # Output
  return(GM)
}
