#######################################################
# Receiver Operating Characteristic (ROC) coordinates #
#######################################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Receiver Operating Characteristic (ROC)
#' @description Get Receiver Operating Characteristic (ROC) coordinates.
#' An ROC curve is a plot in which the horizontal axis (the x axis) denotes the false-positive rate.
#' The vertical axis (the y axis) denotes the true-positive rate of a classifier.
#' ROC analysis studies the relationship between the sensitivity and the specificity of the classifier.
#' It makes sence in the context of binary classification.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return matrix of ROC coordinates for given confusion matrix
#'
#' @export
#'
#'

roc_coordinates <- function(confusion_matrix) {
  # Get coordinates
  x <- false_positive_rate(confusion_matrix)
  y <- true_positive_rate(confusion_matrix)

  # Bind
  coord <- cbind(x, y)
  colnames(coord) <- c("x", "y")
  rownames(coord) <- colnames(confusion_matrix)

  # Set class
  class(coord) <- "roc_coordinates"

  # Return
  return(coord)
}



#' @title Plot ROC coordinates
#' @description Plot ROC points using provided coordinates.
#'
#' @param x A ROC coordinates object.
#' A simple data.frame containing respective `x` and `y` coordinates.
#' The coordinates should be ROC space compliant.
#' @param ... Other parameters passed to base plot function.
#'
#' @return plot of ROC coordinates for given coordinates
#'
#' @export
#'
#' @importFrom graphics plot text abline

plot.roc_coordinates <- function(x, ...) {
  # Get coordinates
  plot(
    # Coordinates
    x = x[, "x"],
    y = x[, "y"],
    # Limits
    xlim = c(0, 1),
    ylim = c(0, 1),
    # Labels
    main = "ROC",
    xlab = "FPR",
    ylab = "TPR",
    # Parameters
    type = "p",
    ...
  )

  # Label points
  text(
    x[, "x"] - 0.05,
    x[, "y"] - 0.05,
    labels = rownames(x)
  )

  # Add middle line
  abline(
    a = 0, b = 1,
    col = "red"
  )
}
