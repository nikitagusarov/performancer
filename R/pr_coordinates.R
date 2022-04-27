#####################################
# Precision–Recall (PR) coordinates #
#####################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Precision–Recall (PR)
#' @description Get Precision–Recall (PR) coordinates.
#' Precision–recall Curves, sometimes abbreviated as PR curves, are similar to
#' ROC curves and lift charts in that they explore the trade-off between the well-classified
#' positive examples and the number of misclassified negative examples.
#'
#' @param confusion_matrix Confusion matrix to be used.
#'
#' @return matrix of PR coordinates for given confusion matrix
#'
#' @export
#'
#'

pr_coordinates <- function(confusion_matrix) {
  # Get coordinates
  x <- precision(confusion_matrix)
  y <- recall(confusion_matrix)

  # Bind
  coord <- rbind(x, y)
  names(coord) <- c("x", "y")
  rownames(coord) <- colnames(confusion_matrix)

  # Set class
  class(coord) <- "pr_coordinates"

  # Return
  return(coord)
}



#' @title Plot PR coordinates
#' @description Plot PR points using provided coordinates.
#'
#' @param x A PR coordinates object.
#' A simple data.frame containing respective `x` and `y` coordinates.
#' The coordinates should be PR space compliant.
#' @param ... Other parameters passed to base plot function.
#'
#' @return plot of PR coordinates for given confusion matrix
#'
#' @export
#'
#' @importFrom graphics plot text abline

plot.pr_coordinates <- function(x, ...) {
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
    col = "red",
    add = TRUE
  )
}
