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
#' @param pr_coordinates A PR coordinates object.
#' A simple data.frame containing respective `x` and `y` coordinates.
#' The coordinates should be PR space compliant.
#'
#' @return plot of PR coordinates for given confusion matrix
#'
#' @export

plot.pr_coordinates <- function(pr_coordinates, ...) {
  # Get coordinates
  plot(
    # Coordinates
    x = pr_coordinates$x,
    y = pr_coordinates$y,
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
    pr_coordinates$x - 0.05,
    pr_coordinates$y - 0.05,
    labels = rownames(pr_coordinates)
  )

  # Add middle line
  abline(
    a = 0, b = 1,
    col = "red",
    add = TRUE
  )
}
