#################################################
# Precision–Recall (PR) curve #
#################################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Precision–Recall (PR)
#' @description Get Precision–Recall (PR) curve construction.
#'
#' @param y_real Observed values to compare with in matrix format.
#' @param y_predicted Predicte values (probabiblities by class).
#' @param threshold_range Decision boundary threshold range.
#' Defaults to `c(0, 1)` for basic probabilistic classifier, but other values may be used.
#' @param threshold_step Step to use over the boundary threshold range.
#' Defaults to 0.01 for binary classification.
#' @param ... Other parameters passed to base plot function.
#'
#' @return matrix of PR curve coordinates for given probabilistic classifier output.
#'
#' @export
#'
#' @import foreach

pr_curve <- function(y_real,
                     y_predicted,
                     threshold_range = c(0, 1),
                     threshold_step = 0.01) {
  # Threshold sequence
  threshold <- seq(
    from = threshold_range[1],
    to = threshold_range[2],
    by = threshold_step
  )

  # Iterate
  coord_full <- foreach(i = threshold, .combine = "rbind") %do% {
    # Construct confusion matrix
    conf_m <- confusion_matrix(
      y_real,
      y_predicted,
      threshold = threshold
    )

    # Get coordinates
    coord <- c(
      x = precision(conf_m),
      y = recall(conf_m)
    )
  }

  # Set class
  class(coord_full) <- "pr_curve"

  # Return
  return(coord_full)
}



#' @title Plot PR coordinates
#' @description Plot PR points using provided coordinates.
#'
#' @param x A PR coordinates object.
#' A simple data.frame containing respective `x` and `y` coordinates.
#' The coordinates should be PR space compliant.
#' @param ... Other parameters passed to base plot function.
#'
#' @return plot of a PR curve for given `pr_curve`
#'
#' @export
#'
#' @importFrom graphics plot text abline

plot.pr_curve <- function(x, ...) {
  # Get coordinates
  plot(
    # Coordinates
    x = x$x,
    y = x$y,
    # Limits
    xlim = c(0, 1),
    ylim = c(0, 1),
    # Labels
    main = "PR",
    xlab = "FPR",
    ylab = "TPR",
    # Parameters
    type = "l",
    ...
  )

  # Label points
  text(
    x$x - 0.05,
    x$y - 0.05,
    labels = rownames(x)
  )

  # Add middle line
  abline(
    a = 0, b = 1,
    col = "red",
    add = TRUE
  )
}
