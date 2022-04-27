#################################################
# Receiver Operating Characteristic (ROC) curve #
#################################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Receiver Operating Characteristic (ROC)
#' @description Get Receiver Operating Characteristic (ROC) curve construction.
#' An ROC curve is a plot in which the horizontal axis (the x axis) denotes the false-positive rate.
#' The vertical axis (the y axis) denotes the true-positive rate of a classifier.
#' ROC analysis studies the relationship between the sensitivity and the specificity of the classifier.
#' It makes sence in the context of binary classification.
#'
#' @param y_real Observed values to compare with in matrix format.
#' @param y_predicted Predicte values (probabiblities by class).
#' @param threshold_range Decision boundary threshold range.
#' Defaults to `c(0, 1)` for basic probabilistic classifier, but other values may be used.
#' @param threshold_step Step to use over the boundary threshold range.
#' Defaults to 0.01 for binary classification.
#'
#' @return matrix of ROC curve coordinates for given probabilistic classifier output.
#'
#' @export
#'
#' @import foreach

roc_curve <- function(y_real,
                      y_predicted,
                      threshold_range = c(0, 1),
                      threshold_step = 0.01) {
  # Avoid varning on check
  i <- NULL

  # Class chekc
  if (!(is.vector(y_real) & is.vector(y_predicted))) {
    stop("The inputs should be probability vectors.")
  }

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
      threshold = i
    )

    # Get coordinates
    coord <- c(
      x = false_positive_rate(conf_m),
      y = true_positive_rate(conf_m)
    )
  }

  # Set class
  class(coord_full) <- "roc_curve"

  # Return
  return(coord_full)
}



#' @title Plot ROC coordinates
#' @description Plot ROC points using provided coordinates.
#'
#' @param x A ROC coordinates object.
#' A simple data.frame containing respective `x` and `y` coordinates.
#' The coordinates should be ROC space compliant.
#' @param ... Other parameters passed to base plot function.
#'
#' @return plot of a ROC curve for given `roc_curve`
#'
#' @export
#'
#' @importFrom graphics plot text abline

plot.roc_curve <- function(x, ...) {
  # Get coordinates
  plot(
    # Coordinates
    x = x$x,
    y = x$y,
    # Limits
    xlim = c(0, 1),
    ylim = c(0, 1),
    # Labels
    main = "ROC",
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
