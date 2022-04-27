#####################################
# Kullback–Leibler Divergence (KLD) #
#####################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Kullback–Leibler Divergence (KLD)
#' @description Compute Kullback–Leibler Divergence (KLD) using confusion matrix.
#' KL divergence basically just finds the difference between the entropies of the two distributions `P(y|f)` and `p(y)`.
#' The inputs are assumed to be expressed in probabilistic terms.
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return integer value of Kullback–Leibler Divergence (KLD)
#'
#' @export
#'
#'

kl_divergence <- function(y_real,
                          y_predicted) {
  # Class chekc
  if (!any(class(y_real) == class(y_predicted))) {
    stop("The classes of input objects do not match.")
  }

  # Check probabilities
  if (!(
    all((0 <= y_real) <= 1) &
      all((0 <= y_predicted) <= 1)
  )) {
    stop("The provided observations are not within the (0, 1) range.")
  }

  # Compute KL Divergence
  KLD <- y_real *
    replace(
      log(y_real / y_predicted),
      is.infinite(log(y_real / y_predicted)),
      0
    )

  # Observation by observation case
  # compute mean across observations
  if (!is.vector(KLD)) {
    # Summarise
    KLD <- rowSums(KLD)
  } else
  # else compute the summ
  if (is.vector(KLD)) {
    KLD <- sum(KLD)
  } else {
    stop("No sensible format of KLD provided")
  }

  # Calculate mean
  KLD <- mean(KLD)

  # Output
  return(KLD)
}
