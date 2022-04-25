#####################################
# Kullback–Leibler Divergence (KLD) #
#####################################

# nikita.gusarov@univ-grenoble-alpes.fr - April 2022

#' @title Kullback–Leibler Divergence (KLD)
#' @description Compute Kullback–Leibler Divergence (KLD) using confusion matrix.
#' KL divergence basically just finds the difference between the entropies of the two distributions `P(y|f)` and `p(y)`.
#'
#' @param y_real Observed values (integers) to compare with
#' (in matrix format for multiclass classification).
#' @param y_predicted Predicte values (probabiblities by class).
#'
#' @return integer value of Kullback–Leibler Divergence (KLD)
#'
#'

kl_divergence <- function(y_real,
                          y_predicted) {
  # Check probabilities
  if (!(
    all((0 <= y_real) <= 1) & 
    all((0 <= y_pred) <= 1)
  )) {
    stop("The provided observations are not within the (0, 1) range.")
  }

  # Compute KL Divergence
  KLD <- y_true *
    replace(
      log(y_true / y_pred),
      is.infinite(log(y_true / y_pred)),
      0
    )
  
  # Observation by observation case
  # compute mean across observations
  if (!is.vector(KLD)) {
    # Summarise
    KLD <- mean(
      rowSums(KLD)
    )
  } else 
  # else compute the summ
  if (is.vector(KLD)) {
    KLD <- sum(KLD)
  } else {
    stop("No sensible format of KLD provided")
  }

  # Output
  return(KLD)
}