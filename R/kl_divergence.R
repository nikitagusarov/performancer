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
  # Compute KL Divergence
  KLD <- y_true *
    replace(
      log(y_true / y_pred),
      is.infinite(log(y_true / y_pred)),
      0
    )
  # Output
  return(KLD)
}
