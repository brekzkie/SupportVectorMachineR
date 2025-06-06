#' Predict Labels Using Linear SVM Model
#'
#' This function performs predictions on input data using a linear Support Vector Machine (SVM) model.
#' It applies the linear decision function and assigns class labels based on the sign of the output.
#'
#' @param X A numeric matrix of shape (n_samples, n_features), representing the input data.
#' @param w A numeric vector of weights corresponding to the features (length equal to `ncol(X)`).
#' @param b A numeric value representing the bias term.
#'
#' @return A numeric vector of predicted labels with values either 1 or -1.
#'
#' @details The function computes the decision function \eqn{w^T x + b} for each sample \eqn{x},
#' and returns:
#' \itemize{
#'   \item \code{1} if the result is greater than or equal to 0,
#'   \item \code{-1} otherwise.
#' }
#'
#' @examples
#' X <- matrix(c(2, 3, 4, 5), nrow = 2)
#' w <- c(0.5, -0.3)
#' b <- 0.1
#' predict_svm(X, w, b)
#'
#' @export
predict_svm <- function(X, w, b) {
  n <- nrow(X)
  result <- numeric(n)
  dot_product <- 0

  for (i in 1:n) {
    for (j in 1:length(w)) {
      dot_product <- dot_product + w[j] * X[i, j]
    }

    pred_raw <- dot_product + b

    if (pred_raw >= 0) {
      result[i] <- 1
    } else {
      result[i] <- -1
    }
  }

  return(result)
}
