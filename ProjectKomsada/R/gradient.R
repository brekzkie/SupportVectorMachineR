#' Compute the Gradient of the Hinge Loss Function for SVM
#'
#' This function computes the gradients of the hinge loss function for a linear SVM model.
#' It calculates the gradients with respect to the weights (`w`) and the bias (`b`), including
#' the regularization term for weight decay.
#'
#' @param X A numeric matrix of shape (n_samples, n_features), representing the input data.
#' @param y A numeric vector of labels (either -1 or 1), with length equal to `nrow(X)`.
#' @param w A numeric vector of weights with length equal to `ncol(X)`.
#' @param b A numeric value representing the bias term.
#' @param lambda A numeric value representing the regularization strength (default is 0.01).
#'
#' @return A list containing:
#' \describe{
#'   \item{dw}{A numeric vector of gradients with respect to the weights.}
#'   \item{db}{A numeric value representing the gradient with respect to the bias.}
#' }
#'
#' @details The gradient is calculated for each sample, and the total gradient is averaged over
#' all samples. Additionally, a regularization term is added to the gradient of the weights to penalize
#' large weight values, as follows:
#' \deqn{dw = \frac{1}{n} \sum_{i=1}^{n} \nabla_w L_i + 2\lambda w}
#' where \(\nabla_w L_i\) is the gradient of the hinge loss for each sample and \( \lambda \) is the
#' regularization parameter.
#'
#' @examples
#' X <- matrix(c(2, 3, 4, 5), nrow = 2)
#' y <- c(1, -1)
#' w <- c(0.5, -0.3)
#' b <- 0.1
#' compute_gradient(X, y, w, b, lambda = 0.01)
#'
#' @export
compute_gradient <- function(X, y, w, b, lambda = 0.01) {
  dw <- rep(0, length(w))
  db <- 0

  for (i in 1:nrow(X)) {
    margin <- y[i] * (sum(X[i,] * w) + b)
    if (margin < 1) {
      dw <- dw + (-y[i] * X[i, ])
      db <- db - y[i]
    }
  }

  dw <- dw / nrow(X) + 2 * lambda * w
  db <- db / nrow(X)

  return(list(dw = dw, db = db))
}
