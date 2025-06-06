#' Initialize Weights and Bias
#'
#' This function initializes the weights and bias for a linear model.
#' The weights are initialized to zero for each feature, and the bias is set to zero.
#'
#' @param n_features Integer. The number of features (i.e., columns in the input data).
#'
#' @return A list containing:
#' \describe{
#'   \item{w}{A numeric vector of zeros with length equal to `n_features`.}
#'   \item{b}{A numeric value representing the bias, initialized to 0.}
#' }
#'
#' @examples
#' init <- initialize_weights(5)
#' init$w  # weight vector
#' init$b  # bias term
#'
#' @export
initialize_weights <- function(n_features){
  w <- rep(0, n_features)
  b <- 0
  return(list(w = w, b = b))
}

#' Compute Hinge Loss for Linear Classifier
#'
#' This function calculates the total hinge loss for a linear classifier. Hinge loss
#' is used in Support Vector Machines and is defined as:
#' \deqn{L = \sum_{i=1}^{n} \max(0, 1 - y_i(w^T x_i + b))}
#' where \eqn{y_i} are the labels, \eqn{x_i} are the input features,
#' \eqn{w} is the weight vector, and \eqn{b} is the bias.
#'
#' @param X A numeric matrix of shape (n_samples, n_features), where each row is a sample.
#' @param y A numeric vector of labels of length `nrow(X)`. Assumes labels are numeric (e.g., -1 and 1).
#' @param w A numeric vector of weights of length equal to `ncol(X)`.
#' @param b A numeric value representing the bias term.
#'
#' @return A numeric value representing the total hinge loss over all samples.
#'
#' @examples
#' X <- matrix(rnorm(20), nrow = 5)
#' y <- c(1, -1, 1, -1, 1)
#' init <- initialize_weights(ncol(X))
#' hinge_loss(X, y, init$w, init$b)
#'
#' @export
hinge_loss <- function(X, y, w, b) {
  total_loss <- 0
  for (i in 1:nrow(X)) {
    margin <- y[i] * (sum(X[i,] * w) + b)
    total_loss <- total_loss + max(0, 1 - margin)
  }
  return(total_loss)
}
