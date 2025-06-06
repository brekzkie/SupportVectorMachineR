#' Train a Linear SVM Model Using Gradient Descent
#'
#' This function trains a linear Support Vector Machine (SVM) model using gradient descent.
#' It updates the weights and bias iteratively for a specified number of epochs,
#' minimizing the hinge loss function with an optional regularization term.
#'
#' @param X A numeric matrix of shape (n_samples, n_features), representing the input data.
#' @param y A numeric vector of labels (either -1 or 1), with length equal to `nrow(X)`.
#' @param lr A numeric value representing the learning rate for gradient descent (default is 0.01).
#' @param epochs An integer specifying the number of epochs (iterations over the full dataset, default is 1000).
#' @param lambda A numeric value representing the regularization strength (default is 0.01).
#'
#' @return A list containing:
#' \describe{
#'   \item{w}{A numeric vector of optimized weights after training.}
#'   \item{b}{A numeric value representing the optimized bias after training.}
#' }
#'
#' @details This function uses the following steps:
#' \itemize{
#'   \item Initialize the weights and bias.
#'   \item For each epoch, compute the gradients of the hinge loss.
#'   \item Update the weights and bias using the gradients and the learning rate.
#'   \item Print the loss every 100 epochs to track progress.
#' }
#'
#' @examples
#' X <- matrix(c(2, 3, 4, 5), nrow = 2)
#' y <- c(1, -1)
#' model <- train_svm(X, y, lr = 0.01, epochs = 1000, lambda = 0.01)
#' model$w  # optimized weights
#' model$b  # optimized bias
#'
#' @export
train_svm <- function(X, y, lr = 0.01, epochs = 1000, lambda = 0.01) {
  init <- initialize_weights(ncol(X))
  w <- init$w
  b <- init$b

  for (i in 1:epochs) {
    grad <- compute_gradient(X, y, w, b, lambda)
    update <- update_weights(w, b, grad$dw, grad$db, lr)
    w <- update$w
    b <- update$b

    if (i %% 100 == 0) {
      cat("Epoch:", i, "Loss:", hinge_loss(X, y, w, b), "\n")
    }
  }
  return(list(w = w, b = b))
}
