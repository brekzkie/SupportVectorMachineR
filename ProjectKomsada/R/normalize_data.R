#' Normalize Data Using Z-Score Standardization
#'
#' This function performs column-wise normalization on a numeric matrix using
#' Z-score standardization. Each column will be transformed to have a mean of 0
#' and a standard deviation of 1.
#'
#' @param mat A numeric matrix. The function will normalize each column of the matrix.
#'
#' @return A numeric matrix with the same dimensions as `mat`, where each column has
#' been standardized to have zero mean and unit variance.
#'
#' @details This function manually computes the mean and standard deviation for each
#' column of the matrix, then applies Z-score normalization:
#' \deqn{z = \frac{x - \bar{x}}{s}}
#' where \eqn{\bar{x}} is the column mean and \eqn{s} is the column standard deviation.
#'
#' @examples
#' mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#' normalize_data(mat)
#'
#' @export
normalize_data <- function(mat) {
  n_row <- nrow(mat)
  n_col <- ncol(mat)
  scaled <- matrix(0, nrow = n_row, ncol = n_col)

  for (j in 1:n_col) {
    sumx <- 0
    for (i in 1:n_row) {
      sumx <- sumx + mat[i, j]
    }
    meanx <- sumx / n_row

    numerator <- 0
    for (i in 1:n_row) {
      numerator <- numerator + (mat[i, j] - meanx)^2
    }
    sd_x <- sqrt(numerator / (n_row - 1))

    for (i in 1:n_row) {
      scaled[i, j] <- (mat[i, j] - meanx) / sd_x
    }
  }

  return(scaled)
}
