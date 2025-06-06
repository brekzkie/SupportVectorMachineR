#' Evaluate SVM Model
#'
#' Menghitung akurasi dan confusion matrix dari hasil prediksi SVM.
#'
#' @param y_true Vector berisi label asli (ground truth), berupa angka -1 dan 1.
#' @param y_pred Vector hasil prediksi dari model SVM.
#'
#' @return List dengan dua elemen:
#' \describe{
#'   \item{accuracy}{Akurasi klasifikasi (nilai antara 0 dan 1).}
#'   \item{confusion_matrix}{Tabel confusion matrix: Predicted vs Actual.}
#' }
#'
#' @examples
#' y_true <- c(1, -1, 1, -1, 1)
#' y_pred <- c(1, -1, -1, -1, 1)
#' evaluate_model(y_true, y_pred)
#'
#' @export

evaluate_model <- function(y_true, y_pred) {
  acc <- sum(y_true == y_pred) / length(y_true)
  cm <- table(Predicted = y_pred, Actual = y_true)
  return(list(accuracy = acc, confusion_matrix = cm))
}

