% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_model.R
\name{evaluate_model}
\alias{evaluate_model}
\title{Evaluate SVM Model}
\usage{
evaluate_model(y_true, y_pred)
}
\arguments{
\item{y_true}{Vector berisi label asli (ground truth), berupa angka -1 dan 1.}

\item{y_pred}{Vector hasil prediksi dari model SVM.}
}
\value{
List dengan dua elemen:
\describe{
  \item{accuracy}{Akurasi klasifikasi (nilai antara 0 dan 1).}
  \item{confusion_matrix}{Tabel confusion matrix: Predicted vs Actual.}
}
}
\description{
Menghitung akurasi dan confusion matrix dari hasil prediksi SVM.
}
\examples{
y_true <- c(1, -1, 1, -1, 1)
y_pred <- c(1, -1, -1, -1, 1)
evaluate_model(y_true, y_pred)

}
