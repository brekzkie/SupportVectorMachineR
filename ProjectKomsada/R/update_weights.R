update_weights <- function(w, b, dw, db, lr = 0.01) {
  w <- w - lr * dw
  b <- b - lr * db
  return(list(w = w, b = b))
}
