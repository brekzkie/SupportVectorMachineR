plot_decision <- function(X, y, w, b) {
  plot(X, col = ifelse(y == 1, "blue", "red"), pch = 19, main = "Decision Boundary")
  abline(a = -b / w[2], b = -w[1] / w[2], col = "black", lwd = 2)
}
