#' Cohen's Kappa Confidence Interval
#'
#' @param x A table.
#' @param level Level of the confidence interval.
#'
#' @return
#' @export
kappa_ci <- function(x, level = 0.95) {
  n <- sum(x)
  c <- (n + 3) * (n + 2) * (n + 1) / 6
  a <- matrix(0, ncol = 7, nrow = c)
  i <- 1
  for (m00 in 0:n) {
    for (m01 in 0:(n - m00)) {
      for (m10 in 0:(n - m00 - m01)) {
        m11 <- n - m00 - m01 - m10
        m_vec <- c(m00, m01, m10, m11)
        a[i, 1:4] <- m_vec
        i <- i + 1
      }
    }
  }
  a[, 5] <- ((a[, 4] + a[, 1]) / n - ((a[, 3] + a[, 4]) * (a[, 2] + a[, 4]) +
    (a[, 1] + a[, 2]) * (a[, 1] + a[, 3])) / n^2) /
    (1 - ((a[, 3] + a[, 4]) * (a[, 2] + a[, 4]) + (a[, 1] + a[, 2]) *
    (a[, 1] + a[, 3])) / n^2)
  a[c(1, c), 5] <- 1
  p00 <- x[1, 1] / n
  p01 <- x[1, 2] / n
  p10 <- x[2, 1] / n
  p11 <- x[2, 2] / n
  a[, 6] <- factorial(n) / (factorial(a[, 1]) * factorial(a[, 2]) *
    factorial(a[, 3]) * factorial(a[, 4])) *
    p00^a[, 1] * p01^a[, 2] * p10^a[, 3] * p11^a[, 4]
  a <- a[order(a[, 5]), ]
  a[, 7] <- cumsum(a[, 6])
  ll <- (1 - level) / 2
  lu <- 1 - (1 - level) / 2
  q <- cut(a[, 7], c(0, ll, lu, 1))
  tq <- cumsum(table(q))
  cl <- (a[tq[1], 5] * (a[tq[1] + 1, 7] - ll) + a[tq[1] + 1, 5] *
    (ll - a[tq[1], 7])) / (a[tq[1] + 1, 7] - a[tq[1], 7])
  cu <- (a[tq[2], 5] * (a[tq[2] + 1, 7] - ll) + a[tq[2] + 1, 5] *
    (ll - a[tq[2], 7])) / (a[tq[2] + 1, 7] - a[tq[2], 7])
  c(cl, cu)
}
