#' Cohen's Kappa Confidence Interval
#'
#' Implements the exact bootstrap confidence interval for Cohen's kappa in small
#' samples by Klar et al. (2002).
#'
#' @param x A 2x2 matrix
#' @param level Level of the confidence interval.
#'
#' @export
#'
#' @examples
#' tab <- matrix(c(10, 2, 2, 4), ncol = 2)
#' kappa_ci(tab)
kappa_ci <- function(x, level = 0.95) {
  if (!is.matrix(x)) stop("x isn't a matrix")
  if (any(dim(x) != 2)) stop("incorrect dimension of x")

  # Create matrix with all possible cases
  n <- sum(x)
  no_cases <- (n + 3) * (n + 2) * (n + 1) / 6
  all_cases <- matrix(0, ncol = 4, nrow = no_cases)
  i <- 1
  for (m00 in 0:n) {
    for (m01 in 0:(n - m00)) {
      for (m10 in 0:(n - m00 - m01)) {
        m11 <- n - m00 - m01 - m10
        m_vec <- c(m00, m01, m10, m11)
        all_cases[i, 1:4] <- m_vec
        i <- i + 1
      }
    }
  }

  # Calculate Kappa
  m00 <- all_cases[, 1]
  m01 <- all_cases[, 2]
  m10 <- all_cases[, 3]
  m11 <- all_cases[, 4]
  m0p <- m00 + m01
  m1p <- m10 + m11
  mp0 <- m00 + m10
  mp1 <- m01 + m11
  kappa <- ((m11 + m00) / n - (m1p * mp1 + m0p * mp0) / n^2) /
    (1 - (m1p * mp1 + m0p * mp0) / n^2)
  kappa[c(1, no_cases)] <- 1 # Is NaN because denominator is 0

  # Calculate probabilities
  p00 <- x[1, 1] / n
  p01 <- x[1, 2] / n
  p10 <- x[2, 1] / n
  p11 <- x[2, 2] / n
  prob <- factorial(n) / (factorial(m00) * factorial(m01) * factorial(m10) *
    factorial(m11)) * p00^m00 * p01^m01 * p10^m10 * p11^m11

  # Calculate bootstrap distribution
  sort_order <- order(kappa)
  kappa <- kappa[sort_order]
  prob <- prob[sort_order]
  agframe <- aggregate(prob, list(kappa), sum)
  browser()
  prob <- agframe[, 2]
  kappa <- agframe[, 1]
  cumprob <- cumsum(prob)

  # Calculate confidence limits
  qlow <- (1 - level) / 2
  qup <- 1 - (1 - level) / 2
  qcut <- cumsum(table(cut(cumprob, c(0, qlow, qup, 1))))
  clow <- (kappa[qcut[1]] * (cumprob[qcut[1] + 1] - qlow) +
    kappa[qcut[1] + 1] * (qlow - cumprob[qcut[1]])) /
    (cumprob[qcut[1] + 1] - cumprob[qcut[1]])
  cup <- (kappa[qcut[2]] * (cumprob[qcut[2] + 1] - qup) +
      kappa[qcut[2] + 1] * (qup - cumprob[qcut[2]])) /
    (cumprob[qcut[2] + 1] - cumprob[qcut[2]])

  c(clow, cup)
}
