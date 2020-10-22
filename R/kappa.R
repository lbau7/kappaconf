#' Cohen's Kappa
#'
#' Calculates Cohen's kappa.
#'
#' @param x A 2x2 matrix.
#'
#' @export
#'
#' @examples
#' tab <- matrix(c(10, 2, 2, 4), ncol = 2)
#' kappa(tab)
kappa <- function(x) {
  if (!is.matrix(x)) stop("x isn't a matrix")
  if (any(dim(x) != 2)) stop("incorrect dimension of x")
  n <- sum(x)
  n00 <- x[1, 1]
  n01 <- x[1, 2]
  n10 <- x[2, 1]
  n11 <- x[2, 2]
  n0p <- sum(x[1, ])
  n1p <- sum(x[2, ])
  np0 <- sum(x[, 1])
  np1 <- sum(x[, 2])
  ((n11 + n00) / n - (n1p * np1 + n0p * np0) / n^2) /
    (1 - (n1p * np1 + n0p * np0) / n^2)
}
