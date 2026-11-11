wide_test_data <- function(n = 1L) {
  x <- redlistManipulatr:::wideform()[rep(1L, n), , drop = FALSE]
  rownames(x) <- NULL
  x$iucn_id <- seq_len(n)
  x$season <- rep(1, n)
  x$binomial <- paste("Test species", seq_len(n))
  x
}
