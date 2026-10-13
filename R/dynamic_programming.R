#' Dynamic programming solution to the knapsack problem
#'
#' Build a table of the best achievable value for every number of items
#' and every integer capacity from 0 to W. 
#' Requires integer weights. The complexity is O(Wn).
#'
#' @param x A data.frame with two numeric columns: weights (must
#'   be whole numbers) and values. 
#' @param W A single positive number, the capacity of the knapsack.
#'
#' @return A list with two elements:
#'   \item{value}{The maximum total value of items that fit in the knapsack.}
#'   \item{elements}{The row numbers of \code{x} of the chosen items.}
#'
#' @export
knapsack_dynamic <- function(x, W) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (any(x$w != round(x$w))) {
    stop("the dynamic programming algorithm requires integer weights")
  }
  if (!is.numeric(W) || length(W) != 1 || is.na(W) || W <= 0) {
    stop("W must be a single positive number")
  }
  
  n <- nrow(x)
  if (n == 0) {
    return(list(value = 0, elements = integer(0)))
  }
  
  w <- x$w
  v <- x$v
  
  # ---- Fill the table ----
  # Row 1 (0 items) and column 1 (0 capacity) stay 0.
  # Row i + 1 = using the first i items, column j + 1 = capacity j.
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  for (i in 1:n) {
    for (j in 0:W) {
      if (w[i] > j) {
        # Item i does not fit: best is the same as without it
        m[i + 1, j + 1] <- m[i, j + 1]
      } else {
        # Best of skipping item i or taking it
        m[i + 1, j + 1] <- max(m[i, j + 1],
                               m[i, j + 1 - w[i]] + v[i])
      }
    }
  }
  
  elements <- integer(0)
  j <- W
  for (i in n:1) {
    # If the value changed when item i was added, item i was taken
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      elements <- c(i, elements)
      j <- j - w[i]
    }
  }
  
  list(value = round(m[n + 1, W + 1]), elements = elements)
}