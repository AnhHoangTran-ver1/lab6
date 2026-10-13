#' Brute force solution to the knapsack problem
#'
#' Each combination is represented by the binary form of an integer from 1 to 2^n - 1, 
#' where a bit equal to 1 means the corresponding item is included. The complexity
#' is O(2^n) => feasible for a small number of items only 
#'
#' @param x A data.frame with two numeric columns: weights and values. All weights and values must be positive.
#' @param W A single positive number, the capacity of the knapsack.
#'
#' @return A list with two elements:
#'   \item{value}{The maximum total value of items that fit in the knapsack.}
#'   \item{elements}{The row numbers of \code{x} of the chosen items.}
#'   
#' @export
knapsack_brute_force <- function(x, W) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (!is.numeric(W) || length(W) != 1 || is.na(W) || W <= 0) {
    stop("W must be a single positive number")
  }
  
  n <- nrow(x)
  if (n == 0) {
    return(list(value = 0, elements = integer(0)))
  }
  if (n > 31) { #intToBits() only handles integers up to 2^31
    stop("brute force is only feasible for up to 31 items")
  }
  
  w <- x$w
  v <- x$v
  best_value <- 0
  best_elements <- integer(0)
  
  for (i in 1:(2^n - 1)) {
    # Bits of i (least significant first) tell which items are included
    chosen <- which(as.logical(intToBits(i)[1:n]))
    
    total_weight <- sum(w[chosen])
    if (total_weight <= W) {
      total_value <- sum(v[chosen])
      if (total_value > best_value) {
        best_value <- total_value
        best_elements <- chosen
      }
    }
  }
  
  list(value = round(best_value), elements = best_elements)
}