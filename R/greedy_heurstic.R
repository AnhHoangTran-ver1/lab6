#'
#' Greedy heuristic for the knapsack problem
#'
#' Sorting the data frame in decreasing order of value per unit of weight, and then added to the knapsack one by one as long as
#' they still fit.
#'
#'
#' @param x A data frame with one row per item and two numeric, positive
#'   columns: the weights \code{w} in the first column and the values \code{v}
#'   in the second column. No missing values are allowed.
#' @param W A single positive number giving the weight capacity of the
#'   knapsack.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @examples
#' items <- data.frame(w = c(5, 4, 3), v = c(10, 20, 9))
#' greedy_knapsack(x = items, W = 8)
#'
#' @export



greedy_knapsack <- function(x,W){
  stopifnot(is.data.frame(x), is.numeric(W), length(W) == 1, W > 0,!anyNA(x), all(x > 0))
  ordered_list <- list()
  #for(i in 1:nrow(x)){
  #  ordered_list[i] <- x[[2]][i]/x[[1]][i]
  #}
  #ordered_list <- unlist(ordered_list)
  #return(sort(unlist(ordered_list), decreasing = TRUE))
  ord <- order(x$v / x$w, decreasing = TRUE)

  total_w <- 0
  total_v <- 0
  elements <- integer(0)

  for (i in ord) {
    if (total_w + x$w[i] > W) break
    total_w <- total_w + x$w[i]
    total_v <- total_v + x$v[i]
    elements <- c(elements, i)
  }
  list(value = round(total_v), elements = elements)
}

### optimized version

greedy_knapsack_optimized <- function(x, W) {
  ratio <- x$v / x$w
  ord   <- order(ratio, decreasing = TRUE)
  keep  <- ord[cumsum(x$w[ord]) <= W]
  list(value = round(sum(x$v[keep])), elements = keep)
}

#debug(greedy_knapsack)
#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

# checking for 1M objects

#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 1000000
#knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#                               v = runif(n = n, 0, 10000))

#system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))




