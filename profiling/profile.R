install.packages(c("profvis", "bench"))   # if needed

set.seed(42)
n <- 1000000
big <- data.frame(w = sample(1:4000, n, replace = TRUE),
                  v = runif(n, 0, 10000))

profvis::profvis({
  greedy_knapsack(big, W = 3500)
})

profvis::profvis({
  greedy_knapsack_optimized(big, W = 3500)
})


profvis::profvis({
  knapsack_dynamic(big, W = 3500)
})


profvis::profvis({
  brute_force_knapsack(knapsack_objects[1:20, ], W = 3500)
})

bench::mark(
  old = greedy_knapsack(big, 3500),
  new = greedy_knapsack_optimized(big, 3500),
  check = FALSE,
  iterations = 5
)[, c("expression", "median", "mem_alloc", "n_gc")]
