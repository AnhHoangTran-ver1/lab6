
suppressWarnings(
  RNGversion(min(as.character(getRversion()), "3.5.3"))
)

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 2000

knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(
    dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
  )

  expect_named(dk, c("value", "elements"))
  expect_type(dk$value, "double")
  expect_true(is.numeric(dk$elements))
})

test_that("Function rejects erroneous input", {
  expect_error(knapsack_dynamic("hej", 3500))

  expect_error(
    knapsack_dynamic(x = knapsack_objects[1:8, ], W = -3500)
  )
})

test_that("Function returns correct results", {
  dk <- knapsack_dynamic(
    x = knapsack_objects[1:8, ],
    W = 3500
  )

  expect_equal(round(dk$value), 16770)

  expect_equal(
    round(sum(knapsack_objects[dk$elements, "v"])),
    round(dk$value)
  )

  expect_lte(
    sum(knapsack_objects[dk$elements, "w"]),
    3500
  )

  dk <- knapsack_dynamic(
    x = knapsack_objects[1:12, ],
    W = 3500
  )

  expect_equal(round(dk$value), 16770)

  expect_lte(
    sum(knapsack_objects[dk$elements, "w"]),
    3500
  )

  dk <- knapsack_dynamic(
    x = knapsack_objects[1:8, ],
    W = 2000
  )

  expect_equal(round(dk$value), 15428)

  expect_equal(
    round(sum(knapsack_objects[dk$elements, "v"])),
    round(dk$value)
  )

  expect_lte(
    sum(knapsack_objects[dk$elements, "w"]),
    2000
  )

  dk <- knapsack_dynamic(
    x = knapsack_objects[1:12, ],
    W = 2000
  )

  expect_equal(round(dk$value), 15428)

  expect_lte(
    sum(knapsack_objects[dk$elements, "w"]),
    2000
  )
})

test_that("Dynamic programming runs in reasonable time", {
  st <- system.time(
    dk <- knapsack_dynamic(
      x = knapsack_objects[1:16, ],
      W = 2000
    )
  )

  expect_true(as.numeric(st)[3] < 1)
})

test_that("Selected elements are valid", {
  dk <- knapsack_dynamic(
    x = knapsack_objects[1:12, ],
    W = 3500
  )

  expect_true(all(dk$elements %in% 1:12))
  expect_equal(length(unique(dk$elements)), length(dk$elements))
})
