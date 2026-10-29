onehot <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE,
                 dimnames = list(NULL, c("a", "b", "c")))
uniform <- matrix(1 / 3, nrow = 2, ncol = 3,
                  dimnames = list(NULL, c("a", "b", "c")))

test_that("least confidence, margin and entropy follow the paper's definitions", {
  expect_equal(uncert_lc()$fun(onehot), c(0, 0))
  expect_equal(uncert_lc()$fun(uniform), rep(1 - 1 / 3, 2))
  expect_equal(uncert_margin()$fun(onehot), c(0, 0))
  expect_equal(uncert_margin()$fun(uniform), c(1, 1), tolerance = 1e-12)
  expect_equal(uncert_entropy()$fun(onehot), c(0, 0))
  expect_equal(uncert_entropy()$fun(uniform), rep(log(3), 2))
})

test_that("normalize = TRUE rescales to [0, 1]", {
  expect_equal(uncert_lc(normalize = TRUE)$fun(uniform), c(1, 1))
  expect_equal(uncert_entropy(normalize = TRUE)$fun(uniform), c(1, 1))
  d <- sim_probs(200)
  for (sc in list(uncert_lc(TRUE), uncert_margin(), uncert_entropy(TRUE))) {
    s <- sc$fun(d$p)
    expect_true(all(s >= 0 & s <= 1))
  }
})

test_that("scores order rows sensibly", {
  p <- rbind(c(0.9, 0.05, 0.05), c(0.5, 0.3, 0.2))
  colnames(p) <- c("a", "b", "c")
  for (sc in list(uncert_lc(), uncert_margin(), uncert_entropy())) {
    s <- sc$fun(p)
    expect_lt(s[1], s[2])
  }
})

test_that("margin score uses the two largest probabilities", {
  p <- matrix(c(0.6, 0.3, 0.1), nrow = 1,
              dimnames = list(NULL, c("a", "b", "c")))
  expect_equal(uncert_margin()$fun(p), 1 - (0.6 - 0.3))
})

test_that("variance score matches a hand calculation", {
  draws <- array(c(0.2, 0.8, 0.6, 0.4), dim = c(1, 2, 2),
                 dimnames = list(NULL, c("a", "b"), NULL))
  # p_a draws: 0.2, 0.6 -> sample variance 0.08; p_b draws: 0.8, 0.4 -> 0.08
  p <- rowMeans(draws, dims = 2)
  expect_equal(uncert_var()$fun(p, draws), 0.08)
})

test_that("mutual information matches its definition", {
  draws <- array(c(0.2, 0.8, 0.6, 0.4), dim = c(1, 2, 2),
                 dimnames = list(NULL, c("a", "b"), NULL))
  H <- function(p) -sum(p * log(p))
  expected <- H(c(0.4, 0.6)) - mean(c(H(c(0.2, 0.8)), H(c(0.6, 0.4))))
  p <- rowMeans(draws, dims = 2)
  expect_equal(uncert_mi()$fun(p, draws), expected)
  expect_gt(expected, 0)
})

test_that("variance and MI vanish when all draws agree", {
  one <- sim_draws(10, m = 2)[, , 1]
  draws <- array(rep(one, 5), dim = c(10, 3, 5),
                 dimnames = list(NULL, letters[1:3], NULL))
  p <- rowMeans(draws, dims = 2)
  expect_equal(uncert_var()$fun(p, draws), rep(0, 10), tolerance = 1e-12)
  expect_equal(uncert_mi()$fun(p, draws), rep(0, 10), tolerance = 1e-12)
})

test_that("draw-based scores need draws", {
  p <- sim_probs(10)$p
  expect_error(sp_compute_score(uncert_var(), p, NULL), "posterior draws")
  expect_error(sp_compute_score(uncert_mi(), p, NULL), "posterior draws")
})

test_that("custom scores are validated", {
  p <- sim_probs(10)$p
  bad_len <- uncert_custom(function(p) 1)
  expect_error(sp_compute_score(bad_len, p), "one value per row")
  bad_na <- uncert_custom(function(p) rep(NA_real_, nrow(p)))
  expect_error(sp_compute_score(bad_na, p), "missing or non-finite")
  expect_error(uncert_custom("nope"), "must be a function")
})

test_that("probability input is validated and normalised", {
  expect_error(sp_as_probs(1:3), "numeric matrix")
  expect_error(sp_as_probs(matrix(c(0.5, 0.5), ncol = 1)), "two probability")
  expect_error(sp_as_probs(matrix(c(-0.1, 1.1, 0.5, 0.5), 2)), "negative")
  expect_error(sp_as_probs(matrix(c(0.2, 0.2, 0.2, 0.2), 2)), "sum to 1")
  df <- data.frame(.pred_a = c(0.7, 0.2), .pred_b = c(0.3, 0.8),
                   .pred_class = factor(c("a", "b")))
  p <- sp_as_probs(df)
  expect_equal(colnames(p), c("a", "b"))
  p2 <- sp_as_probs(df, classes = c("b", "a"))
  expect_equal(colnames(p2), c("b", "a"))
  expect_error(sp_as_probs(df, classes = c("a", "z")), "no probability column")
})

test_that("posterior draws are validated and averaged", {
  arr <- sim_draws(20, m = 10)
  prep <- sp_prepare(arr)
  expect_equal(dim(prep$draws), dim(arr))
  expect_equal(unname(prep$P), unname(rowMeans(arr, dims = 2)),
               tolerance = 1e-12)
  expect_equal(colnames(prep$P), letters[1:3])
  expect_error(sp_prepare(arr[, , 1, drop = FALSE]), "two posterior draws")
  bad <- arr
  bad[1, 1, 1] <- 5
  expect_error(sp_prepare(bad), "sum to 1")
  shuffled <- arr[, c("c", "a", "b"), ]
  expect_equal(sp_prepare(shuffled, classes = letters[1:3])$P, prep$P)
})
