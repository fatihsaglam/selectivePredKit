# Calibrated K-class data: labels are drawn from the stated probabilities, so
# confident rows really are more often correct.
sim_probs <- function(n, k = 3L, sd = 2.5, seed = 1L) {
  set.seed(seed)
  logits <- matrix(stats::rnorm(n * k, sd = sd), ncol = k)
  p <- exp(logits) / rowSums(exp(logits))
  colnames(p) <- letters[seq_len(k)]
  y <- factor(
    apply(p, 1, function(r) sample(colnames(p), 1, prob = r)),
    levels = colnames(p)
  )
  list(p = p, y = y)
}

# Imbalanced two-class data in which the minority class "b" is always
# predicted with less confidence than the majority class "a".
sim_imbalanced <- function(n = 2000, seed = 5L) {
  set.seed(seed)
  y <- factor(sample(c("a", "b"), n, replace = TRUE, prob = c(0.9, 0.1)),
              levels = c("a", "b"))
  conf <- ifelse(y == "a", stats::runif(n, 0.8, 1), stats::runif(n, 0.55, 0.85))
  p <- cbind(a = ifelse(y == "a", conf, 1 - conf),
             b = ifelse(y == "a", 1 - conf, conf))
  list(p = p, y = y)
}

# Posterior draws: n x K x M array around base probabilities.
sim_draws <- function(n = 50, k = 3L, m = 30L, spread = 0.5, seed = 3L) {
  set.seed(seed)
  base <- matrix(stats::rnorm(n * k), ncol = k)
  arr <- array(NA_real_, dim = c(n, k, m),
               dimnames = list(NULL, letters[seq_len(k)], NULL))
  for (j in seq_len(m)) {
    l <- base + matrix(stats::rnorm(n * k, sd = spread), ncol = k)
    arr[, , j] <- exp(l) / rowSums(exp(l))
  }
  arr
}
