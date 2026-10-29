test_that("LABEL thresholds are the split-conformal order statistics", {
  d <- sim_probs(600, seed = 11)
  sel <- selective_fit(d$p, d$y, strategy = "label", target = target_level(0.1))
  for (k in seq_along(sel$classes)) {
    sk <- sort(d$p[as.integer(d$y) == k, k])
    idx <- min(max(ceiling(0.1 * (length(sk) + 1) - 1), 1), length(sk))
    expect_equal(unname(sel$threshold[k]), sk[idx])
  }
})

test_that("LABEL accepts singletons and empty sets, rejects ambiguous ones", {
  d <- sim_probs(600, seed = 11)
  sel <- selective_fit(d$p, d$y, strategy = "label", target = target_level(0.2))
  out <- predict(sel, d$p)
  expect_equal(out$.accept, out$.set_size <= 1)
  above <- sweep(d$p, 2, sel$threshold, ">=")
  expect_equal(out$.set_size, unname(rowSums(above)))
  one <- out$.set_size == 1
  expect_equal(as.character(out$.pred_class[one]),
               sel$classes[max.col(above[one, , drop = FALSE] * 1)])
  expect_true(all(is.na(out$.pred_selective[!out$.accept])))
})

test_that("LABEL requires truth and rejects threshold/level misuse", {
  d <- sim_probs(200)
  expect_error(selective_fit(d$p, strategy = "label"), "truth")
  expect_error(selective_fit(d$p, d$y, strategy = "label",
                      target = target_threshold(0.3)), "label")
  expect_error(selective_fit(d$p, d$y, target = target_level(0.1)), "label")
  expect_error(target_level(2), "alpha")
})

test_that("LABEL coverage target and curve", {
  d <- sim_probs(800, seed = 12)
  sel <- selective_fit(d$p, d$y, strategy = "label",
                target = target_coverage(0.6), n_grid = 201)
  expect_equal(sel$coverage, 0.6, tolerance = 0.05)
  cv <- selective_curve(sel)
  expect_true(all(c("coverage", "risk", "iam") %in% names(cv)))
  expect_equal(cv$coverage[nrow(cv)], 1)
  expect_lt(cv$coverage[1], 0.3)
  expect_true(is.finite(aurc(cv)))
  m <- selective_fit(d$p, d$y, strategy = "label",
              target = target_metric(metric_accuracy(), 0.8))
  expect_gte(m$performance[["accuracy"]], 0.8 - 1e-8)
})
