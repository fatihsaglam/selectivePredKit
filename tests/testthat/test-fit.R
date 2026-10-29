cal <- sim_probs(1000, seed = 11)

test_that("targets validate their inputs", {
  expect_error(target_threshold(Inf), "finite")
  expect_error(target_coverage(0), "\\(0, 1\\]")
  expect_error(target_coverage(1.2), "\\(0, 1\\]")
  expect_error(target_metric("accuracy", 0.9), "metric_custom")
  expect_error(target_metric(metric_accuracy(), NA_real_), "finite")
})

test_that("threshold target stores the threshold and needs no labels", {
  sel <- selective_fit(cal$p, target = target_threshold(0.4))
  expect_s3_class(sel, "sp_selector")
  expect_equal(unname(sel$threshold), rep(0.4, 3))
  expect_named(sel$threshold, c("a", "b", "c"))
  s <- uncert_margin()$fun(cal$p)
  expect_equal(sel$coverage, mean(s <= 0.4))
  expect_null(sel$curve)
})

test_that("coverage target reaches at least the requested coverage", {
  for (cv in c(0.3, 0.8, 1)) {
    sel <- selective_fit(cal$p, target = target_coverage(cv))
    expect_gte(sel$coverage, cv - 1e-12)
    expect_lt(sel$coverage, cv + 0.01)
  }
})

test_that("metric target maximises coverage subject to the condition", {
  sel <- selective_fit(cal$p, cal$y,
                target = target_metric(metric_accuracy(), 0.85), n_grid = 1001)
  expect_gte(sel$performance[["accuracy"]], 0.85 - 1e-12)
  expect_gt(sel$coverage, 0)
  expect_lt(sel$coverage, 1)

  # No larger threshold on the grid satisfies the condition
  cv <- sel$curve
  bigger <- cv[cv$threshold > sel$threshold[[1]], ]
  expect_true(all(bigger$accuracy < 0.85 - 1e-12))
})

test_that("error-rate targets use the 'lower is better' direction", {
  sel <- selective_fit(cal$p, cal$y,
                target = target_metric(metric_error_rate(), 0.1),
                n_grid = 1001)
  expect_lte(sel$performance[["risk"]], 0.1 + 1e-12)
})

test_that("an infeasible metric target warns and abstains on everything", {
  expect_warning(
    sel <- selective_fit(cal$p, cal$y,
                  target = target_metric(metric_accuracy(), 1.01)),
    "No threshold reaches"
  )
  expect_equal(sel$coverage, 0)
  expect_true(all(sel$threshold == -Inf))
  expect_true(!any(predict(sel, cal$p, type = "accept")))
})

test_that("an IAM target works and can be unreachable at low coverage", {
  sel <- selective_fit(cal$p, cal$y, target = target_metric(metric_iam(), 0.2),
                n_grid = 201)
  expect_gt(sel$coverage, 0)
  expect_gte(sel$performance[["iam"]], 0.2 - 1e-12)
})

test_that("metric target requires truth", {
  expect_error(
    selective_fit(cal$p, target = target_metric(metric_accuracy(), 0.9)),
    "`truth` is required"
  )
})

test_that("truth is checked against probs and classes", {
  expect_error(selective_fit(cal$p, cal$y[-1]), "length")
  bad <- as.character(cal$y)
  bad[1] <- "zzz"
  expect_error(selective_fit(cal$p, bad), "not among the classes")
})

test_that("the selector works with every probability-based score", {
  base_acc <- mean(cal$y == sp_pred_class(sp_as_probs(cal$p)))
  for (sc in list(uncert_lc(), uncert_margin(), uncert_entropy())) {
    sel <- selective_fit(cal$p, cal$y, score = sc, target = target_coverage(0.5))
    expect_gte(sel$coverage, 0.5)
    expect_gt(sel$performance[["accuracy"]], base_acc)
  }
})

test_that("the selector works with posterior draws", {
  arr <- sim_draws(400, m = 20)
  P <- sp_prepare(arr)$P
  set.seed(9)
  y <- factor(apply(P, 1, function(r) sample(colnames(P), 1, prob = r)),
              levels = colnames(P))
  for (sc in list(uncert_var(), uncert_mi(), uncert_margin())) {
    sel <- selective_fit(arr, y, score = sc, target = target_coverage(0.6))
    expect_gte(sel$coverage, 0.6 - 1e-12)
    expect_equal(sel$classes, letters[1:3])
  }
  # a draw-based score cannot be used with plain probabilities
  expect_error(selective_fit(P, y, score = uncert_var()), "posterior draws")
})

test_that("print works", {
  sel <- selective_fit(cal$p, cal$y, target = target_coverage(0.8))
  expect_output(print(sel), "sp_selector")
  expect_output(print(sel), "accuracy")
  sel_cc <- selective_fit(cal$p, cal$y, strategy = "class_conditional")
  expect_output(print(sel_cc), "thresholds")
})
