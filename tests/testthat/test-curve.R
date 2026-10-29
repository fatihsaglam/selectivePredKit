d <- sim_probs(600, seed = 31)

test_that("curve has the expected columns and rising coverage", {
  cv <- selective_curve(d$p, d$y)
  expect_s3_class(cv, "sp_curve")
  expect_named(cv, c("alpha", "threshold", "coverage", "n_accepted", "risk",
                     "gen_risk", "accuracy", "iam", "coverage_a",
                     "coverage_b", "coverage_c"))
  expect_equal(nrow(cv), 101L)
  expect_false(is.unsorted(cv$coverage))
  expect_equal(max(cv$coverage), 1)
  ok <- !is.na(cv$risk)
  expect_equal(cv$accuracy[ok] + cv$risk[ok], rep(1, sum(ok)))
})

test_that("class-conditional curves have no single threshold column", {
  cv <- selective_curve(d$p, d$y, strategy = "class_conditional")
  expect_false("threshold" %in% names(cv))
  expect_false(is.unsorted(cv$coverage))
})

test_that("accuracy at full coverage equals overall accuracy", {
  cv <- selective_curve(d$p, d$y)
  overall <- mean(d$y == sp_pred_class(sp_as_probs(d$p)))
  expect_equal(cv$accuracy[nrow(cv)], overall)
  expect_equal(cv$gen_risk[nrow(cv)], 1 - overall)
})

test_that("a useful score has a falling risk-coverage curve", {
  cv <- selective_curve(d$p, d$y, uncert_margin())
  expect_lt(cv$risk[10], cv$risk[nrow(cv)])
})

test_that("n_grid sets the number of levels", {
  cv <- selective_curve(d$p, d$y, n_grid = 51)
  expect_equal(nrow(cv), 51L)
})

test_that("extra metrics are added as columns", {
  cv <- selective_curve(d$p, d$y, metrics = list(metric_balanced_accuracy(),
                                          metric_accuracy()))
  expect_true("balanced_accuracy" %in% names(cv))
  # name clash with the built-in `accuracy` column is resolved
  expect_true(any(grepl("^accuracy\\.", names(cv))))
  expect_error(selective_curve(d$p, d$y, metrics = list(1)), "metric_custom")
})

test_that("a selector can be evaluated on new data with stored thresholds", {
  new <- sim_probs(400, seed = 32)
  sel <- selective_fit(d$p, d$y)
  cv <- selective_curve(sel, new$p, new$y)
  expect_s3_class(cv, "sp_curve")
  expect_gt(max(cv$coverage), 0.9)
  expect_error(selective_curve(sel, new$p), "`truth` is required")
  no_truth <- selective_fit(d$p)
  expect_error(selective_curve(no_truth), "without `truth`")
  expect_s3_class(selective_curve(no_truth, new$p, new$y), "sp_curve")
})

test_that("the area helper follows its conventions", {
  x <- c(0.5, 1)
  y <- c(0.2, 0.4)
  expect_equal(sp_area(x, y, "constant"), 0.5 * 0.2 + 0.5 * 0.3)
  expect_equal(sp_area(x, y, "zero"), 0.5 * 0.2 / 2 + 0.5 * 0.3)
  expect_equal(sp_area(x, y, "none"), 0.5 * 0.3)
  # an undefined end point removes its segment
  expect_equal(sp_area(c(0, 0.5, 1), c(NA, 0.2, 0.4), "none"), 0.5 * 0.3)
  expect_true(is.na(sp_area(c(0, 1), c(NA, NA))))
})

test_that("areas of a perfect and an uninformative score behave", {
  # perfect classifier: no errors anywhere
  n <- 300
  y <- factor(sample(c("a", "b", "c"), n, TRUE))
  p <- matrix(0.05, n, 3, dimnames = list(NULL, levels(y)))
  p[cbind(seq_len(n), as.integer(y))] <- 0.9
  cv <- selective_curve(p, y)
  # all scores tie, so every point has coverage 1 and only the extended
  # area (completed to coverage 0) is non-trivial
  expect_equal(aurc(cv, extend = TRUE), 0)
  expect_equal(augrc(cv, extend = TRUE), 0)
  expect_equal(auac(cv, extend = TRUE), 1)
  expect_equal(ca_aurc(cv, extend = TRUE), 0)
  expect_equal(ca_augrc(cv, extend = TRUE), 0)
  expect_equal(aurc(cv), 0)
})

test_that("auc_all returns all six areas and accepts selectors", {
  cv <- selective_curve(d$p, d$y)
  a <- auc_all(cv)
  expect_named(a, c("AURC", "AUGRC", "AUAC", "AUIC", "CA_AURC", "CA_AUGRC"))
  expect_true(all(is.finite(a)))
  expect_lte(a[["AUGRC"]], a[["AURC"]])
  expect_equal(auc_all(selective_fit(d$p, d$y)), a)
  expect_error(auc_all(1), "selective_curve")
  expect_error(aurc(selective_fit(d$p)), "without `truth`")
})

test_that("AUAC and AURC are complementary", {
  cv <- selective_curve(d$p, d$y)
  expect_equal(auac(cv, extend = TRUE) + aurc(cv, extend = TRUE), 1)
  rng <- max(cv$coverage) - min(cv$coverage)
  expect_equal(auac(cv) + aurc(cv), rng)
})

test_that("plot methods run without error", {
  cv <- selective_curve(d$p, d$y)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(cv, "risk"))
  expect_silent(plot(cv, "iam"))
  expect_silent(plot(cv, "class_coverage"))
  expect_error(plot(cv, "nope"), "must be one of")
  expect_silent(plot(cv, "risk", by_class = TRUE))
  expect_silent(plot(cv, "gen_risk", by_class = "panels"))
  expect_error(plot(cv, "risk", by_class = "x"), "by_class")
  sel <- selective_fit(d$p, d$y)
  expect_silent(plot(sel))
  no_truth <- selective_fit(d$p)
  expect_error(plot(no_truth), "no curve")
})

test_that("auc_of integrates any column, including added metrics", {
  cv <- selective_curve(d$p, d$y, metrics = metric_macro_f1())
  expect_equal(auc_of(cv, "risk"), aurc(cv))
  expect_equal(auc_of(cv, "iam"), auic(cv))
  expect_equal(auc_of(cv, "risk", extend = TRUE), aurc(cv, TRUE))
  expect_true(is.finite(auc_of(cv, metric_macro_f1())))
  expect_error(auc_of(cv, "nope"), "not a column")
  expect_error(auc_of(cv, "coverage"), "not a column")
})
