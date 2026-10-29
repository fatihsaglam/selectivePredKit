imb <- sim_imbalanced(3000, seed = 5)

test_that("class-conditional strategy gives one threshold per predicted class", {
  sel <- selective_fit(imb$p, imb$y, target = target_coverage(0.5),
                strategy = "class_conditional")
  expect_equal(sel$strategy, "class_conditional")
  expect_named(sel$threshold, c("a", "b"))
  expect_gt(sel$threshold[["b"]], sel$threshold[["a"]])
  # every predicted class is accepted at (about) the requested rate
  pred <- sp_pred_class(sp_as_probs(imb$p))
  acc <- predict(sel, imb$p, type = "accept")
  expect_equal(mean(acc[pred == "a"]), 0.5, tolerance = 0.02)
  expect_equal(mean(acc[pred == "b"]), 0.5, tolerance = 0.02)
})

test_that("the global strategy rejects the minority class, CC does not", {
  glob <- selective_fit(imb$p, imb$y, target = target_coverage(0.5))
  cc <- selective_fit(imb$p, imb$y, target = target_coverage(0.5),
               strategy = "class_conditional")
  mg <- selective_measures(predict(glob, imb$p, type = "accept"),
                    sp_pred_class(sp_as_probs(imb$p)), imb$y)
  mc <- selective_measures(predict(cc, imb$p, type = "accept"),
                    sp_pred_class(sp_as_probs(imb$p)), imb$y)
  expect_lt(mg$coverage_b, 0.05)
  expect_gt(mc$coverage_b, 0.4)
  # classical selection looks better on a global risk but collapses a class
  expect_true(is.na(mg$iam))
  expect_false(is.na(mc$iam))
})

test_that("CC keeps class coverage close to the diagonal, global does not", {
  cc <- selective_curve(imb$p, imb$y, strategy = "class_conditional")
  gl <- selective_curve(imb$p, imb$y, strategy = "global")
  dev_cc <- max(abs(cc$coverage_b - cc$coverage))
  dev_gl <- max(abs(gl$coverage_b - gl$coverage))
  expect_lt(dev_cc, 0.1)
  expect_gt(dev_gl, 0.3)
})

test_that("the imbalance-aware measures separate the strategies", {
  cc <- selective_curve(imb$p, imb$y, strategy = "class_conditional")
  gl <- selective_curve(imb$p, imb$y, strategy = "global")
  expect_gt(auic(cc), auic(gl))
})

test_that("a threshold target is only available for the global strategy", {
  expect_error(
    selective_fit(imb$p, imb$y, target = target_threshold(0.3),
           strategy = "class_conditional"),
    "global"
  )
})

test_that("metric target works with the class-conditional strategy", {
  sel <- selective_fit(imb$p, imb$y, target = target_metric(metric_iam(), 0.6),
                strategy = "class_conditional", n_grid = 201)
  expect_gte(sel$performance[["iam"]], 0.6 - 1e-12)
  expect_gt(sel$coverage, 0.3)
})

test_that("a class that is never predicted falls back to the global quantile", {
  p <- cbind(a = c(0.9, 0.8, 0.7, 0.9, 0.6), b = c(0.1, 0.2, 0.3, 0.1, 0.4))
  expect_warning(
    sel <- selective_fit(p, target = target_coverage(0.6),
                  strategy = "class_conditional"),
    "never predicted"
  )
  expect_equal(sel$threshold[["b"]], sel$threshold[["a"]])
})

test_that("predict uses the threshold of the predicted class", {
  sel <- selective_fit(imb$p, imb$y, target = target_coverage(0.5),
                strategy = "class_conditional")
  out <- predict(sel, imb$p)
  expect_equal(out$.accept,
               out$.score <= sel$threshold[as.integer(out$.pred_class)],
               ignore_attr = TRUE)
})
