truth <- factor(c("a", "a", "b", "b", "b"), levels = c("a", "b"))
pred  <- factor(c("a", "b", "b", "b", "a"), levels = c("a", "b"))

test_that("built-in metrics agree with hand calculations", {
  expect_equal(metric_accuracy()$fun(truth, pred, NULL), 3 / 5)
  expect_equal(metric_error_rate()$fun(truth, pred, NULL), 2 / 5)
  # recall a = 1/2, recall b = 2/3
  expect_equal(metric_balanced_accuracy()$fun(truth, pred, NULL),
               mean(c(1 / 2, 2 / 3)))
  # F1 a: 2*1/(2+2) = 0.5 ; F1 b: 2*2/(3+3) = 2/3
  expect_equal(metric_macro_f1()$fun(truth, pred, NULL), mean(c(0.5, 2 / 3)))
})

test_that("metric direction is recorded", {
  expect_true(metric_accuracy()$higher_is_better)
  expect_false(metric_error_rate()$higher_is_better)
})

test_that("metric_custom validates its arguments", {
  expect_error(metric_custom("x", "n"), "must be a function")
  expect_error(metric_custom(function(...) 1, ""), "non-empty")
  expect_error(metric_custom(function(...) 1, "n", higher_is_better = NA),
               "TRUE or FALSE")
  expect_error(sp_check_metrics(list(1)), "metric_custom")
})

test_that("empty selection gives NA", {
  expect_true(is.na(sp_eval_metric(metric_accuracy(), truth, pred, NULL,
                                   integer(0))))
})

test_that("IAM matches a hand calculation", {
  # confusion matrix (rows = truth): [[8, 2], [1, 9]]
  tr <- factor(rep(c("a", "b"), each = 10))
  pr <- factor(c(rep(c("a", "b"), c(8, 2)), rep(c("a", "b"), c(1, 9))))
  # class a: (8 - max(2, 1)) / max(10, 9) = 0.6
  # class b: (9 - max(1, 2)) / max(10, 11) = 7 / 11
  expect_equal(metric_iam()$fun(tr, pr, NULL), mean(c(0.6, 7 / 11)))
  # perfect classifier: IAM = 1
  expect_equal(metric_iam()$fun(tr, tr, NULL), 1)
})

test_that("IAM is undefined when a class is absent from the selection", {
  tr <- factor(c("a", "a", "b"), levels = c("a", "b"))
  expect_true(is.na(metric_iam()$fun(tr[1:2], tr[1:2], NULL)))
})
