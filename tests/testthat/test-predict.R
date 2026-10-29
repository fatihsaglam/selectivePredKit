cal <- sim_probs(800, seed = 21)
new <- sim_probs(300, seed = 22)
sel <- selective_fit(cal$p, cal$y, target = target_coverage(0.7))

test_that("predict applies the stored threshold to new probabilities", {
  out <- predict(sel, new$p)
  expect_s3_class(out, "data.frame")
  expect_named(out, c(".pred_class", ".score", ".accept", ".pred_selective"))
  expect_equal(out$.accept, out$.score <= sel$threshold[[1]])
  expect_true(all(is.na(out$.pred_selective[!out$.accept])))
  expect_equal(as.character(out$.pred_selective[out$.accept]),
               as.character(out$.pred_class[out$.accept]))
  expect_equal(levels(out$.pred_class), sel$classes)
})

test_that("coverage on new data is close to calibration coverage", {
  out <- predict(sel, new$p)
  expect_lt(abs(mean(out$.accept) - sel$coverage), 0.1)
})

test_that("type argument selects the output", {
  expect_type(predict(sel, new$p, type = "accept"), "logical")
  cl <- predict(sel, new$p, type = "class")
  expect_s3_class(cl, "factor")
  expect_equal(length(cl), nrow(new$p))
})

test_that("new data are matched to classes by name, including .pred_ columns", {
  shuffled <- new$p[, c("c", "a", "b")]
  expect_equal(predict(sel, shuffled), predict(sel, new$p))
  df <- as.data.frame(new$p)
  names(df) <- paste0(".pred_", names(df))
  expect_equal(predict(sel, df), predict(sel, new$p))
})

test_that("a different set of classes is rejected", {
  wrong <- new$p[, 1:2]
  wrong <- wrong / rowSums(wrong)
  expect_error(predict(sel, wrong), "columns but there are")
})

test_that("a model can supply the probabilities", {
  toy <- list(p = new$p)
  pf <- function(model, new_data) model$p[new_data, , drop = FALSE]
  out <- predict(sel, 1:50, model = toy, prob_fun = pf)
  expect_equal(out, predict(sel, new$p[1:50, ]), ignore_attr = TRUE)

  # default path uses stats::predict(model, new_data, type = "prob")
  predict.toy_model <- function(object, newdata, type = "prob", ...) {
    object$p[newdata, , drop = FALSE]
  }
  registerS3method("predict", "toy_model", predict.toy_model)
  toy2 <- structure(list(p = new$p), class = "toy_model")
  out2 <- predict(sel, 1:50, model = toy2)
  expect_equal(out2, out, ignore_attr = TRUE)

  expect_error(predict(sel, 1:5, model = list(), prob_fun = function(m, d) 1),
               "probability matrix")
})

test_that("predict works with posterior draws", {
  arr <- sim_draws(300, m = 20, seed = 4)
  P <- sp_prepare(arr)$P
  set.seed(10)
  y <- factor(apply(P, 1, function(r) sample(colnames(P), 1, prob = r)),
              levels = colnames(P))
  sel_mi <- selective_fit(arr[1:200, , ], y[1:200], score = uncert_mi(),
                   target = target_coverage(0.7))
  out <- predict(sel_mi, arr[201:300, , ])
  expect_equal(nrow(out), 100L)
  expect_true(any(out$.accept) && !all(out$.accept))
  expect_error(predict(sel_mi, P[201:300, ]), "posterior draws")
})

test_that("decisions on new data can be evaluated with selective_measures", {
  out <- predict(sel, new$p)
  m <- selective_measures(out$.accept, out$.pred_class, new$y)
  expect_equal(m$coverage, mean(out$.accept))
  overall <- mean(new$y != out$.pred_class)
  expect_lt(m$risk, overall)
})
