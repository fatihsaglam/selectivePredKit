truth <- factor(rep(c("a", "b"), each = 10))
pred  <- factor(c(rep(c("a", "b"), c(8, 2)), rep(c("a", "b"), c(1, 9))))

test_that("selective_measures with everything accepted gives the overall numbers", {
  m <- selective_measures(rep(TRUE, 20), pred, truth)
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 1L)
  expect_equal(m$coverage, 1)
  expect_equal(m$n_accepted, 20)
  expect_equal(m$risk, 3 / 20)
  expect_equal(m$gen_risk, 3 / 20)
  expect_equal(m$accuracy, 17 / 20)
  expect_equal(m$iam, mean(c(0.6, 7 / 11)))
  expect_equal(m$coverage_a, 1)
  expect_equal(m$coverage_b, 1)
})

test_that("generalized risk is the joint share of accepted errors", {
  # accept the first 15 rows: errors among them are rows 9, 10 (a -> b)
  # and row 11 (b -> a)
  accept <- rep(c(TRUE, FALSE), c(15, 5))
  m <- selective_measures(accept, pred, truth)
  expect_equal(m$coverage, 15 / 20)
  expect_equal(m$risk, 3 / 15)
  expect_equal(m$gen_risk, 3 / 20)
  expect_equal(m$coverage_a, 1)
  expect_equal(m$coverage_b, 5 / 10)
})

test_that("class-specific coverage uses the true class", {
  accept <- c(rep(TRUE, 10), rep(FALSE, 10))
  m <- selective_measures(accept, pred, truth)
  expect_equal(m$coverage_a, 1)
  expect_equal(m$coverage_b, 0)
  # class b completely rejected but predicted b among accepted -> IAM defined
  accept2 <- c(rep(TRUE, 8), rep(FALSE, 12))
  m2 <- selective_measures(accept2, pred, truth)
  expect_true(is.na(m2$iam))
})

test_that("nothing accepted gives NA risk and zero coverage", {
  m <- selective_measures(rep(FALSE, 20), pred, truth)
  expect_equal(m$coverage, 0)
  expect_true(is.na(m$risk))
  expect_equal(m$gen_risk, 0)
  expect_true(is.na(m$iam))
})

test_that("selective_measures validates its input", {
  expect_error(selective_measures(1:3, pred, truth), "logical")
  expect_error(selective_measures(rep(TRUE, 3), pred, truth), "same length")
})
