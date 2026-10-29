#' Performance metrics
#'
#' Metrics are small objects that carry a function and its direction. They are
#' used as targets in [target_metric()] and as extra columns in
#' [selective_curve()]. Every metric function has the signature
#' `f(truth, pred, probs)` and returns a single number:
#'
#' * `truth`: factor of true classes,
#' * `pred`: factor of predicted classes (same levels),
#' * `probs`: matrix of class probabilities, one row per observation. Metrics
#'   that do not need probabilities simply ignore it.
#'
#' A metric is only ever evaluated on the **accepted** observations, so the
#' value is a *selective* metric (for the error rate, the selective risk).
#'
#' Built-in metrics: `metric_accuracy()`, `metric_error_rate()`,
#' `metric_balanced_accuracy()`, `metric_macro_f1()` and `metric_iam()`. The last one is
#' the Imbalance Accuracy Metric used in Sağlam et al. (2026); it is `NA` when
#' a class has been rejected completely (see [selective_measures()]), and a target
#' that depends on an `NA` value counts as not satisfied.
#'
#' @param fun A function with signature `function(truth, pred, probs)`.
#' @param name Label used for the column in curves and in printed output.
#' @param higher_is_better Whether larger values are better (`TRUE` for
#'   accuracy, `FALSE` for an error rate).
#'
#' @return An object of class `sp_metric`.
#' @examples
#' truth <- factor(c("a", "a", "b", "b"))
#' pred  <- factor(c("a", "b", "b", "b"), levels = levels(truth))
#' metric_accuracy()$fun(truth, pred, NULL)
#'
#' # Custom metric: share of class "b" that was predicted correctly
#' recall_b <- metric_custom(
#'   function(truth, pred, probs) mean(pred[truth == "b"] == "b"),
#'   name = "recall_b", higher_is_better = TRUE
#' )
#' @name metric_custom
NULL

#' @rdname metric_custom
#' @export
metric_custom <- function(fun, name, higher_is_better = TRUE) {
  if (!is.function(fun)) {
    stop("`fun` must be a function.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.logical(higher_is_better) || length(higher_is_better) != 1L ||
      is.na(higher_is_better)) {
    stop("`higher_is_better` must be TRUE or FALSE.", call. = FALSE)
  }
  structure(
    list(fun = fun, name = name, higher_is_better = higher_is_better),
    class = "sp_metric"
  )
}

#' @rdname metric_custom
#' @export
metric_accuracy <- function() {
  metric_custom(function(truth, pred, probs) mean(truth == pred),
            name = "accuracy", higher_is_better = TRUE)
}

#' @rdname metric_custom
#' @export
metric_error_rate <- function() {
  metric_custom(function(truth, pred, probs) mean(truth != pred),
            name = "error_rate", higher_is_better = FALSE)
}

#' @rdname metric_custom
#' @export
metric_balanced_accuracy <- function() {
  metric_custom(function(truth, pred, probs) {
    present <- levels(truth)[tabulate(truth, nlevels(truth)) > 0L]
    mean(vapply(present, function(k) mean(pred[truth == k] == k), numeric(1)))
  }, name = "balanced_accuracy", higher_is_better = TRUE)
}

#' @rdname metric_custom
#' @export
metric_macro_f1 <- function() {
  metric_custom(function(truth, pred, probs) {
    lv <- levels(truth)
    f1 <- vapply(lv, function(k) {
      tp <- sum(truth == k & pred == k)
      den <- sum(truth == k) + sum(pred == k)
      if (den == 0L) NA_real_ else 2 * tp / den
    }, numeric(1))
    mean(f1, na.rm = TRUE)
  }, name = "macro_f1", higher_is_better = TRUE)
}

#' @rdname metric_custom
#' @export
metric_iam <- function() {
  metric_custom(function(truth, pred, probs) {
    cm <- unclass(table(truth, pred))
    sp_iam_from_cm(matrix(cm, nrow = nrow(cm)))
  }, name = "iam", higher_is_better = TRUE)
}

#' @export
print.sp_metric <- function(x, ...) {
  cat("<sp_metric> ", x$name,
      if (x$higher_is_better) " (higher is better)" else " (lower is better)",
      "\n", sep = "")
  invisible(x)
}

# Evaluate a metric on the rows `idx`; NA when nothing is selected.
sp_eval_metric <- function(metric, truth, pred, probs, idx) {
  if (length(idx) == 0L) {
    return(NA_real_)
  }
  val <- metric$fun(truth[idx], pred[idx], probs[idx, , drop = FALSE])
  if (!is.numeric(val) || length(val) != 1L) {
    stop(sprintf("Metric '%s' must return a single number.", metric$name),
         call. = FALSE)
  }
  as.numeric(val)
}

sp_check_metrics <- function(metrics) {
  if (inherits(metrics, "sp_metric")) {
    metrics <- list(metrics)
  }
  if (!is.list(metrics) ||
      !all(vapply(metrics, inherits, logical(1), what = "sp_metric"))) {
    stop("`metrics` must be a metric (see metric_custom()) or a list of them.",
         call. = FALSE)
  }
  metrics
}
