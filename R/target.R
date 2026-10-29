#' Targets: how the acceptance threshold is chosen
#'
#' A target tells [selective_fit()] how to turn the calibration scores into
#' threshold(s). Observations with `score <= threshold` are accepted.
#'
#' * `target_threshold()`: use a given score threshold directly. No labels
#'   are needed. Only for the `"global"` strategy.
#' * `target_coverage()`: choose the smallest threshold that accepts at
#'   least the requested share of observations. No labels are needed. With
#'   the `"global"` strategy the share refers to all observations; with the
#'   `"class_conditional"` strategy it applies to each predicted class
#'   separately (coverage matching, Algorithm 1 of Sağlam et al., 2026). With
#'   tied scores the realised coverage can be slightly larger.
#' * `target_level()`: the error level `alpha` of the `"label"` strategy
#'   (Sadinle et al., 2019): each prediction set contains the true class with
#'   probability at least `1 - alpha`. Larger `alpha` gives larger coverage.
#'   Labels are required.
#' * `target_metric()`: choose the **largest coverage** level whose
#'   selective metric on the accepted observations still satisfies the
#'   condition. The condition is `metric >= value` for metrics where higher
#'   is better and `metric <= value` otherwise. Labels are required. The
#'   metric is not assumed to be monotone in the threshold, so a grid of
#'   levels is scanned (see `n_grid` in [selective_fit()]).
#'
#' @param threshold A single finite number.
#' @param coverage A single number in `(0, 1]`.
#' @param alpha A single number in `[0, 1]`.
#' @param metric An [metric_custom()] object, for example [metric_accuracy()].
#' @param value The value the metric has to reach on the accepted
#'   observations.
#'
#' @return An object of class `sp_target`.
#' @examples
#' target_threshold(0.3)
#' target_coverage(0.8)
#' target_level(0.1)
#' target_metric(metric_accuracy(), value = 0.95)
#' target_metric(metric_error_rate(), value = 0.05)
#' @name target_threshold
NULL

#' @rdname target_threshold
#' @export
target_threshold <- function(threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold)) {
    stop("`threshold` must be a single finite number.", call. = FALSE)
  }
  structure(
    list(type = "threshold", threshold = as.numeric(threshold),
         label = sprintf("score <= %s", format(threshold))),
    class = "sp_target"
  )
}

#' @rdname target_threshold
#' @export
target_coverage <- function(coverage) {
  if (!is.numeric(coverage) || length(coverage) != 1L || is.na(coverage) ||
      coverage <= 0 || coverage > 1) {
    stop("`coverage` must be a single number in (0, 1].", call. = FALSE)
  }
  structure(
    list(type = "coverage", coverage = as.numeric(coverage),
         label = sprintf("coverage >= %s", format(coverage))),
    class = "sp_target"
  )
}

#' @rdname target_threshold
#' @export
target_level <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single number in [0, 1].", call. = FALSE)
  }
  structure(
    list(type = "level", alpha = as.numeric(alpha),
         label = sprintf("alpha = %s", format(alpha))),
    class = "sp_target"
  )
}

#' @rdname target_threshold
#' @export
target_metric <- function(metric, value) {
  if (!inherits(metric, "sp_metric")) {
    stop("`metric` must be created with metric_custom() or one of the built-in ",
         "sp_*() metrics, e.g. metric_accuracy().", call. = FALSE)
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop("`value` must be a single finite number.", call. = FALSE)
  }
  structure(
    list(type = "metric", metric = metric, value = as.numeric(value),
         label = sprintf("%s %s %s", metric$name,
                         if (metric$higher_is_better) ">=" else "<=",
                         format(value))),
    class = "sp_target"
  )
}

#' @export
print.sp_target <- function(x, ...) {
  cat("<sp_target> ", x$type, ": ", x$label, "\n", sep = "")
  invisible(x)
}
