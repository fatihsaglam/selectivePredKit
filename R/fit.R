#' Fit a selective-prediction rule
#'
#' Learns the acceptance threshold(s) on a calibration set of class
#' probabilities. The classifier that produced the probabilities is not part
#' of the object, so any model can be used and nothing is retrained. Apply
#' the result to new data with `predict()`.
#'
#' Use a calibration set that the classifier did **not** train on. Choosing
#' the threshold and judging it on the same data gives optimistic coverage
#' and risk estimates.
#'
#' @section Strategies:
#' * `"global"` (the classical approach): one threshold on the score for all
#'   observations. Under class imbalance it tends to reject the minority
#'   classes much more often than the majority class.
#' * `"class_conditional"` (class-conditional coverage matching, Algorithm 1
#'   of Sağlam et al., 2026): one threshold per **predicted** class, the
#'   `alpha`-quantile of the calibration scores of the observations predicted
#'   as that class. Every class is therefore accepted at about the same rate.
#'   A new observation is accepted when its score is at most the threshold of
#'   its predicted class. This needs enough calibration observations in each
#'   predicted class.
#'
#'
#' * `"label"` (LABEL, Sadinle, Lei and Wasserman, 2019, split-conformal
#'   version): a prediction set `{k: p_k(x) >= t_k}` with one threshold per
#'   **true** class, learned from the probabilities the calibration data give
#'   to their own class. A set with exactly one class is accepted and
#'   predicted as that class; an empty set is accepted and predicted as the
#'   arg max; a set with several classes is rejected. The `score` is not
#'   used, the calibration `truth` is required and the level `alpha` is the
#'   error level of the sets (see [target_level()]); a coverage or metric
#'   target is met by searching over `alpha`. The selector is not
#'   monotone in `alpha` in general, so the grid point whose calibration
#'   coverage is closest to the request is used.
#'
#' @param probs Calibration class probabilities: a numeric matrix or data
#'   frame with one column per class. Columns named `.pred_<class>` (as
#'   returned by tidymodels' `predict(type = "prob")`) are recognised and the
#'   prefix is dropped. Rows must sum to 1 (tolerance 0.01). For the scores
#'   [uncert_var()] and [uncert_mi()] give posterior draws instead, an
#'   array with dimensions observations x classes x draws.
#' @param truth True classes, one per row. Optional for
#'   [target_threshold()] and [target_coverage()], required for
#'   [target_metric()]. When supplied, the calibration curve and the
#'   selective measures at the chosen threshold are stored.
#' @param score An uncertainty score, see [uncert_custom()]. Lower means more
#'   confident. Default [uncert_margin()].
#' @param target How the threshold is chosen, see [target_threshold()],
#'   [target_coverage()] and [target_metric()].
#' @param strategy `"global"` (default) or `"class_conditional"`, see
#'   Strategies.
#' @param n_grid Number of coverage levels in `[0, 1]` scanned for the metric
#'   target and for the stored curve.
#'
#' @return An object of class `sp_selector`, a list with elements `score`,
#'   `target`, `strategy`, `threshold` (named numeric vector with one
#'   threshold per predicted class; all equal for the global strategy),
#'   `level` (the coverage level `alpha` that was used, `NA` for a threshold
#'   target), `classes`, `n`, `coverage` (realised on the calibration data),
#'   `performance` (named numeric vector of [selective_measures()], only with
#'   `truth`), `curve` (only with `truth`), `cal` (calibration scores,
#'   predictions and truth) and `call`.
#' @references
#' Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
#' classification under imbalance in multiclass settings: A novel metric for
#' bias-aware risk-coverage evaluation. *Journal of Biomedical Informatics*,
#' 181, 105084. \doi{10.1016/j.jbi.2026.105084}
#' @seealso [predict.sp_selector()], [selective_curve()], [auc_all()]
#' @examples
#' set.seed(42)
#' sim <- function(n) {
#'   logits <- matrix(rnorm(n * 3, sd = 2.5), ncol = 3)
#'   p <- exp(logits) / rowSums(exp(logits))
#'   colnames(p) <- c("a", "b", "c")
#'   y <- factor(apply(p, 1, function(r) sample(colnames(p), 1, prob = r)),
#'               levels = colnames(p))
#'   list(p = p, y = y)
#' }
#' cal <- sim(1000)
#' new <- sim(500)
#'
#' # Keep the most confident 80 % of observations
#' sel <- selective_fit(cal$p, cal$y, target = target_coverage(0.8))
#' sel
#'
#' # Largest coverage that still reaches 90 % accuracy on accepted rows
#' sel2 <- selective_fit(cal$p, cal$y, target = target_metric(metric_accuracy(), 0.9))
#' sel2
#'
#' # One threshold per predicted class (equal coverage for every class)
#' sel3 <- selective_fit(cal$p, cal$y, strategy = "class_conditional")
#' sel3
#'
#' # Apply to new data: predict or abstain (NA)
#' head(predict(sel2, new$p))
#' @export
selective_fit <- function(probs, truth = NULL, score = uncert_margin(),
                   target = target_coverage(0.8),
                   strategy = c("global", "class_conditional", "label"),
                   n_grid = 101L) {
  call <- match.call()
  strategy <- match.arg(strategy)
  if (!inherits(score, "sp_score")) {
    stop("`score` must be created with uncert_custom() or one of uncert_*().",
         call. = FALSE)
  }
  if (!inherits(target, "sp_target")) {
    stop("`target` must be created with one of target_*().", call. = FALSE)
  }
  if (!is.numeric(n_grid) || length(n_grid) != 1L || n_grid < 2) {
    stop("`n_grid` must be a single number >= 2.", call. = FALSE)
  }
  if (target$type == "metric" && is.null(truth)) {
    stop("`truth` is required when the target is target_metric().",
         call. = FALSE)
  }
  if (strategy == "label" && is.null(truth)) {
    stop("`truth` is required for the \"label\" strategy.", call. = FALSE)
  }
  if (target$type == "level" && strategy != "label") {
    stop("target_level() is only for strategy = \"label\".", call. = FALSE)
  }
  if (strategy == "label" && target$type == "threshold") {
    stop("target_threshold() does not apply to the \"label\" strategy; ",
         "use target_level(), target_coverage() or target_metric().",
         call. = FALSE)
  }
  if (target$type == "threshold" && strategy != "global") {
    stop("target_threshold() is a single global threshold; use ",
         "target_coverage() with strategy = \"class_conditional\".",
         call. = FALSE)
  }

  classes_in <- if (is.factor(truth)) levels(truth) else NULL
  prep <- sp_prepare(probs, classes_in)
  P <- prep$P
  classes <- colnames(P)
  K <- length(classes)
  n <- nrow(P)
  y <- if (is.null(truth)) NULL else sp_as_truth(truth, classes, n)
  s <- sp_compute_score(score, P, prep$draws)
  pred <- sp_pred_class(P)
  thr_fun <- sp_make_thr_fun(s, pred, classes, strategy, cal_truth = y,
                             cal_P = P)
  alphas <- seq(0, 1, length.out = n_grid)

  level <- NA_real_
  threshold <- switch(
    target$type,
    threshold = stats::setNames(rep(target$threshold, K), classes),
    level = {
      level <- target$alpha
      thr_fun(level)[1L, ]
    },
    coverage = {
      if (strategy == "label") {
        sw <- sp_sweep(thr_fun, alphas, strategy, s, pred, y, P, classes)
        level <- alphas[which.min(abs(sw$coverage - target$coverage))]
      } else {
        level <- target$coverage
      }
      thr_fun(level)[1L, ]
    },
    metric = {
      m <- target$metric
      sw <- sp_sweep(thr_fun, alphas, strategy, s, pred, y, P, classes,
                     list(m))
      v <- sw[[ncol(sw)]]
      ok <- if (m$higher_is_better) {
        v >= target$value - 1e-12
      } else {
        v <= target$value + 1e-12
      }
      ok[is.na(ok)] <- FALSE
      if (any(ok)) {
        level <- max(alphas[ok])
        thr_fun(level)[1L, ]
      } else {
        warning(sprintf(
          "No threshold reaches %s; the selector will abstain on everything.",
          target$label
        ), call. = FALSE)
        stats::setNames(rep(-Inf, K), classes)
      }
    }
  )

  dec <- sp_decide(strategy, threshold, s, as.integer(pred), P)
  accept <- dec$accept
  perf <- NULL
  curve <- NULL
  if (!is.null(y)) {
    yi <- as.integer(y)
    pm <- sp_point_measures(accept, yi, dec$pred, K,
                            tabulate(yi, K), classes)
    perf <- pm[setdiff(names(pm), "n_accepted")]
    curve <- sp_as_curve(
      sp_sweep(thr_fun, alphas, strategy, s, pred, y, P, classes),
      score, strategy, classes
    )
  }

  structure(
    list(score = score, target = target, strategy = strategy,
         threshold = threshold, level = level, classes = classes, n = n,
         coverage = mean(accept), performance = perf, curve = curve,
         cal = list(score = s, pred = pred, truth = y, probs = P),
         call = call),
    class = "sp_selector"
  )
}

#' @export
print.sp_selector <- function(x, ...) {
  cat("<sp_selector>\n")
  if (x$strategy != "label") {
    cat("  score      : ", x$score$name, " (lower = more confident)\n",
        sep = "")
  }
  cat("  strategy   : ", x$strategy, "\n", sep = "")
  cat("  target     : ", x$target$label, "\n", sep = "")
  if (x$strategy == "label") {
    cat("  thresholds : ",
        paste(names(x$threshold), format(x$threshold, digits = 3), sep = "=",
              collapse = ", "),
        "\n               (class k is in the set when p_k >= t_k; accept when the set has at most one class)\n",
        sep = "")
  } else if (x$strategy == "global") {
    cat("  threshold  : ", format(x$threshold[[1L]], digits = 4),
        "  (accept when score <= threshold)\n", sep = "")
  } else {
    cat("  thresholds : ",
        paste(names(x$threshold), format(x$threshold, digits = 3), sep = "=",
              collapse = ", "),
        "\n               (accept when score <= threshold of the predicted class)\n",
        sep = "")
  }
  cat("  classes    : ", paste(x$classes, collapse = ", "), "\n", sep = "")
  cat("  calibration: n = ", x$n, ", coverage = ",
      format(round(x$coverage, 4)), "\n", sep = "")
  if (!is.null(x$performance)) {
    keep <- intersect(c("accuracy", "risk", "gen_risk", "iam"),
                      names(x$performance))
    cat("  on accepted: ",
        paste(keep, format(round(x$performance[keep], 4)), sep = " = ",
              collapse = ", "),
        "\n", sep = "")
  }
  invisible(x)
}

#' @param x A fitted `sp_selector`.
#' @param y Name of the column to plot, see [selective_curve()]. Defaults to
#'   `"risk"`.
#' @param ... Further arguments passed to [graphics::plot()].
#' @rdname selective_fit
#' @export
plot.sp_selector <- function(x, y = "risk", ...) {
  if (is.null(x$curve)) {
    stop("The selector was fitted without `truth`, so there is no curve.",
         call. = FALSE)
  }
  plot.sp_curve(x$curve, y = y, ...)
  graphics::abline(v = x$coverage, lty = 2)
  invisible(x)
}
