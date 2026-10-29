# Thresholds and sweeps --------------------------------------------------------

# Returns a function `alphas -> matrix (length(alphas) x K)` of thresholds, one
# column per predicted class.
#
# * "global": the alpha-quantile of all calibration scores, repeated for every
#   class.
# * "class_conditional": for each predicted class k the alpha-quantile of the
#   calibration scores of the observations predicted as k (Algorithm 1 of
#   Saglam et al., 2026). A class that is never predicted in the calibration
#   data falls back to the global quantile.
sp_make_thr_fun <- function(cal_s, cal_pred, classes, strategy, warn = TRUE,
                            cal_truth = NULL, cal_P = NULL) {
  K <- length(classes)
  if (strategy == "label") {
    if (is.null(cal_truth)) {
      stop("`truth` is required for the \"label\" strategy.", call. = FALSE)
    }
    return(sp_label_thr_fun(cal_P, cal_truth, classes))
  }
  glob <- function(a) {
    stats::quantile(cal_s, probs = a, type = 7, names = FALSE)
  }
  if (strategy == "global") {
    return(function(alphas) {
      matrix(rep(glob(alphas), K), nrow = length(alphas),
             dimnames = list(NULL, classes))
    })
  }
  by <- split(cal_s, cal_pred)
  empty <- lengths(by) == 0L
  if (warn && any(empty)) {
    warning(sprintf(
      "Class(es) never predicted in the calibration data: %s. Their thresholds fall back to the global quantile.",
      paste(classes[empty], collapse = ", ")
    ), call. = FALSE)
  }
  function(alphas) {
    tm <- matrix(NA_real_, length(alphas), K, dimnames = list(NULL, classes))
    for (k in seq_len(K)) {
      tm[, k] <- if (empty[k]) {
        glob(alphas)
      } else {
        stats::quantile(by[[k]], probs = alphas, type = 7, names = FALSE)
      }
    }
    tm
  }
}

# LABEL (Sadinle, Lei & Wasserman, 2019), split-conformal version. For each
# true class k the threshold t_k is an order statistic of the calibration
# probabilities of class k among the observations whose true class is k:
# the sorted value at index ceiling(alpha * (n_k + 1) - 1), clipped to the
# range of the scores (0 for a class absent from the calibration data).
# Returns `alphas -> matrix (length(alphas) x K)` with one column per class.
sp_label_thr_fun <- function(P, truth, classes) {
  K <- length(classes)
  yi <- as.integer(truth)
  srt <- lapply(seq_len(K), function(k) sort(P[yi == k, k]))
  function(alphas) {
    tm <- matrix(0, length(alphas), K, dimnames = list(NULL, classes))
    for (k in seq_len(K)) {
      nk <- length(srt[[k]])
      if (nk == 0L) next
      idx <- ceiling(alphas * (nk + 1) - 1)
      idx <- pmin(pmax(idx, 1L), nk)
      tm[, k] <- srt[[k]][idx]
    }
    tm
  }
}

# One decision rule for all strategies. `thr` is one row of thresholds (one
# per class). Returns the acceptance indicator, the prediction as integer
# (for LABEL: the single class of the prediction set, or the arg max when the
# set is empty) and the size of the LABEL set (NA otherwise).
sp_decide <- function(strategy, thr, s, pi, P) {
  if (strategy != "label") {
    return(list(accept = s <= thr[pi], pred = pi, set_size = NULL))
  }
  above <- sweep(P, 2L, thr, `>=`)
  size <- rowSums(above)
  single <- max.col(above * 1, ties.method = "first")
  list(accept = size <= 1L,
       pred = ifelse(size == 1L, single, pi),
       set_size = size)
}

# Evaluate the selector at every level in `alphas`. The thresholds come from
# the calibration scores (via `thr_fun`); the observations evaluated are
# `s`, `pred`, `truth`, `P`. Returns a data frame with columns alpha,
# [threshold], coverage, n_accepted, and, when `truth` is given, risk,
# gen_risk, accuracy, iam, coverage_<class> and one column per extra metric.
sp_sweep <- function(thr_fun, alphas, strategy, s, pred, truth, P, classes,
                     metrics = list()) {
  K <- length(classes)
  n <- length(s)
  tm <- thr_fun(alphas)
  pi <- as.integer(pred)
  na <- length(alphas)
  has_truth <- !is.null(truth)
  if (length(metrics) && !has_truth) {
    stop("`truth` is required to compute metrics.", call. = FALSE)
  }

  if (has_truth) {
    yi <- as.integer(truth)
    n_true <- tabulate(yi, K)
    cols <- c("coverage", "n_accepted", "risk", "gen_risk", "accuracy",
              "iam", paste0("coverage_", classes))
  } else {
    cols <- c("coverage", "n_accepted")
  }
  mat <- matrix(NA_real_, na, length(cols), dimnames = list(NULL, cols))
  ext <- matrix(NA_real_, na, length(metrics))
  for (i in seq_len(na)) {
    d <- sp_decide(strategy, tm[i, ], s, pi, P)
    accept <- d$accept
    if (has_truth) {
      mat[i, ] <- sp_point_measures(accept, yi, d$pred, K, n_true, classes)
      if (length(metrics)) {
        pf <- factor(classes[d$pred], levels = classes)
        for (j in seq_along(metrics)) {
          ext[i, j] <- sp_eval_metric(metrics[[j]], truth, pf, P,
                                      which(accept))
        }
      }
    } else {
      mat[i, 1L] <- mean(accept)
      mat[i, 2L] <- sum(accept)
    }
  }
  out <- data.frame(alpha = alphas, mat, check.names = FALSE, row.names = NULL)
  if (strategy == "global") {
    out <- data.frame(alpha = alphas, threshold = tm[, 1L], mat,
                      check.names = FALSE, row.names = NULL)
  }
  if (length(metrics)) {
    nm <- vapply(metrics, function(m) m$name, character(1))
    nm <- make.unique(c(names(out), nm))[-seq_along(out)]
    colnames(ext) <- nm
    out <- cbind(out, as.data.frame(ext, check.names = FALSE))
  }
  out
}

sp_as_curve <- function(df, score, strategy, classes) {
  attr(df, "score") <- score$name
  attr(df, "strategy") <- strategy
  attr(df, "classes") <- classes
  class(df) <- c("sp_curve", "data.frame")
  df
}

#' Risk-coverage curves and the measures along them
#'
#' Sweeps the coverage level `alpha` from 0 to 1. At each level the
#' threshold(s) are taken from the **calibration** scores (the alpha-quantile
#' of all scores for the `"global"` strategy, the alpha-quantile within each
#' predicted class for `"class_conditional"`) and applied to the evaluation
#' data. For every level the curve records the measures of [selective_measures()]:
#' total coverage, class-specific coverage, selective risk, generalized risk,
#' accuracy and IAM. The areas under these curves are computed by
#' [auc_all()] and its relatives.
#'
#' Two ways to call it:
#'
#' * `selective_curve(sel)` or `selective_curve(sel, new_probs, new_truth)` with a fitted
#'   [sp_selector][selective_fit()]: thresholds come from the calibration data stored
#'   in the selector and are applied to the calibration data (when `probs` is
#'   omitted; requires that the selector was fitted with `truth`) or to new
#'   data. This is the intended way to evaluate on a test set.
#' * `selective_curve(probs, truth)`: shortcut that fits a selector on the same data
#'   and evaluates it there.
#'
#' @param object A fitted `sp_selector`, or, for the default method, class
#'   probabilities (matrix, data frame or posterior-draws array, see
#'   [selective_fit()]).
#' @param probs Evaluation probabilities for the `sp_selector` method. If
#'   `NULL`, the calibration data stored in the selector are used.
#' @param truth True classes. For the default method: the calibration truth.
#'   For the `sp_selector` method: the truth of `probs`.
#' @param score,strategy Only for the default method, see [selective_fit()].
#' @param n_grid Number of coverage levels in `[0, 1]`.
#' @param metrics Optional [metric_custom()] objects (or a list of them) to be
#'   evaluated on the accepted observations at every level and added as
#'   columns.
#' @param ... Not used.
#'
#' @return A data frame of class `sp_curve` with columns `alpha`,
#'   (`threshold` for the global strategy), `coverage`, `n_accepted`, `risk`,
#'   `gen_risk`, `accuracy`, `iam`, `coverage_<class>` and one column per
#'   extra metric.
#' @references
#' Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
#' classification under imbalance in multiclass settings: A novel metric for
#' bias-aware risk-coverage evaluation. *Journal of Biomedical Informatics*,
#' 181, 105084. \doi{10.1016/j.jbi.2026.105084}
#' @seealso [auc_all()], [selective_measures()], [selective_fit()]
#' @examples
#' set.seed(1)
#' sim <- function(n) {
#'   y <- factor(sample(c("a", "b", "c"), n, TRUE, prob = c(0.8, 0.15, 0.05)))
#'   logits <- matrix(rnorm(n * 3), ncol = 3)
#'   logits[cbind(seq_len(n), as.integer(y))] <-
#'     logits[cbind(seq_len(n), as.integer(y))] + c(3, 1.5, 1)[as.integer(y)]
#'   p <- exp(logits) / rowSums(exp(logits))
#'   colnames(p) <- levels(y)
#'   list(p = p, y = y)
#' }
#' cal <- sim(1000)
#' test <- sim(500)
#'
#' sel <- selective_fit(cal$p, cal$y, strategy = "class_conditional")
#' cv <- selective_curve(sel, test$p, test$y)
#' head(cv)
#' auc_all(cv)
#' plot(cv, "risk")
#' plot(cv, "class_coverage")
#' @export
selective_curve <- function(object, ...) {
  UseMethod("selective_curve")
}

#' @rdname selective_curve
#' @export
selective_curve.sp_selector <- function(object, probs = NULL, truth = NULL,
                                 n_grid = 101L, metrics = list(), ...) {
  metrics <- sp_check_metrics(metrics)
  classes <- object$classes
  thr_fun <- sp_make_thr_fun(object$cal$score, object$cal$pred, classes,
                             object$strategy, warn = FALSE,
                             cal_truth = object$cal$truth,
                             cal_P = object$cal$probs)
  if (is.null(probs)) {
    if (is.null(object$cal$truth)) {
      stop("The selector was fitted without `truth`. Supply `probs` and ",
           "`truth` to evaluate it.", call. = FALSE)
    }
    s <- object$cal$score
    pred <- object$cal$pred
    y <- object$cal$truth
    P <- object$cal$probs
  } else {
    prep <- sp_prepare(probs, classes, "probs")
    P <- prep$P
    s <- sp_compute_score(object$score, P, prep$draws)
    pred <- sp_pred_class(P)
    if (is.null(truth)) {
      stop("`truth` is required when `probs` is supplied.", call. = FALSE)
    }
    y <- sp_as_truth(truth, classes, nrow(P))
  }
  alphas <- seq(0, 1, length.out = n_grid)
  df <- sp_sweep(thr_fun, alphas, object$strategy, s, pred, y, P, classes,
                 metrics)
  sp_as_curve(df, object$score, object$strategy, classes)
}

#' @rdname selective_curve
#' @export
selective_curve.default <- function(object, truth, score = uncert_margin(),
                             strategy = c("global", "class_conditional", "label"),
                             n_grid = 101L, metrics = list(), ...) {
  sel <- selective_fit(object, truth, score = score,
                target = target_coverage(1),
                strategy = match.arg(strategy), n_grid = n_grid)
  selective_curve(sel, n_grid = n_grid, metrics = metrics)
}

#' @param x An `sp_curve`.
#' @param y Name of the column to plot against total coverage, for example
#'   `"risk"` (default), `"gen_risk"`, `"accuracy"` or `"iam"`. The special
#'   value `"class_coverage"` draws the coverage of every class against the
#'   total coverage together with the diagonal.
#' @param by_class Draw one curve per class. The measure `y` is then plotted
#'   against the **class-specific** coverage of each class (the curves whose
#'   areas are averaged by [ca_aurc()] and [ca_augrc()]). `TRUE` or
#'   `"overlay"` puts all classes in one plot, `"panels"` gives one panel per
#'   class. Not used with `y = "class_coverage"`.
#' @rdname selective_curve
#' @export
plot.sp_curve <- function(x, y = "risk", by_class = FALSE, ...) {
  if (identical(y, "class_coverage")) {
    cols <- grep("^coverage_", names(x), value = TRUE)
    if (!length(cols)) {
      stop("The curve has no class coverage columns (computed without ",
           "truth).", call. = FALSE)
    }
    mat <- as.matrix(x[, cols, drop = FALSE])
    graphics::matplot(x$coverage, mat, type = "l", lty = 1,
                      col = seq_along(cols), xlab = "Total coverage",
                      ylab = "Class coverage", xlim = c(0, 1),
                      ylim = c(0, 1),
                      main = paste0("Class coverage (", attr(x, "score"), ", ",
                                    attr(x, "strategy"), ")"), ...)
    graphics::abline(0, 1, lty = 3)
    graphics::legend("bottomright", legend = sub("^coverage_", "", cols),
                     col = seq_along(cols), lty = 1, bty = "n", cex = 0.8)
    return(invisible(x))
  }
  if (!y %in% names(x) || y %in% c("alpha", "threshold", "n_accepted")) {
    stop("`y` must be one of: ",
         paste(setdiff(names(x), c("alpha", "threshold", "n_accepted")),
               collapse = ", "), ", class_coverage.", call. = FALSE)
  }
  if (!isFALSE(by_class)) {
    mode <- if (isTRUE(by_class)) "overlay" else by_class
    if (!mode %in% c("overlay", "panels")) {
      stop("`by_class` must be FALSE, TRUE, \"overlay\" or \"panels\".",
           call. = FALSE)
    }
    cols <- grep("^coverage_", names(x), value = TRUE)
    if (!length(cols)) {
      stop("The curve has no class coverage columns (computed without ",
           "truth).", call. = FALSE)
    }
    cls <- sub("^coverage_", "", cols)
    yy <- x[[y]]
    ttl <- paste0("Selective ", y, " (", attr(x, "score"), ", ",
                  attr(x, "strategy"), ")")
    dots <- list(...)
    if (mode == "overlay") {
      args <- list(x = x[[cols[1L]]], y = yy, type = "n", xlim = c(0, 1),
                   ylim = range(yy, na.rm = TRUE),
                   xlab = "Class coverage", ylab = y, main = ttl)
      args[names(dots)] <- dots
      do.call(graphics::plot, args)
      for (i in seq_along(cols)) {
        graphics::lines(x[[cols[i]]], yy, col = i, lwd = 1.5)
      }
      graphics::legend("topright", legend = cls, col = seq_along(cols),
                       lty = 1, bty = "n", cex = 0.8)
    } else {
      op <- graphics::par(mfrow = grDevices::n2mfrow(length(cols)))
      on.exit(graphics::par(op), add = TRUE)
      for (i in seq_along(cols)) {
        args <- list(x = x[[cols[i]]], y = yy, type = "l", xlim = c(0, 1),
                     col = i, xlab = paste0("Coverage of class ", cls[i]),
                     ylab = y, main = cls[i])
        args[names(dots)] <- dots
        do.call(graphics::plot, args)
      }
    }
    return(invisible(x))
  }
  args <- list(x = x$coverage, y = x[[y]], type = "l",
               xlab = "Total coverage", ylab = y,
               main = paste0("Selective ", y, " (", attr(x, "score"), ", ",
                             attr(x, "strategy"), ")"))
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(graphics::plot, args)
  invisible(x)
}

# Areas under the curves -------------------------------------------------------

# Trapezoidal area of y over x, summing only segments whose two end points are
# both defined (a segment with an undefined end contributes 0, so the area of
# a curve that is undefined over part of the coverage range counts only the
# defined part). `left` says how the region between coverage 0 and the first
# defined point is treated:
#   "constant": y is held at its first defined value (risk, accuracy),
#   "zero": the curve starts at (0, 0) (generalized risk),
#   "none": not counted (IAM).
sp_area <- function(x, y, left = c("constant", "zero", "none")) {
  left <- match.arg(left)
  ok <- is.finite(x) & is.finite(y)
  n <- length(x)
  total <- 0
  if (n >= 2L) {
    both <- ok[-n] & ok[-1L]
    if (any(both)) {
      i <- which(both)
      total <- sum((x[i + 1L] - x[i]) * (y[i] + y[i + 1L]) / 2)
    }
  }
  if (any(ok)) {
    f <- which(ok)[1L]
    total <- total + switch(
      left,
      constant = x[f] * y[f],
      zero = x[f] * y[f] / 2,
      none = 0
    )
  } else {
    return(NA_real_)
  }
  total
}

sp_as_curve_arg <- function(x, need = "risk") {
  if (inherits(x, "sp_selector")) {
    x <- x$curve
    if (is.null(x)) {
      stop("The selector was fitted without `truth`; use ",
           "selective_curve(sel, probs, truth) to evaluate it.", call. = FALSE)
    }
  }
  if (!inherits(x, "sp_curve") || !need %in% names(x)) {
    stop("`x` must be a curve from selective_curve() computed with `truth` (or a selector ",
         "fitted with `truth`).", call. = FALSE)
  }
  x
}

sp_class_area <- function(x, ycol, left) {
  cols <- grep("^coverage_", names(x), value = TRUE)
  a <- vapply(cols, function(cn) sp_area(x[[cn]], x[[ycol]], left),
              numeric(1))
  mean(a, na.rm = TRUE)
}

#' Areas under the selective-classification curves
#'
#' Scalar summaries of a risk-coverage evaluation, following Sağlam et al.
#' (2026). All take an [selective_curve()] (or a [sp_selector][selective_fit()] fitted with
#' `truth`, whose calibration curve is used) and integrate with the
#' trapezoidal rule over the coverage levels of the curve.
#'
#' * `aurc()`: area under the risk-coverage curve, selective risk against
#'   total coverage (Eq. 25). Smaller is better.
#' * `augrc()`: area under the generalized risk-coverage curve, generalized
#'   risk against total coverage (Eq. 26; Traub et al., 2024). Smaller is
#'   better.
#' * `auac()`: area under the accuracy-coverage curve (Eq. 22). Larger is
#'   better.
#' * `auic()`: area under the IAM-coverage curve (Eq. 22), the main
#'   imbalance-aware performance measure of the paper. Larger is better.
#'   Coverage ranges where IAM is undefined (a class has been rejected
#'   completely) add nothing to the area, so a selector that loses a class
#'   early is penalised.
#' * `ca_aurc()`: class-averaged AURC (Eq. 10, 27). For each class `k` the
#'   selective risk is integrated against the **class-specific** coverage
#'   `phi_k`, then the areas are averaged over classes. Smaller is better.
#' * `ca_augrc()`: class-averaged AUGRC (Eq. 11, 27). Smaller is better.
#' * `auc_all()`: all six as a named vector.
#'
#' By default the trapezoidal rule is applied to the points of the curve
#' only, so the region before the first point (for the default `n_grid = 101`
#' the first point has a small positive coverage) is not counted. With
#' `extend = TRUE` the area is completed to coverage 0: risk and accuracy are
#' held constant at their first defined value and the generalized risk starts
#' at 0. The IAM area is never extended.
#'
#' @param x An `sp_curve` computed with `truth`, or a fitted `sp_selector`.
#' @param extend Complete the area to coverage 0, see Details.
#'
#' @return A single number (a named vector for `auc_all()`).
#' @references
#' Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
#' classification under imbalance in multiclass settings: A novel metric for
#' bias-aware risk-coverage evaluation. *Journal of Biomedical Informatics*,
#' 181, 105084. \doi{10.1016/j.jbi.2026.105084}
#'
#' Traub J, et al. (2024). Overcoming common flaws in the evaluation of
#' selective classification systems. *Advances in Neural Information
#' Processing Systems*, 37, 2323-2347.
#' @seealso [selective_curve()]
#' @examples
#' set.seed(1)
#' logits <- matrix(rnorm(900, sd = 2), ncol = 3)
#' p <- exp(logits) / rowSums(exp(logits))
#' colnames(p) <- c("a", "b", "c")
#' y <- factor(apply(p, 1, function(r) sample(colnames(p), 1, prob = r)),
#'             levels = colnames(p))
#' cv <- selective_curve(p, y)
#' auc_all(cv)
#' aurc(cv)
#' @name auc_all
NULL

#' @rdname auc_all
#' @export
aurc <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  sp_area(x$coverage, x$risk, if (extend) "constant" else "none")
}

#' @rdname auc_all
#' @export
augrc <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  sp_area(x$coverage, x$gen_risk, if (extend) "zero" else "none")
}

#' @rdname auc_all
#' @export
auac <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  sp_area(x$coverage, x$accuracy, if (extend) "constant" else "none")
}

#' @rdname auc_all
#' @export
auic <- function(x) {
  x <- sp_as_curve_arg(x)
  sp_area(x$coverage, x$iam, "none")
}

#' @rdname auc_all
#' @export
ca_aurc <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  sp_class_area(x, "risk", if (extend) "constant" else "none")
}

#' @rdname auc_all
#' @export
ca_augrc <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  sp_class_area(x, "gen_risk", if (extend) "zero" else "none")
}

#' @rdname auc_all
#' @export
auc_all <- function(x, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  c(AURC = aurc(x, extend), AUGRC = augrc(x, extend),
    AUAC = auac(x, extend), AUIC = auic(x),
    CA_AURC = ca_aurc(x, extend), CA_AUGRC = ca_augrc(x, extend))
}

#' Area under the curve of any metric
#'
#' Area under the curve of an arbitrary column of an [selective_curve()], plotted
#' against total coverage, with the trapezoidal rule. Use it for metrics you
#' added with the `metrics` argument of [selective_curve()], or for any of the
#' built-in columns (`"risk"`, `"accuracy"`, `"iam"`, ...).
#'
#' Coverage levels where the metric is undefined (`NA`) add nothing to the
#' area. With `extend = TRUE` the region before the first defined point is
#' filled by holding the first defined value constant (as in [auc_all()]).
#'
#' @param x An [selective_curve()] computed with `truth`, or a fitted
#'   [sp_selector][selective_fit()] (its calibration curve, which only has the
#'   built-in columns).
#' @param metric Column name, or an [metric_custom()] object whose name is the
#'   column.
#' @param extend Complete the area to coverage 0.
#'
#' @return A single number.
#' @seealso [auc_all()], [selective_curve()]
#' @examples
#' set.seed(1)
#' logits <- matrix(rnorm(900, sd = 2), ncol = 3)
#' p <- exp(logits) / rowSums(exp(logits))
#' colnames(p) <- c("a", "b", "c")
#' y <- factor(apply(p, 1, function(r) sample(colnames(p), 1, prob = r)),
#'             levels = colnames(p))
#' sel <- selective_fit(p, y)
#' cv <- selective_curve(sel, p, y, metrics = metric_macro_f1())
#' auc_of(cv, metric_macro_f1())
#' auc_of(cv, "macro_f1")
#' @export
auc_of <- function(x, metric, extend = FALSE) {
  x <- sp_as_curve_arg(x)
  nm <- if (inherits(metric, "sp_metric")) metric$name else metric
  if (!is.character(nm) || length(nm) != 1L || !nm %in% names(x) ||
      nm %in% c("alpha", "threshold", "n_accepted", "coverage")) {
    stop("`metric` is not a column of the curve. Available: ",
         paste(setdiff(names(x), c("alpha", "threshold", "n_accepted",
                                   "coverage")), collapse = ", "),
         ". Add metrics with selective_curve(..., metrics = ).", call. = FALSE)
  }
  sp_area(x$coverage, x[[nm]], if (extend) "constant" else "none")
}
