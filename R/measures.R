#' Measures at one operating point
#'
#' Computes the quantities that describe a selective classifier at a single
#' operating point, given which observations were accepted. They follow
#' Sağlam et al. (2026), Eqs. 2-3 and 20-24:
#'
#' * `coverage`: share of observations that were accepted (`phi`).
#' * `coverage_<class>`: share of the observations of each **true** class that
#'   were accepted (`phi_k`). Large differences between classes mean that
#'   rejection is imbalanced.
#' * `risk`: selective risk, the error rate among accepted observations
#'   (`R`). `NA` when nothing is accepted.
#' * `gen_risk`: generalized risk, the share of *all* observations that were
#'   both misclassified and accepted (`GR`).
#' * `accuracy`: accuracy among accepted observations (`1 - risk`).
#' * `iam`: the Imbalance Accuracy Metric of Mortaz (2020) computed on the
#'   confusion matrix of the accepted observations. It is `NA`
#'   (undefined) when some class appears neither among the accepted true
#'   labels nor among the accepted predictions, i.e. when a class has been
#'   rejected completely.
#'
#' @param accept Logical vector, `TRUE` for accepted observations, for
#'   example `predict(sel, probs, type = "accept")`.
#' @param pred Predicted classes, one per observation.
#' @param truth True classes, one per observation.
#'
#' @return A one-row data frame with columns `coverage`, `n_accepted`,
#'   `risk`, `gen_risk`, `accuracy`, `iam` and one `coverage_<class>` column
#'   per class.
#' @references
#' Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
#' classification under imbalance in multiclass settings: A novel metric for
#' bias-aware risk-coverage evaluation. *Journal of Biomedical Informatics*,
#' 181, 105084. \doi{10.1016/j.jbi.2026.105084}
#'
#' Mortaz E (2020). Imbalance accuracy metric for model selection in
#' multi-class imbalance classification problems. *Knowledge-Based Systems*,
#' 210, 106490.
#' @seealso [selective_curve()] for the same measures along a whole coverage range.
#' @examples
#' truth <- factor(rep(c("a", "b"), each = 10))
#' pred  <- factor(c(rep(c("a", "b"), c(8, 2)), rep(c("a", "b"), c(1, 9))))
#' accept <- rep(c(TRUE, FALSE), c(15, 5))
#' selective_measures(accept, pred, truth)
#' @export
selective_measures <- function(accept, pred, truth) {
  if (!is.logical(accept) || anyNA(accept)) {
    stop("`accept` must be a logical vector without missing values.",
         call. = FALSE)
  }
  if (length(pred) != length(accept) || length(truth) != length(accept)) {
    stop("`accept`, `pred` and `truth` must have the same length.",
         call. = FALSE)
  }
  classes <- if (is.factor(truth)) {
    levels(truth)
  } else if (is.factor(pred)) {
    levels(pred)
  } else {
    sort(unique(c(as.character(truth), as.character(pred))))
  }
  y <- sp_as_truth(truth, classes, length(accept))
  p <- sp_as_truth(pred, classes, length(accept))
  yi <- as.integer(y)
  pi <- as.integer(p)
  vals <- sp_point_measures(accept, yi, pi, length(classes),
                            tabulate(yi, length(classes)), classes)
  as.data.frame(as.list(vals), check.names = FALSE)
}

# IAM from a confusion matrix (rows = true class, columns = predicted class).
# NA when any class has an empty row and column.
sp_iam_from_cm <- function(cm) {
  d <- diag(cm)
  rt <- rowSums(cm)
  ct <- colSums(cm)
  den <- pmax(rt, ct)
  if (any(den == 0)) {
    return(NA_real_)
  }
  mean((d - pmax(rt - d, ct - d)) / den)
}

# All point measures as a named numeric vector. `yi`, `pi` are integer class
# codes of truth and prediction, `n_true` the class counts of `yi`.
sp_point_measures <- function(accept, yi, pi, K, n_true, classes) {
  n <- length(yi)
  idx <- which(accept)
  n_acc <- length(idx)
  yk <- yi[idx]
  pk <- pi[idx]
  n_err <- sum(yk != pk)
  cov_k <- tabulate(yk, nbins = K) / n_true
  cov_k[n_true == 0L] <- NA_real_
  if (n_acc > 0L) {
    cm <- matrix(tabulate((yk - 1L) * K + pk, nbins = K * K), K, K,
                 byrow = TRUE)
    risk <- n_err / n_acc
    iam <- sp_iam_from_cm(cm)
  } else {
    risk <- NA_real_
    iam <- NA_real_
  }
  out <- c(coverage = n_acc / n, n_accepted = n_acc, risk = risk,
           gen_risk = n_err / n, accuracy = 1 - risk, iam = iam, cov_k)
  names(out)[-seq_len(6L)] <- paste0("coverage_", classes)
  out
}
