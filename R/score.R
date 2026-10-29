#' Uncertainty scores
#'
#' A score turns each row of class probabilities into one number that
#' measures **uncertainty: lower means more confident**. A row is accepted
#' when its score is less than or equal to the selector's threshold, so every
#' score, including custom ones, follows the same rule.
#'
#' The first three scores use only the class probabilities. They follow the
#' definitions of Sağlam et al. (2026), Eqs. 15-17:
#'
#' * `uncert_lc()`: least confidence, `1 - max(p)`.
#' * `uncert_margin()`: `1 - (p1 - p2)`, where `p1` and `p2` are the two
#'   largest probabilities.
#' * `uncert_entropy()`: Shannon entropy `-sum(p log p)`.
#'
#' With `normalize = TRUE`, least confidence is multiplied by `K / (K - 1)`
#' and entropy is divided by `log(K)`, so that both lie in `[0, 1]` and
#' thresholds are comparable across problems with different numbers of
#' classes. The margin score is always in `[0, 1]`.
#'
#' The last two scores need **posterior draws** of the class probabilities,
#' for example from a Bayesian model such as BART (Eqs. 18-19). Pass an
#' array of dimension `n x K x M` (observations, classes, draws) as `probs`
#' to [selective_fit()] and `predict()`. The point probabilities used for the
#' predicted class are the means over the draws.
#'
#' * `uncert_var()`: predictive variance, the average over classes of the
#'   variance of `p_k` across draws (sample variance).
#' * `uncert_mi()`: mutual information (epistemic uncertainty), the entropy
#'   of the mean probabilities minus the mean entropy of the draws.
#'
#' @param fun For `uncert_custom()`: a function taking an `n x K` probability
#'   matrix (columns named by class) and returning a numeric vector of
#'   length `n`. Lower values must mean more confident. With
#'   `needs_draws = TRUE` it is called as `fun(p, draws)`, where `draws` is
#'   the `n x K x M` array.
#' @param name A short label used when printing.
#' @param needs_draws Whether the score needs posterior draws.
#' @param normalize Whether to rescale the score to `[0, 1]`, see Details.
#'
#' @return An object of class `sp_score`.
#' @references
#' Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
#' classification under imbalance in multiclass settings: A novel metric for
#' bias-aware risk-coverage evaluation. *Journal of Biomedical Informatics*,
#' 181, 105084. \doi{10.1016/j.jbi.2026.105084}
#' @examples
#' p <- rbind(c(0.9, 0.05, 0.05), c(0.4, 0.35, 0.25), c(1/3, 1/3, 1/3))
#' colnames(p) <- c("a", "b", "c")
#' uncert_margin()$fun(p)
#' uncert_entropy(normalize = TRUE)$fun(p)
#'
#' # A custom score
#' my_score <- uncert_custom(function(p) 1 - apply(p, 1, max), name = "one_minus_max")
#' my_score
#' @name uncert_custom
NULL

#' @rdname uncert_custom
#' @export
uncert_custom <- function(fun, name = "custom", needs_draws = FALSE) {
  if (!is.function(fun)) {
    stop("`fun` must be a function.", call. = FALSE)
  }
  structure(
    list(fun = fun, name = as.character(name)[1L],
         needs_draws = isTRUE(needs_draws)),
    class = "sp_score"
  )
}

#' @rdname uncert_custom
#' @export
uncert_lc <- function(normalize = FALSE) {
  uncert_custom(function(p) {
    u <- 1 - p[cbind(seq_len(nrow(p)), max.col(p, ties.method = "first"))]
    if (normalize) u * ncol(p) / (ncol(p) - 1) else u
  }, name = "least_confidence")
}

#' @rdname uncert_custom
#' @export
uncert_margin <- function() {
  uncert_custom(function(p) {
    n <- nrow(p)
    i <- seq_len(n)
    j1 <- max.col(p, ties.method = "first")
    p1 <- p[cbind(i, j1)]
    q <- p
    q[cbind(i, j1)] <- -Inf
    p2 <- q[cbind(i, max.col(q, ties.method = "first"))]
    1 - (p1 - p2)
  }, name = "margin")
}

#' @rdname uncert_custom
#' @export
uncert_entropy <- function(normalize = FALSE) {
  uncert_custom(function(p) {
    h <- pmax(-rowSums(sp_plogp(p)), 0)
    if (normalize) pmin(h / log(ncol(p)), 1) else h
  }, name = "entropy")
}

#' @rdname uncert_custom
#' @export
uncert_var <- function() {
  uncert_custom(function(p, draws) {
    m <- dim(draws)[3L]
    m1 <- rowMeans(draws, dims = 2L)
    m2 <- rowMeans(draws^2, dims = 2L)
    rowMeans(pmax((m2 - m1^2) * m / (m - 1), 0))
  }, name = "variance", needs_draws = TRUE)
}

#' @rdname uncert_custom
#' @export
uncert_mi <- function() {
  uncert_custom(function(p, draws) {
    m <- dim(draws)[3L]
    h_mean <- -rowSums(sp_plogp(p))
    e_h <- -rowSums(sp_plogp(draws), dims = 1L) / m
    pmax(h_mean - e_h, 0)
  }, name = "mutual_information", needs_draws = TRUE)
}

# x * log(x) with 0 log 0 = 0
sp_plogp <- function(x) {
  out <- x * log(x)
  out[x <= 0] <- 0
  out
}

#' @export
print.sp_score <- function(x, ...) {
  cat("<sp_score> ", x$name, " (lower = more confident",
      if (x$needs_draws) ", needs posterior draws" else "", ")\n", sep = "")
  invisible(x)
}

# Apply a score to validated probabilities (and optional draws).
sp_compute_score <- function(score, probs, draws = NULL) {
  if (!inherits(score, "sp_score")) {
    stop("`score` must be created with uncert_custom() or one of uncert_*().",
         call. = FALSE)
  }
  if (score$needs_draws) {
    if (is.null(draws)) {
      stop(sprintf(
        paste0("Score '%s' needs posterior draws: pass an array with ",
               "dimensions observations x classes x draws as `probs`."),
        score$name
      ), call. = FALSE)
    }
    s <- score$fun(probs, draws)
  } else {
    s <- score$fun(probs)
  }
  if (!is.numeric(s) || length(s) != nrow(probs)) {
    stop(sprintf(
      "Score '%s' must return a numeric vector with one value per row (%d).",
      score$name, nrow(probs)
    ), call. = FALSE)
  }
  if (anyNA(s) || any(!is.finite(s))) {
    stop(sprintf("Score '%s' returned missing or non-finite values.",
                 score$name), call. = FALSE)
  }
  as.numeric(s)
}
