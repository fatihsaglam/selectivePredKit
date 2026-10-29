# Internal helpers for class probabilities and labels -------------------------

# Validate class probabilities and return a numeric matrix with named columns.
#
# * data frames: if any column starts with ".pred_" (tidymodels convention),
#   only those columns are used and the prefix is stripped. `.pred_class` is
#   the hard prediction in tidymodels output and is never a class.
# * `classes`: if given, columns are matched by name (or by position when the
#   input has no column names) and reordered to follow `classes`.
# * rows must sum to 1 within a tolerance of 0.01; they are then renormalised.
sp_as_probs <- function(probs, classes = NULL, arg = "probs") {
  if (inherits(probs, "data.frame")) {
    nm <- names(probs)
    pred_cols <- startsWith(nm, ".pred_") & nm != ".pred_class"
    if (any(pred_cols)) {
      probs <- probs[, pred_cols, drop = FALSE]
    }
    probs <- as.matrix(as.data.frame(probs))
  }
  if (!is.matrix(probs) || !is.numeric(probs)) {
    stop(sprintf(
      "`%s` must be a numeric matrix or data frame of class probabilities.",
      arg
    ), call. = FALSE)
  }
  if (nrow(probs) == 0L) {
    stop(sprintf("`%s` has no rows.", arg), call. = FALSE)
  }
  if (ncol(probs) < 2L) {
    stop(sprintf("`%s` needs at least two probability columns.", arg),
         call. = FALSE)
  }
  if (anyNA(probs)) {
    stop(sprintf("`%s` contains missing values.", arg), call. = FALSE)
  }
  if (any(probs < 0)) {
    stop(sprintf("`%s` contains negative probabilities.", arg), call. = FALSE)
  }

  cn <- colnames(probs)
  if (!is.null(cn)) {
    cn <- sub("^\\.pred_", "", cn)
    if (anyDuplicated(cn)) {
      stop(sprintf("`%s` has duplicated column names.", arg), call. = FALSE)
    }
    colnames(probs) <- cn
  }

  if (!is.null(classes)) {
    if (ncol(probs) != length(classes)) {
      stop(sprintf(
        "`%s` has %d columns but there are %d classes (%s).",
        arg, ncol(probs), length(classes), paste(classes, collapse = ", ")
      ), call. = FALSE)
    }
    if (is.null(cn)) {
      colnames(probs) <- classes
    } else {
      miss <- setdiff(classes, cn)
      if (length(miss)) {
        stop(sprintf(
          "`%s` has no probability column for class(es): %s.",
          arg, paste(miss, collapse = ", ")
        ), call. = FALSE)
      }
      probs <- probs[, classes, drop = FALSE]
    }
  } else if (is.null(cn)) {
    colnames(probs) <- paste0("class", seq_len(ncol(probs)))
  }

  rs <- rowSums(probs)
  if (any(abs(rs - 1) > 0.01)) {
    stop(sprintf(
      "Rows of `%s` must sum to 1 (tolerance 0.01); the largest deviation is %.3g.",
      arg, max(abs(rs - 1))
    ), call. = FALSE)
  }
  probs / rs
}

# Accept either class probabilities (matrix / data frame) or posterior draws
# of class probabilities (numeric array n x K x M). Returns a list with
#   P     n x K matrix of (posterior mean) class probabilities
#   draws n x K x M array, or NULL when no draws were given
sp_prepare <- function(probs, classes = NULL, arg = "probs") {
  if (is.array(probs) && length(dim(probs)) == 3L) {
    return(sp_prepare_draws(probs, classes, arg))
  }
  list(P = sp_as_probs(probs, classes, arg), draws = NULL)
}

sp_prepare_draws <- function(draws, classes, arg) {
  d <- dim(draws)
  if (!is.numeric(draws)) {
    stop(sprintf("`%s` must be numeric.", arg), call. = FALSE)
  }
  if (d[1L] == 0L) {
    stop(sprintf("`%s` has no rows.", arg), call. = FALSE)
  }
  if (d[2L] < 2L) {
    stop(sprintf("`%s` needs at least two classes (second dimension).", arg),
         call. = FALSE)
  }
  if (d[3L] < 2L) {
    stop(sprintf("`%s` needs at least two posterior draws (third dimension).",
                 arg), call. = FALSE)
  }
  if (anyNA(draws)) {
    stop(sprintf("`%s` contains missing values.", arg), call. = FALSE)
  }
  if (any(draws < 0)) {
    stop(sprintf("`%s` contains negative probabilities.", arg), call. = FALSE)
  }
  dn <- dimnames(draws)
  cn <- if (is.null(dn)) NULL else dn[[2L]]
  if (!is.null(cn)) {
    cn <- sub("^\\.pred_", "", cn)
    if (anyDuplicated(cn)) {
      stop(sprintf("`%s` has duplicated class names.", arg), call. = FALSE)
    }
  }
  if (!is.null(classes)) {
    if (d[2L] != length(classes)) {
      stop(sprintf(
        "`%s` has %d classes but there are %d classes (%s).",
        arg, d[2L], length(classes), paste(classes, collapse = ", ")
      ), call. = FALSE)
    }
    if (is.null(cn)) {
      cn <- classes
    } else {
      miss <- setdiff(classes, cn)
      if (length(miss)) {
        stop(sprintf(
          "`%s` has no probability slice for class(es): %s.",
          arg, paste(miss, collapse = ", ")
        ), call. = FALSE)
      }
      draws <- draws[, match(classes, cn), , drop = FALSE]
      cn <- classes
    }
  } else if (is.null(cn)) {
    cn <- paste0("class", seq_len(d[2L]))
  }
  dimnames(draws) <- list(NULL, cn, NULL)

  rs <- apply(draws, c(1L, 3L), sum)
  if (any(abs(rs - 1) > 0.01)) {
    stop(sprintf(
      "Class probabilities in `%s` must sum to 1 within each draw (tolerance 0.01); the largest deviation is %.3g.",
      arg, max(abs(rs - 1))
    ), call. = FALSE)
  }
  P <- matrix(rowMeans(draws, dims = 2L), nrow = d[1L])
  colnames(P) <- cn
  list(P = P / rowSums(P), draws = draws)
}

# Predicted class (argmax, first class wins ties) as a factor.
sp_pred_class <- function(probs) {
  cls <- colnames(probs)
  factor(cls[max.col(probs, ties.method = "first")], levels = cls)
}

# Coerce the true labels to a factor with the given levels.
sp_as_truth <- function(truth, classes, n) {
  if (length(truth) != n) {
    stop(sprintf(
      "`truth` has length %d but `probs` has %d rows.", length(truth), n
    ), call. = FALSE)
  }
  if (anyNA(truth)) {
    stop("`truth` contains missing values.", call. = FALSE)
  }
  f <- factor(as.character(truth), levels = classes)
  if (anyNA(f)) {
    stop("`truth` contains labels that are not among the classes: ",
         paste(setdiff(unique(as.character(truth)), classes), collapse = ", "),
         ".", call. = FALSE)
  }
  f
}
