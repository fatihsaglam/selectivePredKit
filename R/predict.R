#' Predict or abstain on new data
#'
#' Applies a fitted [sp_selector][selective_fit()] to new observations. The
#' threshold(s) learned earlier are reused unchanged: a row is accepted when
#' its uncertainty score is less than or equal to the stored threshold (for
#' the class-conditional strategy, the threshold of its predicted class), and
#' the model's prediction is withheld (`NA`) otherwise.
#'
#' The new data can be given in two ways:
#'
#' * **Probabilities** (`model = NULL`, default): `new_data` holds class
#'   probabilities from any classifier, in the same classes as the
#'   calibration data. Columns are matched by name, so `.pred_<class>`
#'   columns work too. For scores that need posterior draws
#'   ([uncert_var()], [uncert_mi()]) give an array
#'   observations x classes x draws.
#' * **Model**: pass the trained classifier as `model` and the raw
#'   predictors as `new_data`. Probabilities are obtained with
#'   `predict(model, new_data, type = "prob")`, which works for fitted
#'   tidymodels objects (`model_fit`, `workflow`) and for many other model
#'   classes. For anything else give `prob_fun`, a function
#'   `function(model, new_data)` returning a probability matrix, data frame
#'   or draws array.
#'
#' For the `"label"` strategy `.pred_class` is the LABEL prediction (the single
#' class of the set, or the arg max when the set is empty) and the data frame
#' has an extra column `.set_size`.
#'
#' @param object A fitted `sp_selector` from [selective_fit()].
#' @param new_data Class probabilities, or predictors when `model` is given.
#' @param type What to return:
#'   * `"full"` (default): data frame with `.pred_class` (the model's usual
#'     prediction), `.score`, `.accept` and `.pred_selective` (the
#'     prediction, or `NA` when abstaining);
#'   * `"class"`: only `.pred_selective`, a factor;
#'   * `"accept"`: only the logical acceptance decision.
#' @param model Optional trained classifier, see Details.
#' @param prob_fun Optional function `function(model, new_data)` that returns
#'   class probabilities. Only used when `model` is given.
#' @param ... Not used.
#'
#' @return A data frame, a factor or a logical vector, depending on `type`.
#' @seealso [selective_fit()], [selective_measures()] to evaluate the decisions when the
#'   true classes are known.
#' @examples
#' set.seed(7)
#' logits <- matrix(rnorm(900, sd = 2.5), ncol = 3)
#' p <- exp(logits) / rowSums(exp(logits))
#' colnames(p) <- c("a", "b", "c")
#' y <- factor(apply(p, 1, function(r) sample(colnames(p), 1, prob = r)),
#'             levels = colnames(p))
#' sel <- selective_fit(p[1:200, ], y[1:200], target = target_coverage(0.7))
#'
#' # New observations: probabilities from the same classifier
#' out <- predict(sel, p[201:300, ])
#' head(out)
#' mean(out$.accept)
#'
#' # Evaluate the decisions when the truth is known
#' selective_measures(out$.accept, out$.pred_class, y[201:300])
#'
#' # Pass a model through a function that returns probabilities
#' toy_model <- list(p = p)
#' pf <- function(model, new_data) model$p[new_data, ]
#' head(predict(sel, 201:210, model = toy_model, prob_fun = pf))
#' @export
predict.sp_selector <- function(object, new_data,
                                type = c("full", "class", "accept"),
                                model = NULL, prob_fun = NULL, ...) {
  type <- match.arg(type)
  if (is.null(model)) {
    prep <- sp_prepare(new_data, object$classes, arg = "new_data")
  } else {
    out <- sp_model_probs(model, new_data, prob_fun)
    prep <- sp_prepare(out, object$classes, arg = "the model's probabilities")
  }
  P <- prep$P
  s <- sp_compute_score(object$score, P, prep$draws)
  base <- sp_pred_class(P)
  dec <- sp_decide(object$strategy, object$threshold, s, as.integer(base), P)
  accept <- dec$accept
  base <- factor(object$classes[dec$pred], levels = object$classes)
  selective <- base
  selective[!accept] <- NA

  switch(
    type,
    full = {
      out <- data.frame(.pred_class = base, .score = s, .accept = accept,
                        .pred_selective = selective)
      if (!is.null(dec$set_size)) out$.set_size <- dec$set_size
      out
    },
    class = selective,
    accept = accept
  )
}

# Get class probabilities out of a trained model.
sp_model_probs <- function(model, new_data, prob_fun) {
  out <- if (!is.null(prob_fun)) {
    if (!is.function(prob_fun)) {
      stop("`prob_fun` must be a function(model, new_data).", call. = FALSE)
    }
    prob_fun(model, new_data)
  } else {
    stats::predict(model, new_data, type = "prob")
  }
  if (!is.matrix(out) && !is.data.frame(out) && !is.array(out)) {
    stop("Could not get a probability matrix from the model. ",
         "Supply `prob_fun = function(model, new_data) ...` returning one ",
         "column per class.", call. = FALSE)
  }
  out
}
