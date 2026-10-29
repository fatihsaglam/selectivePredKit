#' selectivePredKit: Selective Prediction with a Reject Option
#'
#' Selective classification on top of any trained probabilistic classifier.
#' The package never trains a model. It works on class probabilities:
#'
#' 1. An uncertainty **score** turns each row of probabilities into one
#'    number. Lower is better (more confident), see [uncert_lc()],
#'    [uncert_margin()], [uncert_entropy()] and [uncert_custom()].
#' 2. A **target** says how the acceptance threshold is chosen:
#'    [target_threshold()], [target_coverage()] or
#'    [target_metric()].
#' 3. [selective_fit()] learns the threshold on a calibration set and returns a
#'    selector. `predict()` applies it to new data: a row is accepted when
#'    its score is less than or equal to the threshold, otherwise the
#'    prediction is withheld (abstain).
#' 4. [selective_curve()] and [aurc()] evaluate the risk-coverage trade-off.
#'
#' @keywords internal
#' @importFrom stats predict quantile
"_PACKAGE"
