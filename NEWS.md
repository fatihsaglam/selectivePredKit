# selectivePredKit (development version)

* Initial version: uncertainty scores, three kinds of targets (threshold,
  coverage, metric), `selective_fit()`, `predict()` for new data.
* Class-conditional coverage-matching selection
  (`selective_fit(strategy = "class_conditional")`) from Sağlam et al. (2026).
* Scores from posterior draws: `uncert_var()`, `uncert_mi()`.
* Measures: class-specific coverage, generalized risk, IAM (`selective_measures()`,
  `metric_iam()`), risk-coverage curves (`selective_curve()`) and the areas AURC, AUGRC,
  AUAC, AUIC, CA-AURC, CA-AUGRC (`auc_all()` and relatives).
* RStudio project file and citation of the paper.
* LABEL baseline (`selective_fit(strategy = "label")`, `target_level()`): split-conformal
  least ambiguous set-valued classifier of Sadinle, Lei and Wasserman (2019).
  `predict()` returns `.set_size` for it.
* Conventions aligned with the authors' analysis code: sample quantiles of
  type 7, 101 coverage levels by default, and areas computed over the curve
  points only (`extend = TRUE` completes them to coverage 0).
* Functions are named by category instead of a package prefix: `uncert_*`
  (scores), `target_*`, `metric_*`, `selective_*`, and `aurc()`, `auc_all()`,
  `auc_of()` for areas. `auc_of()` integrates any curve column, including
  metrics added with `selective_curve(metrics = )`.
