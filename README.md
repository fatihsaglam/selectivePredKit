# selectivePredKit

Selective classification (classification with a reject option) for **any
already-trained probabilistic classifier**, with the class-conditional
selection and the imbalance-aware evaluation measures of

> Sağlam F, Özgen Ü, Uygun A, Dinçer OS, Albayrak C (2026). Selective
> classification under imbalance in multiclass settings: A novel metric for
> bias-aware risk–coverage evaluation. *Journal of Biomedical Informatics*,
> 181, 105084. [doi:10.1016/j.jbi.2026.105084](https://doi.org/10.1016/j.jbi.2026.105084)

The package does not train models. It takes class probabilities (or posterior
draws of them), decides which predictions to trust, and abstains on the rest.
Other names for the same idea: *classification with abstention*, *learning to
reject*, *selective prediction*.

> Status: early development (`0.0.0.9000`). Classification only; selective
> regression is planned.

## Idea

1. A **score** turns each row of probabilities into one uncertainty number.
   **Lower means more confident.**
2. A **target** says how the acceptance threshold is chosen.
3. `selective_fit()` learns the threshold(s) on a calibration set, with one global
   threshold (`strategy = "global"`, the classical approach) or one threshold
   per predicted class (`strategy = "class_conditional"`, coverage matching), or LABEL
   (`strategy = "label"`).
4. `predict()` applies the stored thresholds to new data: a row is accepted
   when `score <= threshold`; otherwise the prediction is withheld (`NA`).
5. `selective_curve()`, `selective_measures()` and `auc_all()` evaluate the result.

## What is in the package

| Part | Functions | Paper |
|---|---|---|
| Scores | `uncert_lc()`, `uncert_margin()`, `uncert_entropy()` | Eqs. 15–17 |
| Scores from posterior draws | `uncert_var()`, `uncert_mi()` | Eqs. 18–19 |
| Custom score | `uncert_custom()` | |
| Targets | `target_threshold()`, `target_coverage()`, `target_level()` (LABEL), `target_metric()` | |
| Selection | `selective_fit(strategy = "global" \| "class_conditional" \| "label")` | Alg. 1; LABEL (Sadinle et al., 2019) |
| Apply to new data | `predict()` | |
| Measures at one operating point | `selective_measures()`: coverage, class-specific coverage, risk, generalized risk, accuracy, IAM | Eqs. 2–3, 20–24 |
| Curves | `selective_curve()`, `plot()` | Figs. 2–6 |
| Areas | `aurc()`, `augrc()`, `auac()`, `auic()`, `ca_aurc()`, `ca_augrc()`, `auc_all()` | Eqs. 10–11, 22, 25–27 |
| Metrics (for targets and curves) | `metric_accuracy()`, `metric_error_rate()`, `metric_balanced_accuracy()`, `metric_macro_f1()`, `metric_iam()`, `metric_custom()` for your own | |

Not included yet: the Friedman/Wilcoxon comparison and the bootstrap
intervals of the paper, selective regression.

## Example

```r
library(selectivePredKit)

# `cal_p`: calibration probabilities (one column per class), `cal_y`: true classes
# `test_p`, `test_y`: held-out data from the same classifier

# Class-conditional selection at 80 % coverage in every predicted class
sel <- selective_fit(cal_p, cal_y,
              score    = uncert_margin(),
              target   = target_coverage(0.8),
              strategy = "class_conditional")
sel

# Predict or abstain on new data
out <- predict(sel, test_p)          # .pred_class, .score, .accept, .pred_selective
selective_measures(out$.accept, out$.pred_class, test_y)

# Imbalance-aware evaluation over the whole coverage range
cv <- selective_curve(sel, test_p, test_y)  # thresholds from calibration, applied to test data
auc_all(cv)                           # AURC, AUGRC, AUAC, AUIC, CA_AURC, CA_AUGRC
plot(cv, "iam")
plot(cv, "class_coverage")           # class coverage against total coverage

# Largest coverage with accuracy >= 0.95 on accepted rows
selective_fit(cal_p, cal_y, target = target_metric(metric_accuracy(), 0.95))

# Hand over the trained model and raw predictors instead of probabilities
predict(sel, new_data = new_x, model = fitted_model)
# (uses predict(model, new_data, type = "prob"); give `prob_fun` for other model classes)
```

For Bayesian models pass an array of posterior draws (observations × classes ×
draws) as `probs` and use `uncert_var()` or `uncert_mi()`.

`predict()` returns a data frame with `.pred_class` (the model's usual
prediction), `.score`, `.accept` and `.pred_selective` (the prediction, or
`NA` when abstaining). Use `type = "class"` or `type = "accept"` for a single
vector.

## Design notes

* **Model-agnostic.** The core only needs class probabilities. Columns named
  `.pred_<class>` (tidymodels' `predict(type = "prob")`) are recognised, but
  nothing from tidymodels is imported. Dependencies: base R `stats` and
  `graphics`.
* **Own metrics.** Metrics have the signature `f(truth, pred, probs)` and are
  evaluated on the accepted observations only. Pass your own with `metric_custom()`.
* **Thresholds come from calibration data.** Fit the threshold on data the
  classifier was not trained on. `selective_curve(sel, probs, truth)` applies those
  thresholds to a separate evaluation set. Distribution-free guarantees on
  selective risk are not implemented yet.
* **Scores follow the paper.** `uncert_lc()` and `uncert_entropy()` are
  unnormalised by default; `normalize = TRUE` rescales them to `[0, 1]`.
* **Area conventions.** The paper gives the integrals but not how the
  region below the first observed coverage is handled. Here risk and accuracy
  are held at their first defined value, the generalized risk starts at 0, and
  the IAM is not extended; IAM segments that are undefined (a class rejected
  completely) add nothing. See `?auc_all`.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/selectivePredKit")
```

Or open `selectivePredKit.Rproj` in RStudio and use *Build → Install* or
`devtools::load_all()`.

## Citation

```r
citation("selectivePredKit")
```

## License

MIT

Sadinle M, Lei J, Wasserman L (2019). Least ambiguous set-valued classifiers
with bounded error levels. *Journal of the American Statistical Association*,
114(525), 223-234. <https://doi.org/10.1080/01621459.2017.1395341>
