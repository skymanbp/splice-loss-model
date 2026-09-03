# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - unreleased

Corrects the model response, which was physically wrong and prevented the
shipped pipeline from running at all.

### Fixed

- **Model response.** The three GLMM formulas fitted the `result` column,
  which is the absolute optical power level after the splice and is negative
  for every row of the reference data. `Gamma(link = "log")` requires a
  strictly positive response, so `Rscript main.R` failed with "non-positive
  values not allowed for the 'Gamma' family". The response is now
  `splice_loss = -diff`, derived in `create_derived_features()`: the workbook
  records `diff = result - ref`, so the power lost across the splice is
  `ref - result = -diff`, positive for a lossy splice. The change covers all
  six formulas (`R/modeling.R`, `splice_loss_glmm.R`), the EDA and diagnostic
  plots, the data summary, and `config.yaml`.
- **Example prediction far outside the data range.** `create_example_data()`
  hard-coded `fiber1_dist_center = 0.5` and `fiber2_dist_center = 0.8` micron
  while the reference data runs around 28 micron. Under the log link that
  extrapolation produced a predicted loss of order 1e10 dB. The continuous
  predictors are now column medians of the fitted data, giving 0.0865 dB on
  the reference data.
- **bobyqa evaluation budget.** The extended and interaction fits stopped with
  "maximum number of function evaluations exceeded" at lme4's default budget.
  `build_models()` now passes `optCtrl = list(maxfun = 2e5)`, and that warning
  no longer appears.

### Added

- `filter_nonpositive_loss()` drops rows whose measured loss is not strictly
  positive, logging how many were removed and why. A splice with no measurable
  loss reads at or just below zero because the pre- and post-splice power
  readings differ only by measurement noise; on the reference data this is 35
  of 1088 rows, all within 0.02 dB of zero. Controlled by the new
  `data.drop_nonpositive_loss` (default `true`) and `data.min_loss_db`
  (default `0`) keys in `config.yaml`. The Gamma family was kept: the fits
  converge on the filtered data, so no Tweedie substitution was needed.
- `build_models()` refuses to start on a response containing non-positive
  values, instead of letting `glmer` fail deeper in.
- `create_derived_features()` errors out when the `diff` column is absent,
  rather than silently producing a data frame with no response.
- `tests/testthat/test-modeling.R`: a fixture carrying the workbook's sign
  convention and scale, including non-positive losses, driven through
  `create_derived_features()`, `filter_nonpositive_loss()`, `build_models()`
  and `compare_models()`. It asserts that the response entering `glmer` is
  strictly positive and that all three model objects come back as Gamma
  log-link fits. Suite: 85 passing, up from 41.
- `CHANGELOG.md`.

### Changed

- README documents the response derivation, the non-positive-loss rule, the
  AIC/BIC disagreement behind the fixed model selection, and the singular fit.
- `.gitignore` ignores `output/` in full, so `output/model_summary.txt` is no
  longer committable.

### Known issues

- The extended and interaction fits are singular on the reference data: the
  `fiber1` intercept variance is estimated at exactly zero while `fiber2`
  carries SD 0.831. This is structural — there are only 11 distinct fibers,
  and the fixed effects absorb the between-`fiber1` variation; `Nelder_Mead`
  reproduces the same boundary. The model is kept as documented and the
  warning is left visible. `performance::r2()` consequently returns `NA` for
  the conditional R2; the marginal R2 (0.289) is unaffected.
- `compare_models()` always returns the extended model. AIC marginally prefers
  the interaction model (-3014.61 vs -3012.32, LRT p = 0.038) while BIC
  prefers extended (-2952.81 vs -2950.14), so the choice is left fixed and the
  full comparison table is reported.

