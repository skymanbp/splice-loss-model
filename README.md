# Splice Loss GLMM Model

Predicting optical fiber splice power losses based on geometric features using generalized linear mixed models (`lme4::glmer`, Gamma family with log link).

## Overview

This project provides a complete workflow for analyzing and predicting splice loss in optical fiber connections. It uses mixed-effects models to account for fiber-to-fiber variation while estimating the effects of geometric features on splice performance.

## Features

- Data preprocessing and feature engineering, including the `splice_loss = -diff` response
- Exploratory data analysis with visualizations
- Multiple GLMM comparison (AIC/BIC, likelihood ratio tests) — reported for information only; `compare_models()` always returns the extended model, because AIC and BIC disagree on the reference data (see [Model selection](#model-selection))
- Model diagnostics (residual plots, Q-Q plots)
- Prediction functions for new data
- Configurable via YAML configuration file

## Project Structure

```
splice-loss-model/
├── main.R                 # Main entry script
├── config.yaml            # Configuration file
├── splice_loss_glmm.R     # Original single-file script
├── R/                     # Modular R functions
│   ├── utils.R            # Utility functions
│   ├── data_processing.R  # Data loading and preprocessing
│   ├── visualization.R    # Plotting functions
│   ├── modeling.R         # GLMM building (lme4::glmer, Gamma log link)
│   └── prediction.R       # Prediction functions
├── tests/                 # Unit tests
│   └── testthat/
├── data/                  # Data files (not tracked)
├── output/                # Generated outputs (gitignored in full)
├── CHANGELOG.md           # Release notes
├── DESCRIPTION            # R package metadata
└── LICENSE                # MIT License
```

## Installation

### Prerequisites

- R >= 4.0.0
- Required packages:

```r
install.packages(c(
  "lme4",        # Mixed effects models
  "lmerTest",    # Loaded by main.R; inert for the Gamma glmer fits
  "readxl",      # Excel file reading
  "dplyr",       # Data manipulation
  "ggplot2",     # Visualization
  "performance", # Model diagnostics
  "yaml",        # Configuration parsing
  "rstudioapi"   # Only for the interactive source("main.R") path (main.R:10)
))
```

### Clone Repository

```bash
git clone https://github.com/skymanbp/splice-loss-model.git
cd splice-loss-model
```

## Usage

### Quick Start

1. Place your data file (`splice_data.xlsx`) in the `data/` directory
2. Run the main script from the repository root:

```bash
Rscript main.R
```

From an interactive R session instead:

```r
source("main.R")
```

That second path takes the `interactive()` branch at `main.R:9-11`, which calls
`rstudioapi::getSourceEditorContext()`, so it also needs the `rstudioapi`
package (RStudio only).

### Using Individual Modules

```r
# Load modules
source("R/utils.R")
source("R/data_processing.R")
source("R/modeling.R")
source("R/prediction.R")

# Load configuration
config <- load_config("config.yaml")

# Process data
df <- load_and_preprocess_data(config)

# Build and compare models
models <- build_models(df, config)
comparison <- compare_models(models)

# Make predictions. The model has a log link, so keep the continuous
# predictors inside the observed range — distances run around 28 micron in
# the reference data, and 0.5 micron would extrapolate by a factor of e^28.
new_data <- data.frame(
  splice_type = factor("Cross splice", levels = levels(df$splice_type)),
  fiber1_dist_center = median(df$fiber1_dist_center),
  fiber2_dist_center = median(df$fiber2_dist_center),
  pitch_diff = median(df$pitch_diff),
  avg_pitch = median(df$avg_pitch),
  core_no = factor("2", levels = levels(df$core_no)),
  fiber1 = df$fiber1[1],
  fiber2 = df$fiber2[1]
)

predicted_loss <- predict_splice_loss(new_data, comparison$selected_model)
```

`create_example_data(df)` builds exactly this one-row frame for you.

## Data Format

Columns are mapped **by position, not by header name**: `apply_column_mapping()`
(`R/data_processing.R:59-74`) overwrites `colnames(df)` with the
`data.column_mapping` list from `config.yaml`. The input Excel file must
therefore carry these 15 columns in this order:

| Column | Description | Unit |
|--------|-------------|------|
| fiber1 | First fiber identifier | - |
| fiber2 | Second fiber identifier | - |
| splice_type | Type of splice (Self/Cross) | - |
| test_no | Test number | - |
| core_no | Core number | - |
| ref | Pre-splice reference power level (dropped via `columns_to_remove`; it enters the response through `diff`) | dB |
| result | Measured power level after splice — a level, not a loss; negative throughout | dB |
| diff | `result - ref`; the model response is `splice_loss = -diff` | dB |
| prooftest | Prooftest reading | (unknown) |
| fiber1_dist_center | Fiber 1 distance to center | micron |
| fiber2_dist_center | Fiber 2 distance to center | micron |
| fiber1_pitch | Fiber 1 pitch angle | degrees |
| fiber2_pitch | Fiber 2 pitch angle | degrees |
| ffw | ffw reading | (unknown) |
| unnamed | Trailing column with no header (dropped via `columns_to_remove`) | - |

## Configuration

Edit `config.yaml` to customize:

- Input/output file paths
- Column mapping and `columns_to_remove`
- The non-positive-loss rule: `data.drop_nonpositive_loss` and `data.min_loss_db`,
  both read by `filter_nonpositive_loss()` (`R/data_processing.R:152-182`)
- Confidence level for intervals (`model.confidence_level` and the two `data`
  filter keys above are the only model-affecting keys any code reads;
  `model.response`, the effect lists and the formulas are documentation —
  the formulas are hard-coded in `R/modeling.R:38-66`)
- Visualization settings
- Logging options

## Model Description

The GLMM structure (fitted with `lme4::glmer`, Gamma family, log link, with
predictions returned on the response scale). The response is `splice_loss`,
and the formula is hard-coded at `R/modeling.R:48-55`:

```
splice_loss ~ splice_type + fiber2_dist_center + fiber1_dist_center +
              pitch_diff + avg_pitch + core_no +
              (1 | fiber1) + (1 | fiber2)
```

### The response: `splice_loss = -diff`

`ref` and `result` are absolute optical power levels in dB, both negative
throughout the reference workbook (`result`: 1092 rows, min -7.773 dB, max
-1.19 dB). Neither is a loss. The workbook's `diff` column is `result - ref`,
so the power lost across the splice is

```
splice_loss = ref - result = -diff
```

which is positive for a lossy splice and zero for a lossless one. It is
derived in `create_derived_features()` (`R/data_processing.R:112-134`), which
errors out if `diff` is absent. On the reference data the loss has mean
0.1395 dB, sd 0.3239 dB and max 2.8506 dB.

### Non-positive losses

`Gamma(link = "log")` requires a strictly positive response. A splice with no
measurable loss reads at or just below zero, because the pre- and post-splice
power readings differ only by measurement noise — on the reference data the 35
non-positive rows (22 negative, 13 exactly zero, all within 0.02 dB of zero)
sit well inside the instrument's resolution.

`filter_nonpositive_loss()` (`R/data_processing.R:152-182`) drops them after
`na.omit()` and logs the count and the reason. The rule is explicit in
`config.yaml`:

```yaml
data:
  drop_nonpositive_loss: true   # keep rows where splice_loss > min_loss_db
  min_loss_db: 0.0
```

Set `drop_nonpositive_loss: false` only alongside a family that admits zeros
(Tweedie, for instance); the Gamma fits will fail otherwise, and
`build_models()` refuses to start on a response with non-positive values
rather than letting `glmer` fail deeper in. On the reference data the rule
takes 1092 raw rows to 1088 after `na.omit()` and 1053 into the fits.

The family was not changed: the Gamma fits converge on the filtered data, so
no Tweedie substitution was needed.

**Fixed Effects:**
- Splice type (Self vs Cross)
- Fiber distances to center
- Pitch difference and average
- Core number

**Random Effects:**
- Fiber 1 ID (random intercept)
- Fiber 2 ID (random intercept)

### Singular fit

On the reference data the extended and interaction models both report
`boundary (singular) fit`: the `fiber1` intercept variance is estimated at
exactly zero while `fiber2` carries SD 0.831. This is structural, not an
optimizer artifact. There are only 11 distinct fibers (7 in the `fiber1`
column, 10 in `fiber2`), and once `fiber1_dist_center`, `avg_pitch` and
`core_no` enter as fixed effects there is no residual between-`fiber1`
variation left to estimate — the basic model, which omits those terms, fits
`fiber1` at SD 0.175 and is not singular. Refitting with `Nelder_Mead`
reproduces the same boundary (`fiber1` SD 0.0012, identical log-likelihood).

The model is kept as documented and the warning is left visible rather than
suppressed. One consequence is that `performance::r2()` cannot compute a
conditional R2 on a boundary fit and returns `NA` for it; the marginal R2
(0.289) is still reported.

`build_models()` passes `optCtrl = list(maxfun = 2e5)` to `bobyqa`
(`R/modeling.R:34-35`). At lme4's default budget the extended and interaction
fits stop with "maximum number of function evaluations exceeded" before
reaching the optimum; with the raised budget that warning is gone and only the
singularity remains.

### Model selection

On the reference data AIC and BIC disagree:

| Model | AIC | BIC |
|---|---|---|
| basic | -2961.11 | -2926.40 |
| extended | -3012.32 | -2952.81 |
| interactions | -3014.61 | -2950.14 |

The extended model improves decisively on the basic one (LRT chi2 = 61.21 on
5 df, p = 6.8e-12). Adding the interaction and the `test_no` random intercept
buys 2.29 AIC (LRT chi2 = 4.29 on 1 df, p = 0.038) but costs 2.67 BIC. That is
a modelling judgement rather than something a single criterion settles, so
`compare_models()` keeps returning `extended` and reports the table for the
reader to weigh.

### Predicting outside the observed range

The link is logarithmic, so an out-of-range covariate is exponentiated into a
meaningless number. In the reference data both distances to center run around
28 micron (`fiber1_dist_center` 28.33-28.93, `fiber2_dist_center`
26.79-29.06). `create_example_data()` (`R/prediction.R:78-97`) therefore
takes column medians from the fitted data rather than hard-coded constants.

## Output

Everything the pipeline writes lands in `output/`, which is gitignored in
full — generated plots, models and summaries are never committed.

- **Plots**: Distribution, boxplots, scatter plots, diagnostic plots
- **Model file**: `splice_loss_glmm_model.rds`
- **Summary report**: `output/model_summary.txt` — observation count, effect
  structure (names only) and marginal/conditional R2. The conditional R2 reads
  `NA` on the reference data because the selected fit is singular; see
  [Singular fit](#singular-fit).

## Running Tests

From the repository root:

```bash
Rscript tests/testthat.R
```

The runner locates the repository root, sources the `R/` modules, and then
executes the `testthat` suite (a bare `testthat::test_dir()` would fail,
because the tested functions live in sourced modules, not an installed
package). Requires the `testthat`, `lme4`, `dplyr`, and `yaml` packages.

Without a local R installation, run the suite in a container:

```bash
docker run --rm -v "$(pwd):/pkg" -w /pkg rocker/r2u:latest \
  bash -c "install.r testthat lme4 dplyr yaml && Rscript tests/testthat.R"
```

## License

MIT License - see [LICENSE](LICENSE) file.

## Author

Zhe Zhang ([@skymanbp](https://github.com/skymanbp))

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request
