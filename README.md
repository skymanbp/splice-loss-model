# Splice Loss GLMM Model

Predicting optical fiber splice power losses based on geometric features using generalized linear mixed models (`lme4::glmer`, Gamma family with log link).

## Overview

This project provides a complete workflow for analyzing and predicting splice loss in optical fiber connections. It uses mixed-effects models to account for fiber-to-fiber variation while estimating the effects of geometric features on splice performance.

## Features

- Data preprocessing and feature engineering
- Exploratory data analysis with visualizations
- Multiple GLMM comparison (AIC/BIC, likelihood ratio tests) — reported for information only; `compare_models()` always returns the extended model
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
├── output/                # Generated outputs
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

# Make predictions
new_data <- data.frame(
  splice_type = factor("Cross splice", levels = levels(df$splice_type)),
  fiber1_dist_center = 0.5,
  fiber2_dist_center = 0.8,
  pitch_diff = 0.2,
  avg_pitch = 40.3,
  core_no = factor("2", levels = levels(df$core_no)),
  fiber1 = df$fiber1[1],
  fiber2 = df$fiber2[1]
)

predicted_loss <- predict_splice_loss(new_data, comparison$selected_model)
```

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
| ref | Pre-splice reference power level (dropped via `columns_to_remove`) | dB |
| result | Measured power level after splice (model response) | dB |
| diff | `result - ref` | dB |
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
- Confidence level for intervals (`model.confidence_level` is the only key of the
  `model` block that any code reads; the response, effect lists and formulas are
  hard-coded in `R/modeling.R:19-47`)
- Visualization settings
- Logging options

## Model Description

The GLMM structure (fitted with `lme4::glmer`, Gamma family, log link, with
predictions returned on the response scale). The response is the `result`
column, and the formula is hard-coded at `R/modeling.R:29-36`:

```
result ~ splice_type + fiber2_dist_center + fiber1_dist_center +
         pitch_diff + avg_pitch + core_no +
         (1 | fiber1) + (1 | fiber2)
```

`Gamma(link = "log")` requires a strictly positive response. The `result`
column of `data/splice_data.xlsx` holds measured power levels that are negative
throughout (1092 rows, min -7.773 dB, max -1.19 dB), so a strictly positive
response has to be supplied before these fits will run.

**Fixed Effects:**
- Splice type (Self vs Cross)
- Fiber distances to center
- Pitch difference and average
- Core number

**Random Effects:**
- Fiber 1 ID (random intercept)
- Fiber 2 ID (random intercept)

## Output

The analysis generates:

- **Plots**: Distribution, boxplots, scatter plots, diagnostic plots
- **Model file**: `splice_loss_glmm_model.rds`
- **Summary report**: Observation count, effect structure (names only) and marginal/conditional R2

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
