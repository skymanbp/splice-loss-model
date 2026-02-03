# Splice Loss GLMM Model

Predicting optical fiber splice power losses based on geometric features using Generalized Linear Mixed Models (GLMM).

## Overview

This project provides a complete workflow for analyzing and predicting splice loss in optical fiber connections. It uses mixed-effects models to account for fiber-to-fiber variation while estimating the effects of geometric features on splice performance.

## Features

- Data preprocessing and feature engineering
- Exploratory data analysis with visualizations
- Multiple GLMM model comparison (AIC/BIC, likelihood ratio tests)
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
│   ├── modeling.R         # GLMM model building
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
  "lmerTest",    # P-values for lmer
  "readxl",      # Excel file reading
  "dplyr",       # Data manipulation
  "ggplot2",     # Visualization
  "performance", # Model diagnostics
  "yaml"         # Configuration parsing
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
2. Run the main script:

```r
source("main.R")
```

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

The input Excel file should contain the following columns:

| Column | Description | Unit |
|--------|-------------|------|
| fiber1 | First fiber identifier | - |
| fiber2 | Second fiber identifier | - |
| splice_type | Type of splice (Self/Cross) | - |
| test_no | Test number | - |
| core_no | Core number | - |
| result | Splice loss (target variable) | dB |
| fiber1_dist_center | Fiber 1 distance to center | micron |
| fiber2_dist_center | Fiber 2 distance to center | micron |
| fiber1_pitch | Fiber 1 pitch angle | degrees |
| fiber2_pitch | Fiber 2 pitch angle | degrees |

## Configuration

Edit `config.yaml` to customize:

- Input/output file paths
- Model parameters
- Visualization settings
- Logging options

## Model Description

The GLMM model structure:

```
splice_loss ~ splice_type + fiber1_dist_center + fiber2_dist_center +
              pitch_diff + avg_pitch + core_no +
              (1 | fiber1) + (1 | fiber2)
```

**Fixed Effects:**
- Splice type (Self vs Cross)
- Fiber distances to center
- Pitch difference and average

**Random Effects:**
- Fiber 1 ID (random intercept)
- Fiber 2 ID (random intercept)

## Output

The analysis generates:

- **Plots**: Distribution, boxplots, scatter plots, diagnostic plots
- **Model file**: `splice_loss_glmm_model.rds`
- **Summary report**: Model coefficients and performance metrics

## Running Tests

```r
# Install testthat if needed
install.packages("testthat")

# Run tests
testthat::test_dir("tests/testthat")
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
