# Run all tests
library(testthat)

# Source the modules
source("../../R/utils.R")
source("../../R/data_processing.R")
source("../../R/visualization.R")
source("../../R/modeling.R")
source("../../R/prediction.R")

test_check("spliceloss")
