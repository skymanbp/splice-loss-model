# Run all tests:  Rscript tests/testthat.R   (from the repository root)
#
# This project is run by sourcing the R/ modules (see main.R), not as an
# installed package, so the runner sources the modules itself and uses
# test_dir() rather than test_check("spliceloss"), which would require the
# package to be installed.
library(testthat)

# Locate the repo root whether invoked from the root, tests/, or tests/testthat
root <- Find(function(p) dir.exists(file.path(p, "R")), c(".", "..", "../.."))
if (is.null(root)) {
  stop("Cannot locate the repository root (no R/ directory found)")
}

invisible(lapply(sort(list.files(file.path(root, "R"), full.names = TRUE)), source))

# stop_on_failure: a failing suite must exit non-zero so scripts and CI
# cannot mistake it for a pass
test_dir(file.path(root, "tests", "testthat"), stop_on_failure = TRUE)
