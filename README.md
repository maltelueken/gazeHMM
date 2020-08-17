
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gazeHMM

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/maltelueken/gazeHMM.svg?branch=master)](https://travis-ci.com/maltelueken/gazeHMM)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/maltelueken/gazeHMM?branch=master&svg=true)](https://ci.appveyor.com/project/maltelueken/gazeHMM)
[![Codecov test
coverage](https://codecov.io/gh/maltelueken/gazeHMM/branch/master/graph/badge.svg)](https://codecov.io/gh/maltelueken/gazeHMM?branch=master)
<!-- badges: end -->

## Installation

The package can be installed from GitHub with the following R commands:

``` r

devtools::install_github("maltelueken/gazeHMM")
```

## Structure

The main function of the package is ‘gazeHMM’ which applies the
algorithm to data given some user parameters. Otherwise, the package
contains an example data set from Andersson et al. (2017) and functions
to summarise the algorithms output and inspect the quality of the
results. A guide on how to apply gazeHMM can be found in the vignette
“Classifying gaze data with gazeHMM”:

``` r

vignette("Classifying gaze data with gazeHMM", "gazeHMM")
```

## Literature

Andersson, R., Larsson, L., Holmqvist, K., Stridh, M., & Nyström, M.
(2017). One algorithm to rule them all? An evaluation and discussion of
ten eye movement event-detection algorithms. Behavior Research Methods,
49, 616-637. <https://doi.org/10.3758/s13428-016-0738-9>
