
# RobZS

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

RobZS is  an R-package for fitting robust linear log-contrast models [1] combined with the elastic-net regularization [2].

By combining the least trimmed squares (LTS) objective function with the elastic-net penalty, Monti et al. (2020) [3] introduced the sparse least trimmed squares estimator with compositional
covariates for high dimensional data. They proposed a trimmed version of the ZeroSum estimator [4].


## Installation

You can install the released version of RobZS from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RobZS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RobZS)
## basic example code
```

## References

1. Aitchison J, Bacon-Shone J (1984) Log Contrast Models for Experiments with Mixtures. Biometrika; 71(2): 323-330.
2. Zou H, Hastie T (2005) Regularization and variable selection via the elastic net. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 67(2):301-320.
3. Monti GS, Filzmoser, P (2020) Sparse least trimmed squares regression with compositional
covariates for high dimensional data, _submitted manuscript_
4. Altenbuchinger M, Rehberg T, Zacharias HU, et al. (2017) Reference point insensitive molecular data analysis. Bioinformatics; 33 2: 219-226.
