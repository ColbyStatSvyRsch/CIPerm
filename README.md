# CIPerm

<!-- badges: start -->
[![R-CMD-check](https://github.com/ColbyStatSvyRsch/CIPerm/workflows/R-CMD-check/badge.svg)](https://github.com/ColbyStatSvyRsch/CIPerm/actions)
<!-- badges: end -->

This is a (draft) R package that implements computationally-efficient construction of confidence intervals from permutation tests or randomization tests for simple differences in means. In other words, if we set up a permutation or randomization test to evaluate $H_0: \mu_A - \mu_B = 0$, how do we use those same permutations to cheaply construct a CI for the $(\mu_A - \mu_B)$ parameter?

The method is based on Minh D. Nguyen's 2009 MS thesis paper, "Nonparametric Inference using Randomization and Permutation Reference Distribution and their Monte-Carlo Approximation."

## Installation

For the latest development version, install directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ColbyStatSvyRsch/CIPerm", build_vignettes = TRUE)
```

## Usage

```r
x <- c(19, 22, 25, 26)
y <- c(23, 33, 40)
demo <- dset(x, y)
cint(demo, .95, "Two")
#> [1] -21   3
```

## References

Ernst, M.D. (2004).
"Permutation Methods: A Basis for Exact Inference,"
*Statistical Science*, vol. 19, no. 4, 676-685,
[DOI:10.1214/088342304000000396](https://doi.org/10.1214/088342304000000396).

Nguyen, M.D. (2009).
"Nonparametric Inference using Randomization and Permutation
Reference Distribution and their Monte-Carlo Approximation"
[unpublished MS thesis], Portland State University.
