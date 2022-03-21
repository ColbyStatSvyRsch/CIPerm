# `CIPerm`: Computationally-Efficient CIs for Mean Shift from Permutation Methods

<!-- badges: start -->
[![R-CMD-check](https://github.com/ColbyStatSvyRsch/CIPerm/workflows/R-CMD-check/badge.svg)](https://github.com/ColbyStatSvyRsch/CIPerm/actions)
<!-- badges: end -->

This R package implements computationally-efficient construction of confidence intervals from permutation tests or randomization tests for simple differences in means. In other words, if we set up a permutation or randomization test to evaluate $H_0: \mu_A - \mu_B = 0$, how do we use those same permutations to cheaply construct a CI for the $(\mu_A - \mu_B)$ parameter?

The method is based on Minh D. Nguyen's 2009 MS thesis paper, "Nonparametric Inference using Randomization and Permutation Reference Distribution and their Monte-Carlo Approximation."
See the `nguyen` vignette for a brief summary of the method.

## Installation

For the latest development version, install directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ColbyStatSvyRsch/CIPerm", build_vignettes = TRUE)
```

## Usage

First use `dset(x,y)` to tabulate summary statistics for each permutation.
Then pass the results into `cint()` to compute a confidence interval.

```r
x <- c(19, 22, 25, 26)
y <- c(23, 33, 40)
demo <- dset(x, y)
cint(demo, .95, "Two")
#> [1] -21   3
```

You can also pass the results of `dset()` into `pval()` to calculate p-values
for a difference in means (`value="m"`),
a difference in medians (`value="d"`),
or the Wilcoxon rank sum test (`value="w"`).

```r
pval(demo, "Left", "m")
#> [1] 0.08571429
pval(demo, "Left", "d")
#> [1] 0.08571429
pval(demo, "Left", "w")
#> [1] 0.1142857
```

## References

Ernst, M.D. (2004).
"Permutation Methods: A Basis for Exact Inference,"
*Statistical Science*, vol. 19, no. 4, 676-685,
[DOI:10.1214/088342304000000396](https://doi.org/10.1214/088342304000000396).

Nguyen, M.D. (2009).
"Nonparametric Inference using Randomization and Permutation
Reference Distribution and their Monte-Carlo Approximation"
[unpublished MS thesis; Mara Tableman, advisor], Portland State University.
