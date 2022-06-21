# `CIPerm`: Computationally-Efficient Confidence Intervals for Mean Shift from Permutation Methods

<!-- badges: start -->
[![R-CMD-check](https://github.com/ColbyStatSvyRsch/CIPerm/workflows/R-CMD-check/badge.svg)](https://github.com/ColbyStatSvyRsch/CIPerm/actions)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/CIPerm?color=5f810e)](https://cran.r-project.org/package=CIPerm)
<!-- badges: end -->

This R package implements computationally-efficient construction of confidence intervals from permutation tests or randomization tests for simple differences in means. In other words, if we set up a permutation or randomization test to evaluate $H_0: \mu_A - \mu_B = 0$,
then how can we use that same single set of permutations to cheaply construct a CI for the $(\mu_A - \mu_B)$ parameter?

The method is based on Minh D. Nguyen's 2009 MS thesis paper, "Nonparametric Inference using Randomization and Permutation Reference Distribution and their Monte-Carlo Approximation," http://doi.org/10.15760/etd.7798.
See the `nguyen` vignette for a brief summary of the method and for our replication of Nguyen's results.

Note that our R function arguments and outputs are structured differently than the similarly-named R functions in Nguyen (2009), but the results are equivalent.

A copy of our [useR! 2022 conference](https://user2022.r-project.org/) poster about the package is in our GitHub repo, in the [`data-raw` folder](https://github.com/ColbyStatSvyRsch/CIPerm/tree/master/data-raw).

## Installation

Install a stable version of the package from [CRAN](https://cran.r-project.org/package=CIPerm):

```r
install.packages("CIPerm")
```

Or, for the latest development version, install directly from GitHub:

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
cint(dset = demo, conf.level = .95, tail = "Two")
#> [1] -21   3
```

You can also pass the results of `dset()` into `pval()` to calculate p-values
for a difference in means (`value="m"`),
a difference in medians (`value="d"`),
or the Wilcoxon rank sum test (`value="w"`).

```r
pval(dset = demo, tail = "Left", value = "m")
#> [1] 0.08571429
pval(dset = demo, tail = "Left", value = "d")
#> [1] 0.08571429
pval(dset = demo, tail = "Left", value = "w")
#> [1] 0.1142857
```

See also our `naive` vignette for timing comparisons of Nguyen's one-pass method. In the "naive" approach, you would run many separate permutation tests with different null values for $(\mu_A - \mu_B)$, and your CI would consist of those values where the null hypothesis was not rejected. The vignette shows that if you need to check more than a few null values and your dataset isn't trivially small, Nguyen's method can be considerably faster than the naive approach.

## References

Ernst, M.D. (2004).
"Permutation Methods: A Basis for Exact Inference,"
*Statistical Science*, vol. 19, no. 4, 676-685,
[DOI:10.1214/088342304000000396](https://doi.org/10.1214/088342304000000396).

Nguyen, M.D. (2009).
"Nonparametric Inference using Randomization and Permutation
Reference Distribution and their Monte-Carlo Approximation"
[unpublished MS thesis; Mara Tableman, advisor], Portland State University.
*Dissertations and Theses*. Paper 5927.
[DOI:10.15760/etd.7798](http://doi.org/10.15760/etd.7798).

Tupaj, E. and Wieczorek, J. (2022).
"`CIPerm`: An R Package for Computationally Efficient Confidence Intervals from Permutation Test,"
poster presented at the useR! Conference, held virtually, June 22, 2022.
https://user2022.r-project.org/program/posters/
