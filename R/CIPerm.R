#' CIPerm: Computationally-Efficient Confidence Intervals for Mean Shift from Permutation Methods
#'
#' Implements computationally-efficient construction of
#' confidence intervals from permutation tests or randomization tests
#' for simple differences in means.
#' The method is based on Minh D. Nguyen's 2009 MS thesis paper,
#' "Nonparametric Inference using Randomization and Permutation
#' Reference Distribution and their Monte-Carlo Approximation,"
#' <\doi{10.15760/etd.7798}>
#' See the \code{nguyen} vignette for a brief summary of the method.
#' First use \code{\link{dset}} to tabulate summary statistics for each permutation.
#' Then pass the results into \code{\link{cint}} to compute a confidence interval,
#' or into \code{\link{pval}} to calculate p-values.
#'
#' Our R function arguments and outputs are structured differently
#' than the similarly-named R functions in Nguyen (2009),
#' but the results are equivalent. In the \code{nguyen} vignette
#' we use our functions to replicate Nguyen's results.
#'
#' Following Ernst (2004) and Nguyen (2009), we use "permutation methods"
#' to include both randomization tests and permutation tests.
#' In the simple settings in this R package,
#' the randomization and permutation test mechanics are identical,
#' but their interpretations may differ.
#'
#' We say "randomization test" under the model where
#' the units are not necessarily a random sample, but the treatment assignment
#' was random. The null hypothesis is that the treatment has no effect.
#' In this case we can make causal inferences about the
#' treatment effect (difference between groups) for this set of individuals,
#' but cannot necessarily generalize to other populations.
#'
#' By contrast, we say "permutation test" under the model where
#' the units were randomly sampled from two distinct subpopulations.
#' The null hypothesis is that the two groups have identical CDFs.
#' In this case we can make inferences about differences between subpopulations,
#' but there's not necessarily any "treatment" to speak of
#' and causal inferences may not be relevant.
#'
#' @references Ernst, M.D. (2004).
#'   "Permutation Methods: A Basis for Exact Inference,"
#'   \emph{Statistical Science}, vol. 19, no. 4, 676-685,
#'   <\doi{10.1214/088342304000000396}>.
#'
#'   Nguyen, M.D. (2009).
#'   "Nonparametric Inference using Randomization and Permutation
#'   Reference Distribution and their Monte-Carlo Approximation"
#'   [unpublished MS thesis; Mara Tableman, advisor], Portland State University.
#'   \emph{Dissertations and Theses}. Paper 5927.
#'   <\doi{10.15760/etd.7798}>.


#' @importFrom matrixStats colMedians
#' @importFrom stats median
#' @importFrom utils combn
#'
#' @docType package
#' @name CIPerm
NULL


