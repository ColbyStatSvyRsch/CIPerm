#' Permutation-methods summary statistics
#'
#' Calculate table of differences in means, medians, etc. for each
#' combination (or permutation, if using Monte Carlo approx.),
#' as needed in order to compute a confidence interval using \code{\link{cint}}
#' and/or a p-value using \code{\link{pval}}.
#'
#' @param group1 Vector of numeric values for first group.
#' @param group2 Vector of numeric values for second group.
#' @param nmc Threshold for whether to use Monte Carlo draws or complete
#'   enumeration. If the number of all possible combinations
#'   \code{choose(n1+n2, n1) <= nmc}, we use complete enumeration.
#'   Otherwise, we take a Monte Carlo sample of \code{nmc} permutations.
#'   You can set \code{nmc = 0} to force complete enumeration regardless of
#'   how many combinations there are.
#' @param returnData Whether the returned dataframe should include columns for
#'   the permuted data itself (if TRUE), or only the derived columns that are
#'   needed for confidence intervals and p-values (if FALSE, default).
#' @return A data frame ready to be used in \code{cint()} or \code{pval()}.
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y, returnData = TRUE)
#' knitr::kable(demo, digits = 2)
#' @export


######################
#### Old notes-to-self from when we used to have two versions,
####   dset() using for-loops vs a vectorized dsetFast().
####   As of 3/22/2022, dsetFast() became the current version of dset() below,
####   and the old version was simply removed.
####   Look on GitHub for package versions 0.1.0.9004 or earlier
####   (March 21, 2022 or earlier) to see old dset() code.
##
## CHECK TIMINGS AND OUTPUTS OF dset() VS dsetFast():
##
## Skipping the for loop with dsetFast() gives a ~7x speedup over original dset()!
# > x <- c(19, 22, 25, 26)
# > y <- c(23, 33, 40)
# > microbenchmark::microbenchmark(dset(x, y), dsetFast(x, y), times = 1000)
# Unit: microseconds
#           expr      min       lq     mean   median       uq       max neval
#     dset(x, y) 6276.401 6790.501 7715.261 7210.501 7910.150 51722.501  1000
# dsetFast(x, y)  901.101 1060.551 1215.499 1141.201 1238.651  4548.201  1000
##
## And it does give the same result!
##
# > demo <- dset(x, y)
# > demo2 <- dsetFast(x, y)
# > all.equal(demo, demo2)
# [1] TRUE
##
## Let's check this on other datasets too,
## and let's check Monte Carlo runs with set.seed()...
##
## What about the larger groups of 6+9 obs instead of 4+3 obs?
# > wl1 <- c(1.72, 1.64, 1.74, 1.70, 1.82, 1.82, 1.90, 1.82, 2.08)
# > wl2 <- c(1.78, 1.86, 1.96, 2.00, 2.00, 1.96)
# > microbenchmark::microbenchmark(dset(wl1, wl2), dsetFast(wl1, wl2), times = 50)
# Unit: milliseconds
#               expr       min       lq       mean     median        uq       max neval
#     dset(wl1, wl2) 1138.0176 1226.140 1278.69760 1278.08755 1327.2168 1438.2427    50
# dsetFast(wl1, wl2)   43.3406   47.194   50.46918   49.85155   52.8262   77.2373    50
##
## OH WOW, this is 25x faster now.
## Here, plain dset() takes ~1sec each time, while dsetFast() is ~0.05sec.
## I imagine the diffs will only get more extreme for larger datasets.
##
## And they still give same results?
# > demo <- dset(wl1, wl2)
# > demo2 <- dsetFast(wl1, wl2)
# > all.equal(demo, demo2)
# [1] TRUE
# > demo <- dset(wl1, wl2, returnData = TRUE)
# > demo2 <- dsetFast(wl1, wl2, returnData = TRUE)
# > all.equal(demo, demo2)
# [1] TRUE
## Yes they do!
##
## What about Monte Carlo runs?
## They shouldn't match without set.seed(), and indeed they don't...
# > demo <- dset(wl1, wl2, nmc = 5000)
# > demo2 <- dsetFast(wl1, wl2, nmc = 5000)
# > isTRUE(all.equal(demo, demo2))
# [1] FALSE
##
## But WITH set.seed, they DO match, as they should!
# > seed = 20220321
# > set.seed(seed)
# > demo <- dset(wl1, wl2, nmc = 5000)
# > set.seed(seed)
# > demo2 <- dsetFast(wl1, wl2, nmc = 5000)
# > all.equal(demo, demo2)
# [1] TRUE
##
## So old dset() can be replaced with new dsetFast()
######################



# TODO: in the future, consider switching to RcppAlgos::comboSample(),
# which lets you sample *directly* from the set of all possible combinations
# without having to generate them all first.
# However, that would force us to Require several other packages.
# For now, let's start with just using base sample()
# (even though it gives permutations, not only unique combinations,
#  so that we might get multiple perms equivalent to the same combo;
#  we'll need to handle multiple perms equivalent to original data grouping).


dset <- function(group1, group2, nmc = 10000, returnData = FALSE){
  stopifnot(nmc >= 0)
  stopifnot(nmc != 1)
  stopifnot(is.numeric(group1) & is.numeric(group2))
  stopifnot(length(group1) >= 1 & length(group2) >= 1)
  stopifnot(!any(is.na(c(group1, group2))))

  combined <- c(group1, group2)

  n <- length(group1)
  m <- length(group2)
  den <- (1/n + 1/m)

  N <- n + m
  num <- choose(N, n) # number of possible combinations

  # Form a matrix where each column contains indices in new "group1" for that comb or perm
  if(nmc == 0 | num <= nmc) { # take all possible combinations
    dcombn1 <- utils::combn(1:N, n)
  } else { # use Monte Carlo sample of permutations, not all possible combinations
    dcombn1 <- replicate(nmc, sample(N, n))
    dcombn1[,1] <- 1:n # force the 1st "combination" to be original data order
    num <- nmc
  }

  # Form the equivalent matrix for indices in new "group2"
  dcombn2 <- apply(dcombn1, 2, function(x) setdiff(1:N, x))

  # Form the corresponding matrices of data values, not data indices
  group1_perm <- matrix(combined[dcombn1], nrow = n)
  group2_perm <- matrix(combined[dcombn2], nrow = m)

  # For each comb or perm, compute:
  #   difference in group means; sum in group1; difference in group medians;
  #   and sum of *ranks* in group1 (the statistic for the Wilcoxon rank sum test)
  diffmean <- colMeans(group1_perm) - colMeans(group2_perm)
  sum1 <- colSums(group1_perm)
  diffmedian <- matrixStats::colMedians(group1_perm) - matrixStats::colMedians(group2_perm)
  r <- rank(combined, ties.method = "first")
  wilsum <- colSums(matrix(r[dcombn1], nrow = n))

  # For each comb or perm, compute:
  #   k = how many values swapped from group1 to group2?
  #   wkd = Nguyen (2009) statistic whose quantiles are used for CI endpoints
  k <- colSums(matrix(dcombn1 %in% ((n+1):N), nrow=n))
  wkd <- (diffmean[1] - diffmean) / (k * den)

  dataframe <- data.frame(diffmean = diffmean,
                          sum1 = sum1,
                          diffmedian = diffmedian,
                          wilsum = wilsum,
                          k = k,
                          wkd = wkd)

  if(returnData){
    # Return the whole table formatted as in Nguyen (2009)
    # for comparison with his results
    w.i <- sort(dataframe$wkd, decreasing = FALSE, na.last = FALSE)
    ID <- c(1:num)
    dataset <- t(rbind(group1_perm, group2_perm))
    datatable <- cbind(ID, dataset, dataframe, w.i)
    return(datatable)
  } else {
    # Just return the fewest columns needed for p-vals and CIs
    return(dataframe)
  }

}
