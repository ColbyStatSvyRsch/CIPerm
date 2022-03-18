#' dsetFast function
#'
#' DRAFT of faster version of \code{dset()}:
#' Calculate table of differences in means etc. for each
#' combination (or permutation if using Monte Carlo approx.),
#' as needed in order to compute a p-value and/or confidence interval...
#'
#' @param group1 Vector of values for first group.
#' @param group2 Vector of values for second group.
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
#' demo <- dsetFast(x, y, returnData = TRUE)
#' knitr::kable(demo, digits = 2)
#' @export

## TODO: replace dset() with this faster version.
## Skipping the for loop gives a ~7x speedup over our original dset():
##
# > x <- c(19, 22, 25, 26)
# > y <- c(23, 33, 40)
# > microbenchmark::microbenchmark(dset(x, y), dsetFast(x, y), times = 1000)
# Unit: microseconds
#           expr      min       lq     mean   median       uq       max neval
#     dset(x, y) 6276.401 6790.501 7715.261 7210.501 7910.150 51722.501  1000
# dsetFast(x, y)  901.101 1060.551 1215.499 1141.201 1238.651  4548.201  1000
##
## And it does seem to give the same result!
## (though let's check this on other datasets too,
##  and let's check Monte Carlo runs with set.seed()...)
##
# > demo <- dset(x, y)
# > demo2 <- dsetFast(x, y)
# > all.equal(demo, demo2)
# [1] TRUE


## What about the larger groups of 6+9 obs instead of 4+3 obs?
# > wl1 <- c(1.72, 1.64, 1.74, 1.70, 1.82, 1.82, 1.90, 1.82, 2.08)
# > wl2 <- c(1.78, 1.86, 1.96, 2.00, 2.00, 1.96)
# > microbenchmark::microbenchmark(dset(wl1, wl2), dsetFast(wl1, wl2), times = 50)
# Unit: milliseconds
#               expr       min       lq       mean     median        uq       max neval
#     dset(wl1, wl2) 1138.0176 1226.140 1278.69760 1278.08755 1327.2168 1438.2427    50
# dsetFast(wl1, wl2)   43.3406   47.194   50.46918   49.85155   52.8262   77.2373    50
# > demo <- dset(wl1, wl2)
# > demo2 <- dsetFast(wl1, wl2)
# > all.equal(demo, demo2)
# [1] TRUE
## OH WOW, this is 25x faster now.
## Plain dset() takes ~1sec each time, while dsetFast() is ~0.05sec.
## I imagine the diffs will only get more extreme for larger datasets.



# TODO: in the future, consider switching to RcppAlgos::comboSample(),
# which lets you sample *directly* from the set of all possible combinations
# without having to generate them all first.
# However, that would force us to Require several other packages.
# For now, let's start with just using base sample()
# (even though it gives permutations, not only unique combinations,
#  so that we might get multiple perms equivalent to the same combo;
#  we'll need to handle multiple perms equivalent to original data grouping).


dsetFast <- function(group1, group2, nmc = 10000, returnData = FALSE){
  stopifnot(nmc >= 0)
  # TODO: add more error checks:
  #   nmc must be a non-neg integer, not 1, & not too small relative to n,m(?);
  #   group1 and group2 must be numeric, vectors, and non-empty

  # creates the dataset referenced in pval and cint
  combined <- c(group1, group2)

  n <- length(group1)
  m <- length(group2)
  den <- (1/n + 1/m)

  N <- n + m
  num <- choose(N, n)
  if(nmc == 0 | num <= nmc) {
    dcombn <- utils::combn(1:N, n)
  } else { # use Monte Carlo sample of permutations, not all possible combinations
    dcombn <- replicate(nmc, sample(N, n))
    dcombn[,1] <- 1:n # force the 1st "combination" to be original data order
    num <- nmc
  }

  dcombn2 <- apply(dcombn, 2, function(x) setdiff(1:N, x))
  group1_perm <- matrix(combined[dcombn], nrow = n)
  group2_perm <- matrix(combined[dcombn2], nrow = m)

  k <- colSums(matrix(dcombn %in% ((n+1):N), nrow=n))
  diffmean <- colMeans(group1_perm) - colMeans(group2_perm)
  sum1 <- colSums(group1_perm)
  diffmedian <- matrixStats::colMedians(group1_perm) - matrixStats::colMedians(group2_perm)

  r <- rank(combined, ties.method = "first")
  wilsum <- colSums(matrix(r[dcombn], nrow = n))
  wkd = (diffmean[1] - diffmean) / (k * den)

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
