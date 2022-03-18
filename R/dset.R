#' dset function
#'
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
#' demo <- dset(x, y, returnData = TRUE)
#' knitr::kable(demo, digits = 2)
#' @export


# TODO: can we get rid of loop somehow? should be much faster...
#   YES, see dsetFast.R instead!

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

  dataframe <- data.frame(diffmean = rep(NA, num),
                          sum1 = rep(NA, num),
                          diffmedian = rep(NA, num),
                          wilsum = rep(NA, num),
                          k = rep(NA, num),
                          wkd = rep(NA, num))

  group1_perm <- matrix(NA, n, num)
  group2_perm <- matrix(NA, m, num)

  for (ii in 1:num){

    g1ind = dcombn[,ii]
    g2ind = (1:N)[-g1ind]

    group1_perm[,ii] <- combined[g1ind]
    group2_perm[,ii] <- combined[g2ind]

    dataframe$diffmean[ii] = mean(combined[g1ind]) - mean(combined[g2ind])
    dataframe$sum1[ii] = sum(combined[g1ind])
    dataframe$diffmedian[ii] = stats::median(combined[g1ind]) - stats::median(combined[g2ind])

    r <- rank(combined, ties.method = "first")
    rsum <- sum(r[g1ind])
    dataframe$wilsum[ii] = rsum
    dataframe$k[ii] = sum(!(g1ind %in% 1:n))  ## TODO: change to sum(g1ind > n) if faster?
    dataframe$wkd[ii] = (dataframe$diffmean[1] - dataframe$diffmean[ii]) / (dataframe$k[ii] * den)
  }

  if(returnData){
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
