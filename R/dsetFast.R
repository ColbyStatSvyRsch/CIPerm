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
#' @return A data frame ready to be used in \code{cint()} or \code{pval()}.
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y)
#' knitr::kable(demo, digits = 2)
#' @export


# TODO: in the future, consider switching to RcppAlgos::comboSample(),
# which lets you sample *directly* from the set of all possible combinations
# without having to generate them all first.
# However, that would force us to Require several other packages.
# For now, let's start with just using base sample()
# (even though it gives permutations, not only unique combinations,
#  so that we might get multiple perms equivalent to the same combo;
#  we'll need to handle multiple perms equivalent to original data grouping).


dsetFast <- function(group1, group2, nmc = 10000){
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

  dcombn2 <- apply(dc, 2, function(x) setdiff(1:9, x))
  group1_perm <- combined[dcombn]
  group2_perm <- combined[dcombn2]

  k <- colSums(matrix(dcombn %in% ((n+1):N), nrow=n))
  diffmean <- colMeans(group1_perm) - colMeans(group2_perm)
  sum1 <- colSums(group1_perm)
  diffmedian <- matrixStats::colMedians(group1_perm) - matrixStats::colMedians(group2_perm)

  r <- rank(combined, ties.method = "first")
  wilsum <- colSums(r[dcombn])
  wkd = (diffmean[1] - diffmean) / (k * den)

  dataframe <- data.frame(diffmean = diffmean,
                          sum1 = sum1,
                          diffmedian = diffmedian,
                          wilsum = wilsum,
                          k = k,
                          wkd = wkd)


  w.i <- sort(dataframe$wkd, decreasing = FALSE, na.last = FALSE)
  ID <- c(1:num)
  dataset <- t(rbind(group1_perm, group2_perm))
  datatable <- cbind(ID, dataset, dataframe, w.i)
  return(datatable)
}
