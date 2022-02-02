#' dset function
#'
#' Calculate table of differences in means etc. for each permutation,
#' as needed in order to compute a p-value and/or confidence interval...
#'
#' @param group1 Vector of values for first group.
#' @param group2 Vector of values for second group.
#' @return A data frame(?) ready to be used in cint() or pval().
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y)
#' knitr::kable(demo, digits = 2)
#' @export


# TODO: add an argument like `B` in fisher.test()
# or `nmc` in exact2x2::binomMeld.test()
# that lets user choose complete vs monte carlo:
# if nmc==0, force complete enumeration;
# else if choose(N,n)>nmc, do Monte Carlo, but if <= nmc, do complete enum.

dset <- function(group1, group2){
  # creates the dataset referenced in pval and cint
  combined <- c(group1, group2)

  n <- length(group1)
  m <- length(group2)
  N <- n + m
  num <- choose(N, n)
  den <- (1/n + 1/m)
  dcombn <- utils::combn(1:N, n) # TODO: if num is huge, don't run this yet;
  # instead, take a subset more cleverly/randomly!
  # TODO: in the future, consider switching to RcppAlgos::comboSample(),
  # but for now let's start with just using base sample()
  # (even though it gives permutations, not only unique combinations)...
  #

  if (num > 10000) {
    sample <- sample(num, 10000)
    dcombn <- dcombn[,sample]
    dcombn[,1] <- 1:n
    num <- ncol(dcombn)
  }

  dataframe <- data.frame(diffmean = rep(NA, num),
                          sum1 = rep(NA, num),
                          diffmedian = rep(NA, num),
                          wilsum = rep(NA, num),
                          k = rep(NA, num),
                          wkd = rep(NA, num))

  group1_perm <- matrix(NA, n, num)
  group2_perm <- matrix(NA, m, num)

  for (ii in 1:num){ # TODO: can we get rid of loop somehow? should be much faster...
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

  w.i <- sort(dataframe$wkd, decreasing = FALSE, na.last = FALSE)
  ID <- c(1:num)
  dataset <- t(rbind(group1_perm, group2_perm))
  datatable <- cbind(ID, dataset, dataframe, w.i)
  return(datatable)
}
