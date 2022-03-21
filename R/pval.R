#' Permutations-methods p-values for difference in means, medians, or Wilcoxon rank sum test
#'
#' Calculate p-values for a two-sample permutation or randomization test.
#' In other words, we set up a permutation or randomization test to evaluate
#' the null hypothesis that groups A and B have the same distribution,
#' then calculate p-values for several alternatives:
#' a difference in means (\code{value="m"}),
#' a difference in medians (\code{value="d"}),
#' or the Wilcoxon rank sum test (\code{value="w"}).
#'
#' @param dset The output of \code{\link{dset}}.
#' @param tail Which tail? Either "Left" or "Right" or "Two"-tailed test.
#' @param value Either "m" for difference in means (default);
#'     "s" for sum of Group 1 values
#'     (equivalent to "m" and included only for sake of checking results against
#'     Nguyen (2009) and Ernst (2004));
#'     "d" for difference in medians;
#'     or "w" for Wilcoxon rank sum statistic;
#'     or "a" for a named vector of all four p-values.
#' @return Numeric p-value for the selected type of test,
#'     or a named vector of all four p-values if \code{value="a"}.
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y)
#' pval(demo, "Left", "s")
#' pval(demo, "Left", "a")
#' @export


# TODO: Should two-tailed tests be for |x-median| and not |x-mean| ???
#   The p-value involves finding how much of null distr
#   is further away from the 50th percentile, right?...
# However, Ernst (2004) paper in *Statistical Science* uses |x-mean|,
#   so let's stick with that for now.



pval <- function(dset, tail = c("Two", "Left", "Right"),
                 value = c("m", "s", "d", "w", "a")){

  tail <- match.arg(tail)
  value <- match.arg(value)

  num <- nrow(dset)

  if (tail == "Left"){
    pvalmean = sum(dset$diffmean <= dset$diffmean[1])/num
    pvalsum = sum(dset$sum1 <= dset$sum1[1])/num
    pvalmedian = sum(dset$diffmedian <= dset$diffmedian[1])/num
    pvalwilsum = sum(dset$wilsum <= dset$wilsum[1])/num
  } else if (tail == "Right") {
    pvalmean = sum(dset$diffmean >= dset$diffmean[1])/num
    pvalsum = sum(dset$sum1 >= dset$sum1[1])/num
    pvalmedian = sum(dset$diffmedian >= dset$diffmedian[1])/num
    pvalwilsum = sum(dset$wilsum >= dset$wilsum[1])/num
  } else { # tail == "Two"
    pvalmean <- sum(abs(dset$diffmean - mean(dset$diffmean)) >= abs(dset$diffmean[1] - mean(dset$diffmean)))/num
    pvalsum <- sum(abs(dset$sum1 - mean(dset$sum1)) >= abs(dset$sum1[1] - mean(dset$sum1)))/num
    pvalmedian <- sum(abs(dset$diffmedian - mean(dset$diffmedian)) >= abs(dset$diffmedian[1] - mean(dset$diffmedian)))/num
    pvalwilsum <- sum(abs(dset$wilsum - mean(dset$wilsum)) >= abs(dset$wilsum[1] - mean(dset$wilsum)))/num
  }

  if (value == "m"){
    return(pvalmean)
  } else if (value == "s"){
    return(pvalsum)
  } else if (value == "d"){
    return(pvalmedian)
  } else if (value == "w"){
    return(pvalwilsum)
  } else { # value == "a"
    return(c(m = pvalmean,
             s = pvalsum,
             d = pvalmedian,
             w = pvalwilsum))
  }

}
