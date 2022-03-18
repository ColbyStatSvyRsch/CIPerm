#' pval function
#'
#' Calculate p-value from permutation test...
#'
#' @param data The output of \code{dset()}.
#' @param tail Which tail? Either "Left" or "Right" or "Two"-tailed test.
#' @param value Either "m" for difference in means (default),
#'     "s" for sum of Group 1 values,
#'     "d" for difference in medians,
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



pval <- function(data, tail = c("Two", "Left", "Right"),
                 value = c("m", "s", "d", "w", "a")){

  tail <- match.arg(tail)
  value <- match.arg(value)

  num <- nrow(data)

  if (tail == "Left"){
    pvalmean = sum(data$diffmean <= data$diffmean[1])/num
    pvalsum = sum(data$sum1 <= data$sum1[1])/num
    pvalmedian = sum(data$diffmedian <= data$diffmedian[1])/num
    pvalwilsum = sum(data$wilsum <= data$wilsum[1])/num
  } else if (tail == "Right") {
    pvalmean = sum(data$diffmean >= data$diffmean[1])/num
    pvalsum = sum(data$sum1 >= data$sum1[1])/num
    pvalmedian = sum(data$diffmedian >= data$diffmedian[1])/num
    pvalwilsum = sum(data$wilsum >= data$wilsum[1])/num
  } else { # tail == "Two"
    pvalmean <- sum(abs(data$diffmean - mean(data$diffmean)) >= abs(data$diffmean[1] - mean(data$diffmean)))/num
    pvalsum <- sum(abs(data$sum1 - mean(data$sum1)) >= abs(data$sum1[1] - mean(data$sum1)))/num
    pvalmedian <- sum(abs(data$diffmedian - mean(data$diffmedian)) >= abs(data$diffmedian[1] - mean(data$diffmedian)))/num
    pvalwilsum <- sum(abs(data$wilsum - mean(data$wilsum)) >= abs(data$wilsum[1] - mean(data$wilsum)))/num
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
