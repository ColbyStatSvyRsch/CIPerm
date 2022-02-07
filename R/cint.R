#' cint function
#'
#' Calculate confidence interval from permutation test...
#'
#' @param data The output of \code{dset()}.
#' @param sig Significance level (default 0.05 corresponds to 95\% confidence level).
#' @param tail Which tail? Either "Left" or "Right" or "Two"-tailed interval.
#' @return Numeric vector with the CI's two endpoints.
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y)
#' cint(demo, .05, "Two")
#' @export


# TODO: if conf intervals are main goal of this package,
#   and part of the point is to be computationally efficient for large datasets,
#   and cint() only ever uses diff in means NOT the sum, median, or wilcoxon stats...
# THEN maybe we should make cint() able to work directly off the raw data
#   and skip those other stats?
# or even focus cint() and pval() JUST on diff in means,
#   and include those other 3 stats only in hidden fns for vignette purposes
#   (just to show we can replicate Nguyen's paper, but not for package users to use)?


cint <- function(data, sig = .05, tail = c("Two", "Left", "Right")){

  stopifnot(sig > 0 & sig < 1)
  tail <- match.arg(tail)

  num <- nrow(data)

  # TODO: keep an eye on how we use w.i,
  #   because if dset's nmc leads us to use Monte Carlo sims,
  #   we may get some permutations equivalent to orig data
  #   i.e. we may get MORE THAN ONE k=0 and therefore several w.i=NaN...
  # I THINK that we can just replace a hardcoded 1 below
  #   with the nr of k=0 rows, but let's re-check this to make sure.
  nk0 <- sum(data$k == 0)
  stopifnot(nk0 >= 1) # Our code assumes 1st row is orig data, so k=0 at least once

  if (tail == "Left"){
    siglevel <- sig
    index <- ceiling(siglevel*num) - 1
    UB <- data$w.i[(num-index)]
    LT = c(-Inf, UB)
    return(LT)
  } else if (tail == "Right"){
    siglevel <- sig
    index <- ceiling(siglevel*num) - 1
    LB <- data$w.i[1+nk0+index] # starts counting from the (1+nk0)'th row of data
    # (not the first (original) which will always be 'NaN')
    RT = c(LB, Inf)
    return(RT)
  } else { # tail == "Two"
    siglevel <- sig/2  # use half of sig in each tail
    index <- ceiling(siglevel*num) - 1
    UB <- data$w.i[(num-index)]
    LB <- data$w.i[1+nk0+index] # starts counting from the (1+nk0)'th row of data
    # (not the first (original) which will always be 'NaN')
    Upper <- if(is.na(UB)) Inf else UB
    Lower <- if(is.na(LB)) -Inf else LB
    CI = c(Lower, Upper)
    return(CI)
  }

}
