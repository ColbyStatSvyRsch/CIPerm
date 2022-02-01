#' cint function
#'
#' Calculate confidence interval from permutation test...
#'
#' @param data The output of dset().
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

  if (tail == "Left"){
    siglevel <- sig
    alpha <- ceiling(solve((1/num), siglevel))
    index <- alpha - 1
    UB <- data$w.i[(num-index)]
    LT = c(-Inf, UB)
    return(LT)
  } else if (tail == "Right"){
    siglevel <- sig
    alpha <- ceiling(solve((1/num), siglevel))
    index <- alpha - 1
    LB <- data$w.i[2+index] # starts counting from the 2nd row of data (not the first (original) which will always be 'NA')
    RT = c(LB, Inf)
    return(RT)
  } else { # tail == "Two"
    siglevel <- sig/2
    alpha <- ceiling(solve((1/num), siglevel))
    index <- alpha - 1
    UB <- data$w.i[(num-index)]
    LB <- data$w.i[2+index] # starts counting from the 2nd row of data (not the first (original) which will always be 'NA')
    Upper <- if(is.na(UB)) Inf else UB
    Lower <- if(is.na(LB)) -Inf else LB
    CI = c(Lower, Upper)
    return(CI)
  }

}
