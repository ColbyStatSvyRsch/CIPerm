#' cint function
#'
#' Calculate confidence interval for a simple difference in means
#' from a two-sample permutation or randomization test.
#' In other words, we set up a permutation or randomization test to evaluate
#' \eqn{H_0: \mu_A - \mu_B = 0}, then use those same permutations to
#' construct a CI for the parameter \eqn{\delta = (\mu_A - \mu_B)}.
#'
#' @param dset The output of \code{\link{dset}}.
#' @param conf.level Confidence level (default 0.95 corresponds to 95\% confidence level).
#' @param tail Which tail? Either "Left" or "Right" or "Two"-tailed interval.
#' @return Numeric vector with the CI's two endpoints.
#' @examples
#' x <- c(19, 22, 25, 26)
#' y <- c(23, 33, 40)
#' demo <- dset(x, y)
#' cint(demo, .95, "Two")
#' @export



cint <- function(dset, conf.level = .95, tail = c("Two", "Left", "Right")){

  sig = 1 - conf.level
  stopifnot(sig > 0 & sig < 1)
  tail <- match.arg(tail)

  num <- nrow(dset)

  w.i <- sort(dset$wkd, decreasing = FALSE, na.last = FALSE)

  # TODO: keep an eye on how we use w.i,
  #   because if dset's nmc leads us to use Monte Carlo sims,
  #   we may get some permutations equivalent to orig data
  #   i.e. we may get MORE THAN ONE k=0 and therefore several w.i=NaN...
  # I THINK that we can just replace a hardcoded 1 below
  #   with the nr of k=0 rows, but let's re-check this to make sure.
  nk0 <- sum(dset$k == 0)
  stopifnot(nk0 >= 1) # Our code assumes 1st row is orig data, so k=0 at least once

  if (tail == "Left"){
    siglevel <- sig
    index <- ceiling(siglevel*num) - 1
    UB <- w.i[(num-index)]
    LT = c(-Inf, UB)
    return(LT)
  } else if (tail == "Right"){
    siglevel <- sig
    index <- ceiling(siglevel*num) - 1
    LB <- w.i[1+nk0+index] # starts counting from the (1+nk0)'th row of dset
    # (not the first (original) which will always be 'NaN')
    RT = c(LB, Inf)
    return(RT)
  } else { # tail == "Two"
    siglevel <- sig/2  # use half of sig in each tail
    index <- ceiling(siglevel*num) - 1
    UB <- w.i[(num-index)]
    LB <- w.i[1+nk0+index] # starts counting from the (1+nk0)'th row of dset
    # (not the first (original) which will always be 'NaN')
    Upper <- if(is.na(UB)) Inf else UB
    Lower <- if(is.na(LB)) -Inf else LB
    CI = c(Lower, Upper)
    return(CI)
  }

}
