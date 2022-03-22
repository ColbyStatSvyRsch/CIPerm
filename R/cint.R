#' Permutation-methods confidence interval for difference in means
#'
#' Calculate confidence interval for a simple difference in means
#' from a two-sample permutation or randomization test.
#' In other words, we set up a permutation or randomization test to evaluate
#' \eqn{H_0: \mu_A - \mu_B = 0}, then use those same permutations to
#' construct a CI for the parameter \eqn{\delta = (\mu_A - \mu_B)}.
#'
#' If the desired \code{conf.level} is not exactly feasible,
#' the achieved confidence level will be slightly anti-conservative.
#' We use the default numeric tolerance in \code{\link{all.equal}} to check
#' if \code{(1-conf.level) * nrow(dset)} is an integer for one-tailed CIs,
#' or if \code{(1-conf.level)/2 * nrow(dset)} is an integer for two-tailed CIs.
#' If so, \code{conf.level.achieved} will be the desired \code{conf.level}.
#' Otherwise, we will use the next feasible integer,
#' thus slightly reducing the confidence level.
#' For example, in the example below the randomization test has 35 combinations,
#' and a two-sided CI must have at least one combination value in each tail,
#' so the largest feasible confidence level for a two-sided CI is 1-(2/35) or around 94.3\%.
#' If we request a 95\% or 99\% CI, we will have to settle for a 94.3\% CI instead.
#'
#' @param dset The output of \code{\link{dset}}.
#' @param conf.level Confidence level (default 0.95 corresponds to 95\% confidence level).
#' @param tail Which tail? Either "Two"- or "Left"- or "Right"-tailed interval.
#' @return A list containing the following components:\describe{
#'   \item{\code{conf.int}}{Numeric vector with the CI's two endpoints.}
#'   \item{\code{conf.level.achieved}}{Numeric value of the achieved confidence level.}
#' }
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

  # Keeping an eye on how we use w.i later...
  #   When dset's nmc leads us to use Monte Carlo sims,
  #   we may get some permutations equivalent to orig data
  #   i.e. we may get SEVERAL k=0 and therefore several w.i=NaN.
  # For this reason, some hardcoded "1"s below
  #   (which used to be correct when we didn't have a Monte Carlo option)
  #   must be replaced with the number of k=0 rows.
  nk0 <- sum(dset$k == 0)
  stopifnot(nk0 >= 1) # Our code assumes 1st row is orig data, so k=0 at least once

  # Below we'll need to take ceiling(siglevel*num) several times.
  # BUT this has caused problems when siglevel*num "should be" an integer
  # but was represented as sliiightly more than that integer in floating point...
  # So here is an internal function which will EITHER
  # 1. use round(siglevel*num), if siglevel*num is basically an integer,
  #    to within the default numerical tolerance of all.equal();
  # 2. or otherwise use ceiling(siglevel*num) instead.
  roundOrCeiling <- function(x) {
    ifelse(isTRUE(all.equal(round(x), x)), # is x==round(x) to numerical tolerance?
           round(x),
           ceiling(x))
  }
  ## Quick checks:
  ## ceiling() fails to give 2, but roundOrCeiling() works correctly
  # > ceiling((1-(1-2/35))*35) ## whoops!
  # [1] 3
  # > roundOrCeiling((1-(1-2/35))*35)
  # [1] 2
  ## Another check:
  ## ceiling() fails to give 250, but roundOrCeiling() works correctly
  # > ceiling((1-.95)*5000) ## whoops!
  # [1] 251
  # > roundOrCeiling((1-.95)*5000)
  # [1] 250


  if (tail == "Left"){
    siglevel <- sig
    index <- roundOrCeiling(siglevel*num) - 1
    UB <- w.i[(num-index)]
    LT = c(-Inf, UB)
    conf.achieved = 1-((index+1)/num)
    message(paste0("Achieved conf. level: 1-(", index+1, "/", num, ")"))
    return(list(conf.int = LT,
                conf.level.achieved = conf.achieved))
  } else if (tail == "Right"){
    siglevel <- sig
    index <- roundOrCeiling(siglevel*num) - 1
    LB <- w.i[1+nk0+index] # starts counting from the (1+nk0)'th element of w.i
    # (not the first (original) which will always be 'NaN')
    RT = c(LB, Inf)
    conf.achieved = 1-((index+1)/num)
    message(paste0("Achieved conf. level: 1-(", index+1, "/", num, ")"))
    return(list(conf.int = RT,
                conf.level.achieved = conf.achieved))
  } else { # tail == "Two"
    siglevel <- sig/2  # use half of sig in each tail
    index <- roundOrCeiling(siglevel*num) - 1
    UB <- w.i[(num-index)]
    LB <- w.i[1+nk0+index] # starts counting from the (1+nk0)'th element of w.i
    # (not the first (original) which will always be 'NaN')
    Upper <- if(is.na(UB)) Inf else UB
    Lower <- if(is.na(LB)) -Inf else LB
    CI = c(Lower, Upper)
    conf.achieved = 1-(2*(index+1)/num)
    message(paste0("Achieved conf. level: 1-2*(", index+1, "/", num, ")"))
    return(list(conf.int = CI,
                conf.level.achieved = conf.achieved))
  }

}
