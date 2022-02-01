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

cint <- function(data, sig = .05, tail = "Left"){
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
    LB <- data$w.i[1+index]
    RT = c(LB, Inf)
    return(RT)
  } else if (tail == "Two"){
    siglevel <- sig/2
    alpha <- ceiling(solve((1/num), siglevel))
    index <- alpha - 1
    UB <- data$w.i[(num-index)]
    LB <- data$w.i[1+index]
    Upper <- if(is.na(UB)) Inf else UB
    Lower <- if(is.na(LB)) -Inf else LB
    CI = c(Lower, Upper)
    return(CI)
  } else {
    print("No Valid Interval Specified")
  }

}
