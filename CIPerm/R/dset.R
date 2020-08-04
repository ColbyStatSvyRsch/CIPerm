# dset function

# IDK what else I'm supposed to put...



dset <- function(group1, group2){
  # creates the dataset referenced in pval and cint
  combined <- c(group1, group2)

  n <- length(group1)
  m <- length(group2)
  N <- n + m
  num <- choose(N, n)
  den <- (1/n + 1/m)
  dcombn <- combn(1:N, n)

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

  for (ii in 1:num){
    g1ind = dcombn[,ii]
    g2ind = (1:N)[-g1ind]

    group1_perm[,ii] <- combined[g1ind]
    group2_perm[,ii] <- combined[g2ind]

    dataframe$diffmean[ii] = round(mean(combined[g1ind]) - mean(combined[g2ind]), digits = 2)
    dataframe$sum1[ii] = sum(combined[g1ind])
    dataframe$diffmedian[ii] = median(combined[g1ind]) - median(combined[g2ind])

    r <- rank(combined, ties.method = "first")
    rsum <- sum(r[g1ind])
    dataframe$wilsum[ii] = rsum
    dataframe$k[ii] = sum(!(g1ind %in% 1:n))
    dataframe$wkd[ii] = round((dataframe$diffmean[1] - dataframe$diffmean[ii]) / (dataframe$k[ii] * den), digits = 2)
  }

  w.i <- sort(dataframe$wkd, decreasing = FALSE, na.last = FALSE)
  ID <- c(1:num)
  dataset <- t(rbind(group1_perm, group2_perm))
  datatable <- cbind(ID, dataset, dataframe, w.i)
  return(datatable)
}
