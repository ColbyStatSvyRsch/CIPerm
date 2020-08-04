# pval function

# IDK what I'm supposed to put here


pval <- function(data, tail = "Left", value = "m"){

  num <- nrow(data)

  if (tail == "Left"){
    pvalmean = sum(data$diffmean <= data$diffmean[1])/num
    pvalsum = sum(data$sum1 <= data$sum1[1])/num
    pvalmedian = sum(data$diffmedian <= data$diffmedian[1])/num
    pvalwilsum = sum(data$wilsum <= data$wilsum[1])/num
  } else if (tail == "Right") {
    pvalmean = sum(data$diffmean > data$diffmean[1])/num
    pvalsum = sum(data$sum1 > data$sum1[1])/num
    pvalmedian = sum(data$diffmedian > data$diffmedian[1])/num
    pvalwilsum = sum(data$wilsum > data$wilsum[1])/num
  } else if (tail == "Two") {
    pvalmean <- sum(abs(data$diffmean - mean(data$diffmean)) >= abs(data$diffmean[1] - mean(data$diffmean)))/num
    pvalsum <- sum(abs(data$sum1 - mean(data$sum1)) >= abs(data$sum1[1] - mean(data$sum1)))/num
    pvalmedian <- sum(abs(data$diffmedian - mean(data$diffmedian)) >= abs(data$diffmedian[1] - mean(data$diffmedian)))/num
    pvalwilsum <- sum(abs(data$wilsum - mean(data$wilsum)) >= abs(data$wilsum[1] - mean(data$wilsum)))/num
  } else {
    print("No Valid Tail Specified")
  }

  if (value == "m"){
    return(pvalmean)
  } else if (value == "s"){
    return(pvalsum)
  } else if (value == "d"){
    return(pvalmedian)
  } else if (value == "w"){
    return(pvalwilsum)
  } else{
    print("No Valid P-Value Requested")
  }

}
