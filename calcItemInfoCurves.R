calcItemInfoCurves <- function(questionDataFrame,ids){
  
  questionDataFrame <- questionDataFrame[questionDataFrame$id %in% ids,]
  questions <- split(questionDataFrame,questionDataFrame$id)
  listOfItemIICs <- list()
  
for(q in 1:length(questions)){
  
  a <- questions[[q]]$discrimination
  b <- questions[[q]]$difficulty
  
  itemICC <- data.frame(theta = numeric(),
                        I = numeric())

#iterate through abilities
for(theta in seq(-4,4,.1)){

P <- 1/(1 + exp(-a*(theta - b)))
Q <- 1 - P

I <- P*Q*(a^2)

itemICC <- rbind(itemICC,
                 data.frame(
                   theta=theta,
                   I = I)
                 )

}
  
  listOfItemIICs[[q]] <- itemICC  
  
}
  
  
  #calculate TIC
  library(plyr)
  TIC <- join_all(listOfItemIICs, by="theta")
  TIC$sumI <- rowSums(TIC[,2:ncol(TIC)])
  
  #return list of item IICs, and the TIC data frame
  return(list(IICs = listOfItemIICs,TIC = TIC))
  
}