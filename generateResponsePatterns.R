#Code for generating responses to a test

#takes:
#itemBank = a data frame object that contains either 2 parameters or three parameters
#numQuestions = the number of questions to be sampled
#numExaminees = the number of examinees for which a response pattern should be simulated


generateResponsePatterns <- function(itemBank,numQuestions,numExaminees){
  
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  if(!is.wholenumber(numQuestions)){
    stop("Error. Please enter an integer for numQuestions")
  }
  
  if(!is.wholenumber(numTests)){
    stop("Error. Please enter an integer for numTests")
  }
  
  if(!is.data.frame(itemBank)){
    stop("Error. Please use a dataframe object for this function")
  }
  
  if("g" %in% colnames(itemBank)){
    model = "3PL"
  }else{
    model = "2PL"
  }
  
  #we sample a number of questions from the item bank
  test <- itemBank[rownames(itemBank) %in% sample(rownames(itemBank),numQuestions),]
  
  #initialise response patterns data frame
  #responsePatterns <- data.frame(matrix(nrow=numQuestions*numExaminees,ncol=3))
  #colnames(responsePatterns) <- c("persID","questionID","correctResponse")
  responsePatterns <- data.frame(persID=integer(), questionID = integer(), correctResponse = integer())
    
  #Sample from a normally distributed theta distribution which informs the probability of i) answering correctly; ii) not answering
  set.seed(1234)
  abilityDist <- rnorm(1000,0,1)
  
  #loop over examinees and generate response patter
  for(examinee in 1:numExaminees){
    
    #generate a theta for this candidate from the distribution
    candAbility <- sample(abilityDist,1)
    
    candDistributionFunction <- pnorm(candAbility)
    print(candDistributionFunction)
  
    #loop over the questions
    for(question in 1:nrow(test)){
      #identify number of options for this question (equals number of b parameters plus 1)
      options <- length(test[question,][grep("b",colnames(test))][!is.na(test[question,][grep("b",colnames(test))])]) + 1
    
      #generate a probability for responding, dependent on theta and difficulty
      difficulty <- mean(test[question,][grep("b",colnames(test))][!is.na(test[question,][grep("b",colnames(test))])],na.rm=T)
      
      samplingProb <- abs(((1/options)*candDistributionFunction)/pnorm(difficulty))
      print(samplingProb)
      
      #increase prob of correct response (50 perc. chance of correct guess)
      samplingProb <- samplingProb+0.5
      
      if(samplingProb > 1){
        samplingProb <- 1
      }
      
      response <- rbinom(1, 1, samplingProb)
      
      #replace with NA on the basis of theta and a .05 probability
      response <- ifelse(rbinom(1,1,0.85),response,NA)
 
      responsePatterns <- rbind(responsePatterns,data.frame(persID=examinee, questionID=question, correctResp=response))
      
    }
    
  }
  
 return(responsePatterns) 
}