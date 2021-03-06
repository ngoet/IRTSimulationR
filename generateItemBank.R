#custom function for generating item bank
#model can be 2PL or 3PL, takes default 2PL
generateItembank <- function(nItems, maxOptions, model){
  
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  #check entered parameters
  if(!is.wholenumber(nItems)){
    stop("Error. Please enter an integer value for the nItems variable.")
    
  }
  
  if(!is.wholenumber(maxOptions) | maxOptions < 1){
    stop("Error. Please enter a maxOptions between 1 and infinity.")
    
  }
  
  if(!model %in% c("2PL","3PL")){
    stop("Error. Please enter a valid option for the model parameter (2PL/3PL)")
    
  }
  
  #specify discrimination parameter 
  a1 <- rlnorm(nItems, .2,.2)
  
  numBParam <- maxOptions - 1
  
  if(model == "2PL" | is.null(model)){
    
    #initialise item bank
    itemBank <- data.frame(matrix(ncol = numBParam, nrow = nItems))  
    
    for(i in 1:numBParam){
      itemBank[,i] <- rnorm(nItems)
      
    }
    
    #add discrimination parameter
    library(tibble)
    itemBank <- add_column(itemBank, a1, .before = "X1")
    
    #change column names
    colnames(itemBank) <- gsub("X","b",colnames(itemBank))
    
  }
  
  if(model == "3PL"){
    
    #initialise item bank 
    itemBank <- data.frame(matrix(ncol = numBParam, nrow = nItems))
    
    for(i in 1:numBParam){
      itemBank[,i] <- rnorm(nItems)
      
    }
    
    #add guessing
    g <- rbeta(nItems, 20, 80)
    
    #add discrimination parameter
    library(tibble)
    itemBank <- add_column(itemBank, a1, .before = "X1")
    
    #change column names
    colnames(itemBank) <- gsub("X","b",colnames(itemBank))
    
  } 
  
 
  
  #randomly insert NA as per maxOptions if maxOptions > 1
  if(maxOptions > 1){
    
    #go through each b column (starting with the last) and assign NAs
    #identify b columns
    bColumns <- sort(grep("b",colnames(itemBank)),decreasing=T)
    bColumns <- bColumns[bColumns!=min(bColumns)]
    
    for (column in bColumns){
      
      B <- itemBank[,column]
      newB <- unlist(lapply(B, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
      
      #assign appropriate place in item bank
      itemBank[,column] <- newB
      
      #update item bank
      itemBank[is.na(itemBank[,column]), !colnames(itemBank) %in% c(paste("b",as.numeric(gsub("[^\\d]+","", colnames(itemBank)[column], perl=TRUE))-1,sep=""),"b1","g","a1")] <- NA
      
    }
  }
  
  return(itemBank = itemBank)
}
