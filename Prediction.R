#Prediction

predict<-function(str){
  str<-tolower(str)
  words<-strsplit(str,' ')[[1]]
  words<-words[words!=""]
  numberOfWords<-length(words)
  if(numberOfWords==0){
    finalPrediction<-c("The", "I", "Yes")
    return(finalPrediction)
  }
  
  #Take last 3 words
  if(numberOfWords>3){
    words<-c(words[numberOfWords-2],words[numberOfWords-1],words[numberOfWords])
    numberOfWords<-length(words)
  }
    
  if(numberOfWords==3){
    ##Check for 4Gram
    input<-words
    predictedWords<-fourGramPrediction(input)
    
    ##Backoff to 3Gram
    if(nrow(predictedWords)==0){
      input<-c(words[2],words[3])
      predictedWords<-threeGramPrediction(input)      
    }
    ##Backoff to 2Gram
    if(nrow(predictedWords)==0){
      input<-c(words[3])
      predictedWords<-twoGramPrediction(input)      
    }
  }
  else if(numberOfWords==2){
    ##Check for 3Gram
    input<-c(words[1],words[2])
    predictedWords<-threeGramPrediction(input)      
    ##Backoff to 2Gram
    if(nrow(predictedWords)==0){
      input<-c(words[2])
      predictedWords<-twoGramPrediction(input)      
    }
  }
  else if(numberOfWords==1){
    ##Check for 2Gram
    input<-c(words[1])
    predictedWords<-twoGramPrediction(input)  
  }
  
  if(nrow(predictedWords)>0){
    if(numberOfWords==3)
      finalPrediction<-interpolationWithThreeWords(words,predictedWords)
    else if(numberOfWords==2)
      finalPrediction<-interpolationWithTwoWords(words,predictedWords)
    else if(numberOfWords==1)
      finalPrediction<-interpolationWithOneWord(words,predictedWords)
    
    if(length(finalPrediction)==1){
      finalPrediction[2:3]<-""
    }
    else if(length(finalPrediction)==2){
      finalPrediction[3]<-""
    }
    finalPrediction
  }
  else{
    finalPrediction<-c("is", "and", "are")
    finalPrediction
  }
}

#4Gram prediction
fourGramPrediction<-function(input){
  predictedWords<-fourWordTable[Token1==input[1] & Token2==input[2] & Token3==input[3],]
  if(nrow(predictedWords)>3)
    predictedWords<-predictedWords[1:3]
  predictedWords
}

#3Gram Prediction
threeGramPrediction<-function(input){
  predictedWords<-threeWordTable[Token1==input[1] & Token2==input[2],]
  if(nrow(predictedWords)>3)
    predictedWords<-predictedWords[1:3]
  predictedWords
}

#2Gram Prediction
twoGramPrediction<-function(input){
  predictedWords<-twoWordTable[Token1==input[1],]
  if(nrow(predictedWords)>3)
    predictedWords<-predictedWords[1:3]
  predictedWords
}

#4Gram Word Probability
fourGramProbability<-function(input,candidate){
  probability<-fourWordTable[Token1==input[1] & Token2==input[2] & Token3==input[3] & Prediction==candidate,Probability]
  if(length(probability)==0)
    probability<-0
  probability
}

#3Gram Word Probability
threeGramProbability<-function(input,candidate){
  probability<-threeWordTable[Token1==input[1] & Token2==input[2] & Prediction==candidate,Probability]
  if(length(probability)==0)
    probability<-0
  probability
}

#2Gram Word Probability
twoGramProbability<-function(input,candidate){
  probability<-twoWordTable[Token1==input[1] & Prediction==candidate,Probability]
  if(length(probability)==0)
    probability<-0
  probability
}

#1Gram Word Probability
oneGramProbability<-function(candidate){
  probability<-oneWordTable[Token1==candidate,Probability]
  if(length(probability)==0)
    probability<-0
  probability
}

#Linear Interpolation With Three Input Words
interpolationWithThreeWords<-function(inputWords,predictedWords){
  
  input1<-c(inputWords[1],inputWords[2],inputWords[3])
  input2<-c(inputWords[2],inputWords[3])
  input3<-c(inputWords[3])
  candidates<-predictedWords$Prediction
  
  i<-1
  
  if(ncol(predictedWords)==5){
    finalProbabilities<-lapply(candidates, function(candidate){
      prob1<-predictedWords$Probability[i]
      prob2<-threeGramProbability(input2,candidate)
      prob3<-twoGramProbability(input3,candidate)
      prob4<-oneGramProbability(candidate)
      i<-i+1
      (0.25 * (prob1 + prob2 + prob3 + prob4))
    })  
  }
  
  else if(ncol(predictedWords)==4){
    finalProbabilities<-lapply(candidates, function(candidate){
      prob1<-0
      prob2<-predictedWords$Probability[i]
      prob3<-twoGramProbability(input3,candidate)
      prob4<-oneGramProbability(candidate)
      i<-i+1
      (0.25 * (prob1 + prob2 + prob3 + prob4))
    })  
  }
  
  else if(ncol(predictedWords)==3){
    finalProbabilities<-lapply(candidates, function(candidate){
      prob1<-0
      prob2<-0
      prob3<-predictedWords$Probability[i]
      prob4<-oneGramProbability(candidate)
      i<-i+1
      (0.25 * (prob1 + prob2 + prob3 + prob4))
    })  
  }
  
  result<-vector(mode = "character",length = 3)
  finalProbabilities<-as.numeric(finalProbabilities)
  result<-candidates[order(finalProbabilities,decreasing=T)]
  result
}

#Linear Interpolation With Two Input Words
interpolationWithTwoWords<-function(inputWords,predictedWords){
  
  input1<-c(inputWords[1],inputWords[2])
  input2<-c(inputWords[2])
  candidates<-predictedWords$Prediction
  
  i<-1
  
  if(ncol(predictedWords)==4){
    finalProbabilities<-lapply(candidates, function(candidate){
      prob1<-predictedWords$Probability[i]
      prob2<-twoGramProbability(input2,candidate)
      prob3<-oneGramProbability(candidate)
      i<-i+1
      (0.33 * (prob1 + prob2 + prob3))
    })  
  }
  
  else if(ncol(predictedWords)==3){
    finalProbabilities<-lapply(candidates, function(candidate){
      prob1<-0
      prob2<-predictedWords$Probability[i]
      prob3<-oneGramProbability(candidate)
      i<-i+1
      (0.33 * (prob1 + prob2 + prob3))
    })  
  }
  
  result<-vector(mode = "character",length = 3)
  finalProbabilities<-as.numeric(finalProbabilities)
  result<-candidates[order(finalProbabilities,decreasing=T)]
  result
}

#Linear Interpolation With One Input Word
interpolationWithOneWord<-function(inputWords,predictedWords){ 
  input1<-c(inputWords[1])
  candidates<-predictedWords$Prediction
  
  i<-1
  
  finalProbabilities<-lapply(candidates, function(candidate){
    prob1<-predictedWords$Probability[i]
    prob2<-oneGramProbability(candidate)
    i<-i+1
    (0.50 * (prob1 + prob2))
  })
  
  result<-vector(mode = "character",length = 3)
  finalProbabilities<-as.numeric(finalProbabilities)
  result<-candidates[order(finalProbabilities,decreasing=T)]
  result
}

##suppressWarnings(predict("would mean the"))