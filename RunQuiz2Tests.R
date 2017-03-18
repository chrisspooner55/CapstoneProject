library(stringr)

#Test cases for algorithm based on the quizzes

testCases <- read.delim(file = "Quiz1Input.txt",header = FALSE)

testCases[] <- lapply(testCases, as.character)

colnames(testCases) <- c("InputText")

testCases$Expected = ""

testCases$Expected[1] = "beer"
testCases$Expected[2] = "most"
testCases$Expected[3] = "most"
testCases$Expected[4] = "defence"
testCases$Expected[5] = "movie"
testCases$Expected[6] = "way"
testCases$Expected[7] = "time"
testCases$Expected[8] = "fingers"
testCases$Expected[9] = "bad"
testCases$Expected[10] = "insane"


for(i in 1:10){
    startTime = NULL
    endTime = NULL
    currentTest <- tolower(testCases[i,c("InputText")])
    
    print(currentTest)
    
    startTime <- Sys.time()
    
    results <- wordPrediction(currentTest)
    
    endTime <- Sys.time()
    
    testCases$DurationV1[i] <- as.numeric(difftime(endTime,startTime))
    
    x <- nrow(results[testCases$Expected[i] %in% results$LastWord,]) 
    
    if(x > 0) {
        testCases$ResultV1[i] <- "ExpectedFound"
        
    } else {
        testCases$ResultV1[i] <- "NotFound"
    }
}


testCases


wordPredictionTestDataSet$ResultV2 = ""

n <- 100

for(i in 1:n) {
    
    currentTest <- wordPredictionTestDataSet[i,c("Input")]
    currentTest <- word(currentTest,-4,-2)
    
    print(paste(i,currentTest))
    
    startTime <- Sys.time()

    results <- wordPrediction(currentTest)
 
    endTime <- Sys.time()
    
    #Checks if in top 5
    x <- nrow(results[word(wordPredictionTestDataSet$Input[i],-1) %in% results$LastWord,]) 
    #Checks if 1st one matches
    y <- nrow(results[word(wordPredictionTestDataSet$Input[i],-1) == head(results$LastWord,1),]) 
    
    wordPredictionTestDataSet$DurationV1[i] <- as.numeric(difftime(endTime,startTime))
    
    wordPredictionTestDataSet$PredictionResult[i] <-  head(results$LastWord,1)
    
   # y <- results[word(wordPredictionTestDataSet$Input[i],-1) = results$LastWord,])
    
    if(x > 0) {
        wordPredictionTestDataSet$ResultV2[i] <- "ExpectedFound"
        ##wordPredictionTestDataSet$Position[i] <- y
        
    } else {
        wordPredictionTestDataSet$ResultV2[i] <- "NotFound"
    }    
    
    if(y > 0) {
        wordPredictionTestDataSet$ExactMatch[i] <- "Match"
        ##wordPredictionTestDataSet$Position[i] <- y
        
    } else {
        wordPredictionTestDataSet$ExactMatch[i] <- "NoMatch"
    }       
    
}

wordPredictionTests <- wordPredictionTestDataSet[1:n,]
#Orediction rate
print(paste("Top 5 Prediction Rate:"
            ,nrow(wordPredictionTests[wordPredictionTests$ResultV2=="ExpectedFound",])/nrow(wordPredictionTests)*100,"%"))
print(paste("Top 1 Prediction Rate:"
            ,nrow(wordPredictionTests[wordPredictionTests$ExactMatch=="Match",])/nrow(wordPredictionTests)*100,"%"))
print(paste("Average run time:",mean(wordPredictionTests$DurationV1),"ms"))
print(paste("Min run time:",min(wordPredictionTests$DurationV1),"ms"))
print(paste("Max run time:",max(wordPredictionTests$DurationV1),"ms"))
print(paste("Sum run time:",sum(wordPredictionTests$DurationV1),"ms"))



