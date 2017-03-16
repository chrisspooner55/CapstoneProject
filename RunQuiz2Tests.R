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
    
    results <- wordPredictionV1(currentTest)
    
    endTime <- Sys.time()
    
    testCases$DurationV1 <- as.numeric(difftime(endTime,startTime))
    
    x <- nrow(results[testCases$Expected[i] %in% results$LastWord,]) 
    
    if(x > 0) {
        testCases$ResultV1[i] <- "ExpectedFound"
        
    } else {
        testCases$ResultV1[i] <- "NotFound"
    }
}

#Prediction2
for(i in 1:10){
    startTime = NULL
    endTime = NULL
    currentTest <- tolower(testCases[i,c("InputText")])
    
    print(currentTest)
    
    startTime <- Sys.time()
    
    results <- wordPredictionV2(currentTest)
    
    endTime <- Sys.time()
    
    testCases$DurationV2 <- as.numeric(difftime(endTime,startTime))
    
    x <- nrow(results[testCases$Expected[i] %in% results$LastWord,]) 
    
    if(x > 0) {
        testCases$ResultV2[i] <- "ExpectedFound"
        
    } else {
        testCases$ResultV2[i] <- "NotFound"
    }
}

testCases

input = "adore the stats"
wordPredictionV2(tolower(input))

wordPredictionTestDataSet$ResultV2 <- ""

for(i in 1:nrow(wordPredictionTestDataSet)) {
    
    currentTest <- word(tolower(wordPredictionTestDataSet[i,c("Input")]),-4,-2)

    results <- wordPredictionV2(currentTest)
 
    x <- nrow(results[word(wordPredictionTestDataSet$Input[i],-1) %in% results$LastWord,]) 
    
    
   # y <- results[word(wordPredictionTestDataSet$Input[i],-1) = results$LastWord,])
    
    if(x > 0) {
        wordPredictionTestDataSet$ResultV2[i] <- "ExpectedFound"
        ##wordPredictionTestDataSet$Position[i] <- y
        
    } else {
        wordPredictionTestDataSet$ResultV2[i] <- "NotFound"
    }    
    
}
#Orediction rate
nrow(wordPredictionTestDataSet[wordPredictionTestDataSet$ResultV2=="ExpectedFound",])/nrow(wordPredictionTestDataSet)*100

#fails with a capital letter?
wordPredictionV2("This is the")
wordPredictionV2("this is the")
wordPredictionV2("he needs to")
wordPredictionV2("the dog is")
wordPredictionV2("a case of")
wordPredictionV2("click predict to")


