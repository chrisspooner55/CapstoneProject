
#function to read in the source data and sample 5 percent using the 
#rbinom function


subSampleText <- function(filePath, outfilePath, subSamplepct){
    temp <- readLines(filePath)
    randLines <- rbinom(length(temp),1,0.999)
    temp <- temp[!randLines]
    
        #replace funny characters
    temp <- gsub("â€™","'",temp)
    temp <- gsub("â€“","-",temp)
    temp <- gsub("â€”","-",temp)    
    temp <- gsub("â€œ"," ",temp)    
    temp <- gsub("â€"," ",temp)     
    
    write(temp, file = outfilePath,1,append=FALSE)
}


#20% could not process TriGrams
#Trying 10%

seed <- 54321

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.twitter.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.twitter_Test.txt"

subSampleText(filePath,outfilePath,subSamplepct)

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.blogs.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.blogs_Test.txt"

subSampleText(filePath,outfilePath,subSamplepct)

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.news.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.news_Test.txt"

subSampleText(filePath,outfilePath,subSamplepct)

testDataset1 <- readLines("C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.twitter_Test.txt")
testDataset2 <- readLines("C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.blogs_Test.txt")
testDataset3 <- readLines("C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.news_Test.txt")

testDataset <- append(testDataset1,testDataset2)
testDataset <- append(testDataset,testDataset3)

#Now to randomly pick 3 words from each line
library(stringr)

wordPredictionTestDataSet <- data.frame(input=as.character())

for(i in 1:length(testDataset)){
    testDataset[i]
    a <- sapply(gregexpr("\\W+", testDataset[i]), length) + 1
        if(a > 4) {
        a <- a - 4
        a <- sample(1:a, replace=T, 1)
        temp <- as.data.frame(word(testDataset[i],a,a+3,sep = fixed(" ")))
        temp
        wordPredictionTestDataSet <- rbind(wordPredictionTestDataSet,temp)
        }
}

colnames(wordPredictionTestDataSet)[1] <- "Input"

wordPredictionTestDataSet <- wordPredictionTestDataSet[!is.na(wordPredictionTestDataSet$Input),]
wordPredictionTestDataSet <- as.data.frame(wordPredictionTestDataSet)





