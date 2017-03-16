
#function to read in the source data and sample 5 percent using the 
#rbinom function
subSampleText <- function(filePath, outfilePath, subSamplepct){
    temp <- readLines(filePath)
    randLines <- rbinom(length(temp),1,0.95)
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

seed <- 12345

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.twitter.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.twitter_Subset.txt"

subSampleText(filePath,outfilePath,subSamplepct)

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.blogs.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.blogs_Subset.txt"

subSampleText(filePath,outfilePath,subSamplepct)

filePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\en_US\\en_US.news.txt"
outfilePath <- "C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject\\texts\\en_US.news_Subset.txt"

subSampleText(filePath,outfilePath,subSamplepct)