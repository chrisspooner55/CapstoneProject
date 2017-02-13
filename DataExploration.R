rm(list=ls())
#Folowing the text mining method found on:
#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

#subsets of the data have been placed in C:\Projects\R Projects\Capstone Datascience\CapstoneProject\texts

library(SnowballC) 
library(tm)  
library(dplyr)
library(ggplot2)   
library(wordcloud)   
library(RWeka)
library(stringr)

cname <- file.path("C:\\Projects\\R Projects\\Capstone Datascience\\CapstoneProject", "texts")   
docs <- Corpus(DirSource(cname))   

#preprocessing
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, tolower)   

#remove profanities
#list of words obtained from 
#https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words

profanity <- read.csv(file = "DirtyWord_en.txt",header = FALSE,col.names = c("DirtyWord"))
profanity <- profanity$DirtyWord
docs <- tm_map(docs, removeWords, profanity)  

#docs <- tm_map(docs, stemDocument) 
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument)   

#Convert to matrix, this takes a while to run
dtmUniGram <- DocumentTermMatrix(docs)   

freqUniGram <- sort(colSums(as.matrix(dtmUniGram)), decreasing=TRUE)   

wfUniGram <- data.frame(word=names(freqUniGram), freq=freqUniGram)   

## set the levels in order we want
wfUniGram$word = factor(wfUniGram$word, levels = wfUniGram$word[order(-wfUniGram$freq)])

p <- ggplot(head(wfUniGram,20), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

wfUniGram$CumulativePct <- mutate(wfUniGram, cumsum=cumsum(Pct))


#Number of words making up 50 percent of the Corpus
nrow(wfUniGram[wfUniGram$CumulativePct$cumsum < 50,])

#Number of words making up 90 percent of the Corpus
nrow(wfUniGram[wfUniGram$CumulativePct$cumsum < 90,])

##Let's now create a bi-gram
BigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 2, max = 2))
# Using Tyler's method of making the 'Text' object here
dtm_BiGram <- DocumentTermMatrix(docs, 
                                 control = list(tokenize = BigramTokenizer))

freqBiGram <- sort(colSums(as.matrix(dtm_BiGram)), decreasing=TRUE) 

wfBigram <- data.frame(word=names(freqBiGram), freq=freqBiGram)   

## set the levels in order we want
wfBigram$word = factor(wfBigram$word, levels = wfBigram$word[order(-wfBigram$freq)])

p <- ggplot(head(wfBigram,20), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

##Let's now create a tri-gram
TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 3, max = 3))

dtm_TriGram <- DocumentTermMatrix(docs, 
                                 control = list(tokenize = TrigramTokenizer))

freqTriGram <- sort(colSums(as.matrix(dtm_TriGram)), decreasing=TRUE) 

wfTriGram <- data.frame(word=names(freqTriGram), freq=freqTriGram)   

## set the levels in order we want
wfTriGram$word = factor(wfTriGram$word, levels = wfTriGram$word[order(-wfTriGram$freq)])

p <- ggplot(head(wfTriGram,20), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

##Let's now create a quad-gram
QuadgramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 4, max = 4))

dtm_QuadGram <- DocumentTermMatrix(docs, 
                                  control = list(tokenize = QuadgramTokenizer))

freqQuadGram <- sort(colSums(as.matrix(dtm_QuadGram)), decreasing=TRUE) 

wfQuadGram <- data.frame(word=names(freqQuadGram), freq=freqQuadGram)   

## set the levels in order we want
wfQuadGram$word = factor(wfQuadGram$word, levels = wfQuadGram$word[order(-wfQuadGram$freq)])

##Clean up the data frames

#get rid of the row names
row.names(wfTriGram) <- NULL
wfTriGram$LastWord <- word(wfTriGram$word,-1)
wfTriGram$FirstWords <- word(wfTriGram$word,-3,-2)

#Redefine the data frame to make things easier to work with
wfTriGram <- wfTriGram[,c("FirstWords","LastWord","freq")]


#get rid of the row names
row.names(wfQuadGram) <- NULL
wfQuadGram$LastWord <- word(wfQuadGram$word,-1)
wfQuadGram$FirstWords <- word(wfQuadGram$word,-4,-2)

#Redefine the data frame to make things easier to work with
wfQuadGram <- wfQuadGram[,c("FirstWords","LastWord","freq")]

#get rid of the row names
row.names(wfBigram) <- NULL
wfBigram$LastWord <- word(wfBigram$word,-1)
wfBigram$FirstWords <- word(wfBigram$word,-2)

#Redefine the data frame to make things easier to work with
wfBigram <- wfBigram[,c("FirstWords","LastWord","freq")]

#get rid of the row names
row.names(wfUniGram) <- NULL

#Save data for knitR markdown

save(wfUniGram,file="wfUniGram.RData")
save(dtmUniGram,file="dtmUniGram.RData")

save(wfBigram,file="wfBiGram.RData")
save(dtm_BiGram,file="dtmBiGram.RData")

save(wfTriGram,file="wfTriGram.RData")
save(dtm_TriGram,file="dtmTriGram.RData")

save(wfQuadGram,file="wfQuadGram.RData")
save(dtm_QuadGram,file="dtmQuadGram.RData")
