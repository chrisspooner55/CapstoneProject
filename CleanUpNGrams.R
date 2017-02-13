library(stringr)


#Split the n-grams up nicely, by first words, last words

head(wfTriGram)

class(wfTriGram)

dim(wfTriGram)

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
