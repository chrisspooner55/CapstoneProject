#This algorithm uses quad-grams and katz back off


get_qbo_obs_trigrams <- function(inputWords,trigram,Gamma)
{
    LastWords <- word(inputWords,-2,-1)
    
    obs_trigs <-  wfTriGram[wfTriGram$FirstWords==LastWords,] #get freq of the trigram words
    
    qbo_obs_trigrams <- obs_trigs
    #calculate the maximim likelihood 
    qbo_obs_trigrams$pDisc <- (qbo_obs_trigrams$freq-Gamma)/sum(qbo_obs_trigrams$freq)
    
    if(nrow(qbo_obs_trigrams)!=0){   
        qbo_obs_trigrams$source <- "Trigrams"    
    }
    
    qbo_obs_trigrams
}

get_qbo_obs_quadgrams <- function(inputWords,quadgram,Gamma)
{

    qbo_obs_quadgrams <-  wfQuadGram[wfQuadGram$FirstWords==LastWords,] #get freq of the Quadgram words
    
    #calculate the maximim likelihood 
    qbo_obs_quadgrams$pDisc <- (qbo_obs_quadgrams$freq-Gamma)/sum(qbo_obs_quadgrams$freq)
    if(nrow(qbo_obs_quadgrams)!=0){
        qbo_obs_quadgrams$source <- "Quadgrams"
    }
    
    qbo_obs_quadgrams
}

get_cleanInput <- function(inputWords){
    #change to lowercase
    outputWords <- tolower(inputWords)
    #remove punctuation [!"\#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~]
    outputWords <- gsub('[[:punct:]]','',outputWords) 
    #remove numbers
    outputWords <- gsub('[[:digit:]]','', outputWords)
    #trim white space
    outputWords <- trimws(outputWords, which = "both")
    
    outputWords
}


get_TriGramKatzBO <- function(inputWords){
    #This algorithm uses Katz back off model for trigrams and uses a default discount of 0.5    
    
    library(stringr)
    
    Gamma <- 0.5
    
    #Get the last two words
    LastWords <- word(inputWords,-2,-1)
    
    #Get the observed trigrams with their maximum likelihood discounted
    
    #TODO - apply the discounting
    
    qbo_obs_trigrams <- get_qbo_obs_trigrams(inputWords,wfTriGram,Gamma)
    
    #Find unobserved tail words
    unobs_trig_tails <- wfUniGram[!wfUniGram$word %in% qbo_obs_trigrams$LastWord,]
    
    if(nrow(unobs_trig_tails)!=0){
        unobs_trig_tails$source <- "Unigrams"
    }
    
    LastWord <- word(inputWords,-1,-1)
    
    #Find the observed bigrams and remove the ones already observed
    qbo_obs_bigrams <- wfBigram[wfBigram$FirstWords == LastWord,]
    qbo_obs_bigrams <- qbo_obs_bigrams[!qbo_obs_bigrams$LastWord %in% qbo_obs_trigrams$LastWord,]
    
    if(nrow(qbo_obs_bigrams)!=0){
        qbo_obs_bigrams$source <- "Bigrams"
    }
    
    #Get the alpha or left over probablity mass at the bigram level
    
    #TODO - apply the discounting
    
    alpha_bigram <- 1 - sum((qbo_obs_bigrams$freq-Gamma)/sum(qbo_obs_bigrams$freq))
    #alpha_bigram
    
    #Calculate backed off probabilities for bigrams
    if(nrow(qbo_obs_bigrams)!=0){
        qbo_obs_bigrams$pDisc <- (qbo_obs_bigrams$freq - Gamma)/sum(qbo_obs_bigrams$freq)
    }
    
    #remove the observed tails in the observed bigrams
    unobs_trig_tails <- unobs_trig_tails[!unobs_trig_tails$word %in% qbo_obs_bigrams$LastWord,]
    
    #Calculate the probablity of the tails with the proportioned left over probablity mass
    unobs_trig_tails$pDisc <- alpha_bigram*(unobs_trig_tails$freq/sum(unobs_trig_tails$freq))
    
    #Add in the last input word to the tails so it can be binded to the other data frames
    if(nrow(unobs_trig_tails)!=0){
        unobs_trig_tails$FirstWords <- LastWord
        names(unobs_trig_tails)[names(unobs_trig_tails)=="word"] <- "LastWord"
        unobs_trig_tails <- unobs_trig_tails[,c("FirstWords","LastWord","freq","pDisc","source")] 
    }
    
    #check the sum of unobserved bigrams equates to the missing probabilty mass
    #sum(unobs_trig_tails$pDisc)
    
    alpha_trigam <- 1 - sum(qbo_obs_trigrams$pDisc)
    
    qbo_unobs_trigrams <- rbind(qbo_obs_bigrams,unobs_trig_tails)
    
    qbo_unobs_trigrams$pDisc <- alpha_trigam*(qbo_unobs_trigrams$pDisc/sum(qbo_unobs_trigrams$pDisc))
    
    qbo_unobs_trigrams$FirstWords <- LastWords
    
    finalProb <- rbind(qbo_obs_trigrams,qbo_unobs_trigrams)
    
    head(finalProb,5)
    
}


wordPrediction <- function(inputWords){
    
    #Example call
    #wordPrediction("jes fds rsadsa")    
    
    inputWords = get_cleanInput(inputWords)
    
    library(stringr)
    
    if(str_count(inputWords, pattern = " ") + 1 >=3) {
        #Get the last 3 words
        LastWords <- word(inputWords,-3,-1)
    } else {
        LastWords <- word(inputWords,-2,-1)
    }
    
    Gamma <- 0.5
    
    qbo_obs_quadgrams <- get_qbo_obs_quadgrams(inputWords = LastWords,quadgram = wfQuadGram,Gamma = 0.5)
    
    x <- nrow(qbo_obs_quadgrams)
    
    if (x == 0) {
        #none observed at the Quadgram level so back off to the TriGram
        get_TriGramKatzBO(LastWords)
    } else {
        
        #Find the observed bigrams and remove the ones already observed
        qbo_obs_trigrams <- wfTriGram[wfTriGram$FirstWords == word(inputWords,-2,-1),]
        qbo_obs_trigrams <- qbo_obs_trigrams[!qbo_obs_trigrams$LastWord %in% qbo_obs_quadgrams$LastWord,]
        
        if(nrow(qbo_obs_trigrams)!=0){
            qbo_obs_trigrams$source <- "Trigrams"
        }
        #Calculate backed off probabilities for trigrams
        if(nrow(qbo_obs_trigrams)!=0){
            qbo_obs_trigrams$pDisc <- (qbo_obs_trigrams$freq - Gamma)/sum(qbo_obs_trigrams$freq)
        }   
        
        #Find unobserved tail bigrams, we won't go lower to unigrams
        unobs_quad_tails <- wfBigram[!wfBigram$LastWord %in% qbo_obs_quadgrams$LastWord 
                                     & wfBigram$FirstWords %in% word(qbo_obs_quadgrams$FirstWords,-1),]
        
        if(nrow(unobs_quad_tails)!=0){
            unobs_quad_tails$source <- "Bigrams"
        }
        
        #remove the observed tails in the observed trigrams
        unobs_quad_tails <- unobs_quad_tails[!unobs_quad_tails$LastWord %in% qbo_obs_trigrams$LastWord
                                             & unobs_quad_tails$FirstWords %in% word(qbo_obs_trigrams$FirstWords,-1),]
        
        #Get the alpha or left over probablity mass at the bigram level
        
        #TODO - apply the discounting
        
        alpha_trigram <- 1 - sum((qbo_obs_trigrams$freq-Gamma)/sum(qbo_obs_trigrams$freq))
        
        #Calculate the probablity of the tails with the proportioned left over probablity mass
        unobs_quad_tails$pDisc <- alpha_trigram*(unobs_quad_tails$freq/sum(unobs_quad_tails$freq))
        
        alpha_quadgram <- 1 - sum(qbo_obs_quadgrams$pDisc)    
        
        qbo_unobs_quadgrams <- rbind(qbo_obs_trigrams,unobs_quad_tails)
        
        qbo_unobs_quadgrams$pDisc <- alpha_quadgram*(qbo_unobs_quadgrams$pDisc/sum(qbo_unobs_quadgrams$pDisc))
        
        finalProb <- rbind(qbo_obs_quadgrams,qbo_unobs_quadgrams)  
        
        head(finalProb,5)
    }
    
}

#Refer to http://stats.stackexchange.com/questions/91581/question-about-good-turing-discounting?rq=1
#for a good explanation of good turing

calculateDiscount <- function (Ngrams,unseenNgrams) {
    
    # Supposed table "threeGramTable" as above, we want to add a "discount" column.
    
    #add discount column and set to 1
    Ngrams$discount = rep(1, nrow(Ngrams))
    
    # Calculate the discount coefficient.
    # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
    
    #N is the total number of unseen N-Grams
    N = sum(Ngrams[Ngrams$freq < 5,c("freq")]) + nrow(unseenNgrams)
    
    for(i in 1:5){
        #start at i = 1 as we removed all frequencies of 1    
        currRTimes = i 
        nextRTimes = currRTimes + 1
        
        if(i==1)
            currN = nrow(unseenNgrams)
        else
            currN = nrow(Ngrams[Ngrams$freq == currRTimes,])
        
        nextN = nrow(Ngrams[Ngrams$freq == nextRTimes,])
        
        currd <- nextN/N*currN
        
        Ngrams[Ngrams$freq == currRTimes,"discount"] <- currd
    }
    
    Ngrams
}

