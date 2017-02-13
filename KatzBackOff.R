library(stringr)

#Step 1 Unigram, Bigram and Trigram counts

#Step 1 ii select the brigram and trigram discounts
Gamma <- 0.9

#Step 2. Select the Bigram word to be predicted

inputWords <- "one of"

#Step 3. Calculate the probabilities of words completing observed trigrams

#Probablities where trigram exists

FirstWords <- word(inputWords,-2,-1)

get_qbo_obs_trigrams <- function(inputWords,trigram,Gamma)
{
    FirstWords <- word(inputWords,-2,-1)
    obs_trigs <-  wfTriGram[wfTriGram$FirstWords==FirstWords,] #get freq of the trigram words
    
    qbo_obs_trigrams <- obs_trigs
    #calculate the maximim likelihood 
    qbo_obs_trigrams$pDisc <- (qbo_obs_trigrams$freq-Gamma)/sum(qbo_obs_trigrams$freq)
    
    qbo_obs_trigrams
}

qbo_obs_trigrams <- get_qbo_obs_trigrams(inputWords,wfTriGram,Gamma)

#Step 4. Calculate probabilities of UnObserved Trigrams

#Step 4. i Find unobserved tail words
#if (nrow(qbo_obs_trigrams)!=0) {
unobs_trig_tails <- wfUniGram[!wfUniGram$word %in% qbo_obs_trigrams$LastWord,]
#} else unobs_trig_tails <- wfUniGram

#Step 4. ii Calculate the discounted probability mass at bigram level

#Identify all the observed bigrams
#first get all the bigrams in the prefix to be predicted
LastWord <- word(inputWords,-1,-1)
qbo_obs_bigrams <- wfBigram[wfBigram$FirstWords == LastWord,]
alpha_big <- 1 - sum((qbo_obs_bigrams$freq-Gamma)/sum(qbo_obs_bigrams$freq))
alpha_big

#Step 4. iii Calculate backed off probabilities for bigrams

#First do the observed
if(nrow(qbo_obs_bigrams)!=0){
    qbo_obs_bigrams$pDisc <- (qbo_obs_bigrams$freq - Gamma)/sum(qbo_obs_bigrams$freq)
    qbo_obs_bigrams <- qbo_obs_bigrams[!qbo_obs_bigrams$LastWord == qbo_obs_trigrams$LastWord,]
}
#Now the unobserved

#remove the observed
unobs_trig_tails <- unobs_trig_tails[!unobs_trig_tails$word %in% qbo_obs_bigrams$LastWord,]
unobs_trig_tails$pDisc <- alpha_big*(unobs_trig_tails$freq/sum(unobs_trig_tails$freq))
unobs_trig_tails$FirstWords <- LastWord

names(unobs_trig_tails)[names(unobs_trig_tails)=="word"] <- "LastWord"
unobs_trig_tails <- unobs_trig_tails[,c("FirstWords","LastWord","freq","pDisc")] 

#check the sum of unobserved bigrams equates to the missing probabilty mass
sum(unobs_trig_tails$pDisc)

#Step  4.  iv Calculate the discounted probability mass at trigram level
alpha_trig <- 1 - sum(qbo_obs_trigrams$pDisc)

#Step 4. v Calculate unobserved trigram probabilities
qbo_unobs_trigrams <- rbind(qbo_obs_bigrams,unobs_trig_tails)

qbo_unobs_trigrams$pDisc <- alpha_trig*(qbo_unobs_trigrams$pDisc/sum(qbo_unobs_trigrams$pDisc))

qbo_unobs_trigrams$FirstWords <- FirstWords

finalProb <- rbind(qbo_obs_trigrams,qbo_unobs_trigrams)

head(finalProb,10)