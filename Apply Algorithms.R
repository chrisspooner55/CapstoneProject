
wordPredictionV1 <- function(inputWords){


library(stringr)

Gamma <- 0.5

#Get the last two words
FirstWords <- word(inputWords,-2,-1)

#Get the observed trigrams with their maximum likelihood discounted

#TODO - apply the discounting

qbo_obs_trigrams <- get_qbo_obs_trigrams(inputWords,wfTriGram,Gamma)

#Find unobserved tail words
unobs_trig_tails <- wfUniGram[!wfUniGram$word %in% qbo_obs_trigrams$LastWord,]

LastWord <- word(inputWords,-1,-1)

#Find the observed bigrams and remove the ones already observed
qbo_obs_bigrams <- wfBigram[wfBigram$FirstWords == LastWord,]
qbo_obs_bigrams <- qbo_obs_bigrams[!qbo_obs_bigrams$LastWord %in% qbo_obs_trigrams$LastWord,]

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
unobs_trig_tails$FirstWords <- LastWord
names(unobs_trig_tails)[names(unobs_trig_tails)=="word"] <- "LastWord"
unobs_trig_tails <- unobs_trig_tails[,c("FirstWords","LastWord","freq","pDisc")] 

#check the sum of unobserved bigrams equates to the missing probabilty mass
#sum(unobs_trig_tails$pDisc)

alpha_trigam <- 1 - sum(qbo_obs_trigrams$pDisc)

qbo_unobs_trigrams <- rbind(qbo_obs_bigrams,unobs_trig_tails)

qbo_unobs_trigrams$pDisc <- alpha_trigam*(qbo_unobs_trigrams$pDisc/sum(qbo_unobs_trigrams$pDisc))

qbo_unobs_trigrams$FirstWords <- FirstWords

finalProb <- rbind(qbo_obs_trigrams,qbo_unobs_trigrams)

head(finalProb,10)

}
