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
        
        #currd = (currRTimes + 1) * (nextN / (N * currN)) # assumption: 0 < d < 1
        
        currd <- nextN/N*currN

        print(currd)
    
        # the beauty of "data.table"!
        Ngrams[Ngrams$freq == currRTimes,"discount"] <- currd
        }
    
    Ngrams
}





