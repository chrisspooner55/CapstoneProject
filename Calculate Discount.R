# Supposed table "threeGramTable" as above, we want to add a "discount" column.
qbo_obs_bigrams$discount = rep(1, nrow(qbo_obs_bigrams))

# Calculate the discount coefficient.
# We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".

N <- 0
for(i in 1:5){
    currRTimes = i
    N = N + nrow(qbo_obs_bigrams[qbo_obs_bigrams$freq == currRTimes,])
}

N

for(i in 1:5){
    currRTimes = i
    nextRTimes = currRTimes + 1
    
    currN = nrow(qbo_obs_bigrams[qbo_obs_bigrams$freq == currRTimes,])
    nextN = nrow(qbo_obs_bigrams[qbo_obs_bigrams$freq == nextRTimes,])
    
    currd = (currRTimes + 1) * nextN / (N * currN) # assumption: 0 < d < 1
    
    #if (currd >1 | currd == 0){
    #    currd = 1
    #}
    print (currN)
    print(currd)

    # the beauty of "data.table"!
    qbo_obs_bigrams[qbo_obs_bigrams$freq == currRTimes,"discount"] <- currd

}