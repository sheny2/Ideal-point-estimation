library(mosaic)
library(tweetscores)
set.seed(2022)  



tweetscores::refdataCA$colnames
tweetscores::refdataCA$id
tweetscores::refdataCA$colcoord[,1]





Barbera_estimateIdeology(user = friends[[3]]$from_id[1], friends = friends[[3]]$to_id)
New_estimateIdeology(user = friends[[3]]$from_id[1], friends = friends[[3]]$to_id)

readRDS("")



which(myrefData$colnames %in% tweetscores::refdataCA$id)

# both timepoints available
tweetscores::refdataCA$colnames[which(tweetscores::refdataCA$id %in% myrefData$colnames)]


# CNN

tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "759251"),1]
myrefData$colcoord[which(myrefData$colnames == "759251"),1]


# nytimes

tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "807095"),1]
myrefData$colcoord[which(myrefData$colnames == "807095"),1]


# JoeBiden

tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "939091"),1]
myrefData$colcoord[which(myrefData$colnames == "939091"),1]

# BarackObama

tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "813286"),1]
myrefData$colcoord[which(myrefData$colnames == "813286"),1]


# FoxNews

tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "1367531"),1]
myrefData$colcoord[which(myrefData$colnames == "1367531"),1]



# seanhannity
tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "41634520"),1]
myrefData$colcoord[which(myrefData$colnames == "41634520"),1]



# TuckerCarlson
tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "22703645"),1]
myrefData$colcoord[which(myrefData$colnames == "22703645"),1]



elite_name = c("@CNN", "@JoeBiden", "@SpeakerPelosi","@sensanders", "@HillaryClinton","@FoxNews","@tedcruz", "@seanhannity", "@TuckerCarlson")

est_2015 = c(tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "759251"),1], tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "939091"),1],
             tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "15764644"),1],tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "29442313"),1],tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "1339835893"),1],
             tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "1367531"),1],tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "23022687"),1],
             tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "41634520"),1],tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "22703645"),1])

est_2022 = c(myrefData$colcoord[which(myrefData$colnames == "807095"),1],myrefData$colcoord[which(myrefData$colnames == "939091"),1],
             myrefData$colcoord[which(myrefData$colnames == "15764644"),1],myrefData$colcoord[which(myrefData$colnames == "29442313"),1],myrefData$colcoord[which(myrefData$colnames == "1339835893"),1],
             myrefData$colcoord[which(myrefData$colnames == "1367531"),1],myrefData$colcoord[which(myrefData$colnames == "23022687"),1], 
             myrefData$colcoord[which(myrefData$colnames == "41634520"),1],myrefData$colcoord[which(myrefData$colnames == "22703645"),1])


data.frame("Twitter Account" = elite_name, `Estimation in 2015` = est_2015, `Estimation in 2022` = est_2022, `Followers in 2015` = c(17459282,998737, 604664,349149,3612817,5235703,413664,1123327,2060971),
           `Followers in 2022` = c(61122613,36903062, 8123329,12494541,31580745,23711845,5932078,6311037,5567201))
kable(data.frame("Twitter Account" = elite_name, `Estimation in 2015` = est_2015, `Estimation in 2022` = est_2022), format = "html")


which(tweetscores::refdataCA$colnames == "29442313")
tweetscores::refdataCA$id[1005]


# sensanders
tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "11134252"),1]
myrefData$colcoord[which(myrefData$colnames == "11134252"),1]


# sensanders
tweetscores::refdataCA$colcoord[which(tweetscores::refdataCA$id == "15764644"),1]
myrefData$colcoord[which(myrefData$colnames == "15764644"),1]
