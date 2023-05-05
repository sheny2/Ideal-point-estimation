library(mosaic)
library(caret)
library(httr)
library(jsonlite)
library(RTwitterV2)
library(tweetscores)
library(rtweet)

# scp ~/Desktop/AVL/Twitter Project Code/Notes.Rmd  <netid>@dcc-login.oit.duke.edu:/hpc/group/volfovskylab/ys382/


bearer_token <- c("AAAAAAAAAAAAAAAAAAAAABoZjQEAAAAA0gWmqoUzlFKziClP1YhJTrpYjE4%3Dg5iWkyBV5OFJjdEVeYfjbTMmAP39qyhJQqsm0c7tKQwxB1VQrQ")


## Chris's elite data
load("Final Op Leader Twitter Data.Rdata")

elite_sample = final_op_data[,c(3,1)]
elite_sample[1,]


## creating adjacency matrix

elite_file_name = list.files("elite/", full.names = F)

elite_username <- gsub("\\..*","",elite_file_name)

# elite_info <- lookup_users(elite_username, retryonratelimit = T)
# save(elite_info, file = "elite_info.RData")

load("elite_info.RData")

all_elite_ids <- elite_info$id_str

elite_info_full = elite_info
elite_info <- elite_info_full[tolower(elite_info_full$screen_name) %in% c('abc',"joebiden","kamalaharris",
                                                             "nytimes", "cnn", "bccworld", "washingtonpost", "wsj",
                                                             "potus", "hillaryclinton") == FALSE, ]




edge_list = tibble(account = "", id_str = "")[-1,]

for (i in 50:100)
{
elite_follower <- readRDS(paste0("elite/", elite_info[i,4], ".RDS"))
edge_list = rbind(edge_list, 
                  data.frame(account = elite_info[i,1], 
                             id_str = unlist(elite_follower$followers)))
print(i)
}

names(edge_list)[names(edge_list) == "id"] <- "account"

users <- unique(edge_list$id_str)
n <- length(users)
accounts <- unique(edge_list$account)
m <- length(accounts)

rows <- list()
columns <- list()
pb <- txtProgressBar(min=1, max=m, style=3)
for (j in 1:m){
  followers <- edge_list$id_str[edge_list$account==accounts[j]]
  to_add <- which(users %in% followers)
  rows[[j]] <- to_add
  columns[[j]] <- rep(j, length(to_add))
  setTxtProgressBar(pb, j)
}


rows <- unlist(rows)
columns <- unlist(columns)

library(Matrix)
y <- sparseMatrix(i=rows, j=columns)
rownames(y) <- users[1:nrow(y)]
colnames(y) <- accounts



# estimation 
res <- tweetscores::CA(y, nd=3) # this takes a while

sum(res$colnames %in% res$rownames)


# phi for each elite 
-res$colcoord[,1]

elite_follower_id = res$colnames[res$colnames %in% res$rownames]

elite_follower = res$colnames %in% res$rownames 

# estimated phi (elite in row)
phi = -res$colcoord[,1]
elite_follower_phi = phi[elite_follower]
elite_follower_phi = ifelse(elite_follower_phi < -5, -5 , elite_follower_phi)
elite_follower_phi = ifelse(elite_follower_phi > 5, 5 , elite_follower_phi)

# estimated theta (elite in row)
elite_follower_theta = c()
for (i in seq_along(elite_follower_id))
{
  elite_follower_theta[i] = -res$rowcoord[,1][which(res$rownames == elite_follower_id[i])]
}


# elite_follower_inrow = which(res$rownames == res$colnames[elite_follower])
# elite_follower_theta = -res$rowcoord[,1][elite_follower_inrow]


plot(elite_follower_theta, elite_follower_phi)






tweet_id <- "142915704"
user_friends <- rtweet::get_friends(tweet_id, n = Inf, retryonratelimit = T)[,2]

y <- matrix((tweetscores::refdataCA$id %in% user_friends)*1, nrow=1)
y <- matrix((res$colnames %in% user_friends)*1, nrow=1)

values <- supplementaryRows(tweetscores::refdataCA, y)
values <- supplementaryRows(res, y)





estimateIdeologyNEW <- function(user, friends, verbose=TRUE, exact=FALSE, replace_outliers=FALSE){
  
  if(missing(friends))
    friends <- rtweet::get_friends(user, n = Inf, retryonratelimit = T)[,2]
  
  # getting row of adjacency matrix
  y <- matrix((tweetscores::refdataCA$id %in% friends)*1, nrow=1)
  
  # info message
  if (sum(y)==0){
    stop("User follows 0 elites!")
  }
  
  message(user, " follows ", sum(y), " elites: ",
          paste(tweetscores::refdataCA$colname[
            tweetscores::refdataCA$id %in% friends], collapse=", "))
  
  # estimation
  values <- supplementaryRows(tweetscores::refdataCA, y)
  # normalizing
  theta <- tweetscores::refdataCA$qs$theta[which.min(abs(values[1] - (tweetscores::refdataCA$qs$value)))]
  # adding random noise
  # see https://github.com/pablobarbera/echo_chambers/blob/master/02_estimation/11-second-stage.r
  if (!exact) theta <- theta + rnorm(1, 0, 0.05)
  # replacing outliers
  if (replace_outliers && (theta == -Inf || theta == Inf)){
    # sample 10000 values from normal
    if (!exact) set.seed(123)
    rs <- rnorm(n=10000)
    # keep those below or above threshold
    if (theta == -Inf){ theta <- rs[rs<tweetscores::refdataCA$qs$theta[2]][1] }
    if (theta == Inf){ theta <- rs[rs>tweetscores::refdataCA$qs$theta[100]][1] }
  }
  
  return(theta)
}




tweet_id <- "142915704"
user_friends <- rtweet::get_friends(tweet_id, n = Inf, retryonratelimit = T)[,2]

result = tweetscores::estimateIdeology(user = tweet_id, friends = user_friends$to_id, verbose = F)

mean(results$samples[,,2])




estimateIdeology2(user = tweet_id, friends = user_friends$to_id)






