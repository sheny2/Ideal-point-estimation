library(tidyverse)
library(tweetscores)



load("elite_info.RData")
elite_info_full = elite_info[-740,]

load("friends.RData")

load("myrefData.RData")
load("gam1.RData")


gamma = myrefData$colcoord[,1]

plot(density(gam1))

plot(gamma, gam1)
abline(h = 0)
abline(v = 0, col = "red")


length(myrefData$colnames)

myrefData$qs = tweetscores::refdataCA$qs



# Compare results

Barbera_estimateIdeology <- function(user, friends, verbose=TRUE, exact=FALSE,
                                     replace_outliers=FALSE){
  if(missing(friends))
    friends <- getFriends(user)
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

# tweetscores::refdataCA


Barbera_estimateIdeology(user = friends[[3]]$from_id[1], friends = friends[[3]]$to_id)


library(doMC)
doMC::registerDoMC(detectCores()-1)
getDoParWorkers()


old_ideal_points <- foreach(i = 1:1238, .combine = c) %dopar% {
  tryCatch({
    tweetscores::estimateIdeology2(user = friends[[i]]$from_id[1], friends = friends[[i]]$to_id)
  }, error = function(e) {
    # If an error occurs, return NA
    return(NA)
  })
}





New_estimateIdeology <- function(user, friends, verbose=TRUE, exact=FALSE,
                                 replace_outliers=FALSE){
  if(missing(friends))
    friends <- getFriends(user)
  # getting row of adjacency matrix
  y <- matrix((myrefData$colname %in% friends)*1, nrow=1)
  # info message
  if (sum(y)==0){
    stop("User follows 0 elites!")
  }
  message(user, " follows ", sum(y), " elites: ",
          paste(myrefData$colname[
            myrefData$colname %in% friends], collapse=", "))
  # estimation
  values <- supplementaryRows(myrefData, y)
  # normalizing
  theta <- myrefData$qs$theta[which.min(abs(values[1] - (myrefData$qs$value)))]
  # adding random noise
  # see https://github.com/pablobarbera/echo_chambers/blob/master/02_estimation/11-second-stage.r
  if (!exact) theta <- theta + rnorm(1, 0, 0.05)
  # replacing outliers
  if (replace_outliers && (theta == -Inf || theta == Inf)){
    # sample 10000 values from normal
    if (!exact) set.seed(123)
    rs <- rnorm(n=10000)
    # keep those below or above threshold
    if (theta == -Inf){ theta <- rs[rs<myrefData$qs$theta[2]][1] }
    if (theta == Inf){ theta <- rs[rs>myrefData$qs$theta[100]][1] }
  }
  
  return(theta)
}


New_estimateIdeology(user = friends[[3]]$from_id[1], friends = friends[[3]]$to_id)


new_ideal_points <- foreach(i = 1:1238, .combine = c) %dopar% {
  tryCatch({
    New_estimateIdeology(user = friends[[i]]$from_id[1], friends = friends[[i]]$to_id)
  }, error = function(e) {
    # If an error occurs, return NA
    return(NA)
  })
}


# examine result 

length(tweetscores::refdataCA$colnames)
length(myrefData$colnames)


# combine old and new results
load("All_Data/Final Data with Treatment Assignments.Rdata")

all_id_str = final_assignments$id_str

result_ip_old = data.frame("id_str" = all_id_str[1:(length(all_id_str)-1)], "ideal_point" = old_ideal_points, "estimate" = "old")
result_ip_new = data.frame("id_str" = all_id_str[1:(length(all_id_str)-1)], "ideal_point" = new_ideal_points, "estimate" = "new")

result_ip = rbind(result_ip_old, result_ip_new) %>% drop_na()

result_ip = left_join(result_ip, final_assignments, by="id_str")

table(result_ip$estimate)



new_id_avail = (result_ip %>% group_by(id_str) %>% summarise(count = n()) %>% filter(count == 2))$id_str

result_ip_match = result_ip %>% filter(id_str %in% new_id_avail)


# plot 

result_ip_match %>% filter(estimate == "old") %>% ggplot() + geom_density(aes(x=ideal_point, color = party_ID))
result_ip_match %>% filter(estimate == "new") %>% ggplot() + geom_density(aes(x=ideal_point, color = party_ID))

# compare
ggplot(result_ip_match) + geom_density(aes(x=ideal_point, color = party_ID)) + facet_wrap(~estimate)


ggplot(result_ip_match) + geom_point(aes(x = id_str, y=ideal_point, color = estimate)) 







