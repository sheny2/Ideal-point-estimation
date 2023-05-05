library(tidyverse)
library(doMC)
doMC::registerDoMC(detectCores()-1)
getDoParWorkers()

load("elite_info.RData")

elite_info_full = elite_info[-740,]

edge_list = tibble(account = "", id_str = "")[-1,]


edge_list = foreach (i = 1:941, .combine = "rbind") %dopar% {
  elite_follower <- readRDS(paste0("elite/", tolower(elite_info_full[i,4]), ".RDS"))
  data.frame(account = elite_info_full[i,2], id_str = unlist(elite_follower$followers))
}

# dim(edge_list)

# load("edge_list.RData")
# n_distinct(edge_list[,1])


colnames(edge_list) <- c("account", "id_str")

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
y <- sparseMatrix(i=rows, j=columns, x = 1L)
rownames(y) <- users[1:nrow(y)]
colnames(y) <- accounts



# trim users who follow fewer than 10 elites
all_row_sum = rowSums(y)
great_10_index = which(all_row_sum >= 10)
y = y[great_10_index, ]



res <- tweetscores::CA(y, nd=3) # this takes a while
myrefData = res
save(myrefData, file = "myrefData.RData")



library(irlba)
###attempted CA by hand:
ysum <- sum(y)
P <- y/ysum
rm <- apply(P, 1, sum)
cm <- apply(P, 2, sum)
eP <- rm %*% t(cm)
S <- (P - eP) * (eP)^(-0.5)

out <- irlba(S,nv=1)
phi1 <- (rm)^(-0.5) * out$u[,1]  # ordinary people's ideal points
gam1 <- (cm)^(-0.5) * out$v[,1]  # elite's ideal points



yout <- irlba(Y,nv=1)
yout$v[,1]


