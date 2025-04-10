library(tidyverse)
library(clusterSim)

load("sparse_W.RData")


library(irlba)
###attempted CA by hand:

dim(sparse_W)

# Remove rare words
all_row_sum_W = rowSums(sparse_W)
great_5_W = which(all_row_sum_W >= 5)
W = sparse_W[great_5_W, ]
dim(W)

# Remove just numbers
W <- W[!grepl("^[0-9]+$", rownames(W)), ]
dim(W)


W = sparse_W

# W = sparse_W/(sqrt(sum(sparse_W^2)))
# W = sparse_W/(sqrt(max(sparse_W^2)))
# std_W = apply(sparse_W, MARGIN = 1, scale)

WtW = t(W) %*% (W)  # word count

WtW = t(W>0) %*% (W>0) # word occurrence


dim(WtW)

# std_W = sparse_W/rowSums(sparse_W)
# std_WtW = t(std_W) %*% std_W

## Apply Lapcian 
rsWtW = rowSums(WtW)
lap_WtW = diag(rsWtW^{-1/2})%*%WtW%*%diag(rsWtW^{-1/2})


load("sparsematrix_1m_933.RData")

dim(y)

yty = t(y) %*% y

## Apply Lapcian 
rsyty = rowSums(yty)
lap_yty = diag(rsyty^{-1/2})%*%yty%*%diag(rsyty^{-1/2})
# yty = scale(t(y) %*% y)


std_y = y/rowSums(y)
std_yty = t(std_y) %*% std_y


dim(yty)


Combine = yty + WtW
std_Combine = std_yty+ std_WtW
std_Combine2 = yty + std_WtW


comb_lap = lap_yty + lap_WtW

# save(Combine, file = "Combine.RData")




##########  old code

load("sparsematrix.RData")
y = y[,colnames(sparse_W)]

dim(y)

all_row_sum = rowSums(y)
great_10_index = which(all_row_sum >= 10)
y = y[great_10_index, ]


set.seed(20230320)
sample_1m_index = sample(1:nrow(y), size = 1000000, replace = F)
y = y[sample_1m_index, ]


dim(y)

y = y/(sqrt(sum(y^2)))

yty = t(y) %*% y


Combine = yty + WtW

save(Combine, file = "Combine.RData")


# source("CA.R")
# res <- CA(y, nd=3) # this takes a while
# 
# 
# myrefData2 = res 
# save(myrefData2, file = "myrefData2.Rdata")
# 
# Gamma_CA = res$colcoord[,1]

