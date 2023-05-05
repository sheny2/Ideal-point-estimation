library(tidyverse)
library(irlba)

##### prepare WtW
load("sparse_W.RData")

dim(sparse_W)

##### Remove rare words
all_row_sum_W = rowSums(sparse_W)
great_5_W = which(all_row_sum_W >= 5)
W = sparse_W[great_5_W, ]
dim(W)

##### Remove just numbers
W <- W[!grepl("^[0-9]+$", rownames(W)), ]
dim(W)

# rownames(sparse_W) %>% view()
# rownames(W) %>% view()


##### Choose one way of WTW

WtW_count = t(W) %*% (W)  # word count

# Apply Laplacian 
rsWtW_count = rowSums(WtW_count)
lap_WtW_c = diag(rsWtW_count^{-1/2})%*%WtW_count%*%diag(rsWtW_count^{-1/2})


WtW = t(W>0) %*% (W>0) # word occurrence

# Apply Laplacian 
rsWtW = rowSums(WtW)
lap_WtW = diag(rsWtW^{-1/2})%*%WtW%*%diag(rsWtW^{-1/2})

# Spectral clustering and the high-dimensional stochastic blockmodel

dim(lap_WtW)



### prepare yty

load("sparsematrix_1m_933.RData")

dim(y)

yty = t(y) %*% y

## Apply Laplacian 
rsyty = rowSums(yty)
lap_yty = diag(rsyty^{-1/2})%*%yty%*%diag(rsyty^{-1/2})



# Combine them 
comb_lap = lap_yty + lap_WtW

comb_lap_c = lap_yty + lap_WtW_c



