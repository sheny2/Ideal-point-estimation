library(mosaic)
set.seed(2022)  

# Set the dimensionality
nd <- 1
n <- 100
S <- 1000


###  generate U
U <- matrix(sort(runif(n, -10, 10)), ncol = nd, nrow =  n)

dim(U)
# plot(U)


### generate D

D <- rnorm(S, mean = 0, sd = 1)


### generate V

V = list()
for (i in 1:S){
  V[[i]] = matrix(runif(n, -5, 5), ncol = nd, nrow =  n)
}


A1_raw = U %*% D[1] %*% t(V[[1]]) + matrix(rnorm(n*n, 0,1), n, n)
A2_raw = U %*% D[2] %*% t(V[[2]]) + matrix(rnorm(n*n, 0,1), n, n)

# latent variable model: probit model? 
# A_ij = 1 if (UDV')_{ij} > 0, o/w A_ij = 0
A1 = matrix(data = as.numeric(A1_raw>0), nrow = n, ncol = n)
A2 = matrix(data = as.numeric(A2_raw>0), nrow = n, ncol = n)


gam_0 = eigen(A1_raw %*% t(A1_raw))$vectors[, 1]
gam_1 = eigen(A1 %*% t(A1))$vectors[, 1]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 2]


# be careful with sign identification
par(mfrow = c(1, 4))
plot((U), main = "True U")
plot((gam_0), col = "green", main = "Raw A = UDV'") # this must align bc left singular vector = eigenvector of AA' 
plot((gam_1), col = "blue", main = "Probit A1A1'")
plot((gam_2), col = "red", main = "Probit A1A1' + A2A2'")





mat_sum = matrix(0, nrow = n, ncol = n)
AAT_sum = matrix(0, nrow = n, ncol = n)

for (s in 1:1000){
  mat = U %*% D[s] %*% t(V[[s]]) + + matrix(rnorm(n*n, 0,1), n, n)
  
  mat_sum = mat_sum + mat %*% t(mat)
  
  A_s = matrix(data = as.numeric(mat>0), nrow = n, ncol = n)
  AAT_sum = AAT_sum + A_s %*% t(A_s)
}


gam1 = eigen(mat_sum)$vectors[, 1] 
gam2 = eigen(AAT_sum)$vectors[, 1] 



par(mfrow = c(1, 3))
plot((U))
plot((gam1), col = "blue")
plot((-gam2), col = "red")







# latent variable model: logit? 
# A_ij = 1 if inverse_logit(UDV')_{ij} > 0.5, o/w A_ij = 0
A1 = matrix(data = as.numeric(ilogit(A1_raw)>0.5), nrow = n, ncol = n)
A2 = matrix(data = as.numeric(ilogit(A2_raw)>0.5), nrow = n, ncol = n)


gam_0 = eigen(A1_raw %*% t(A1_raw))$vectors[, 1]
gam_1 = eigen(A1 %*% t(A1))$vectors[, 1]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 1]


par(mfrow = c(1, 4))
plot((U), main = "True U")
plot((gam_0), col = "green", main = "Raw A = UDV'")
plot((gam_1), col = "blue", main = "logit A1A1'")
plot((gam_2), col = "red", main = "logit A1A1' + A2A2'")


