library(mosaic)
set.seed(20230906)  

# Set the dimensionality
nd <- 1
n <- 100
S <- 1000


###  generate U
U <- matrix(sort(runif(n, -5, 5)), ncol = nd, nrow =  n)

dim(U)
# plot(U)


### generate D

D <- rnorm(S, mean = 0, sd = 1)


### generate V

V = list()
for (i in 1:S){
  V[[i]] = matrix(runif(n, -1, 1), ncol = nd, nrow =  n)
}



A1_raw = U %*% D[1] %*% t(V[[1]]) + matrix(rnorm(n*n, 0,1), n, n)
A2_raw = U %*% D[2] %*% t(V[[2]]) + matrix(rnorm(n*n, 0,1), n, n)
A3_raw = U %*% D[3] %*% t(V[[3]]) + matrix(rnorm(n*n, 0,1), n, n)
A4_raw = U %*% D[4] %*% t(V[[4]]) + matrix(rnorm(n*n, 0,1), n, n)


# latent variable model: probit model? 
# A_ij = 1 if (UDV')_{ij} > 0, o/w A_ij = 0
A1 = matrix(data = as.numeric(A1_raw>0), nrow = n, ncol = n)
A2 = matrix(data = as.numeric(A2_raw>0), nrow = n, ncol = n)
A3 = matrix(data = as.numeric(A3_raw>0), nrow = n, ncol = n)
A4 = matrix(data = as.numeric(A4_raw>0), nrow = n, ncol = n)


# 
A1 = matrix(data = as.numeric(ilogit(A1_raw)>0.5), nrow = n, ncol = n)
A2 = matrix(data = as.numeric(ilogit(A2_raw)>0.5), nrow = n, ncol = n)
A3 = matrix(data = as.numeric(ilogit(A3_raw)>0.5), nrow = n, ncol = n)
A4 = matrix(data = as.numeric(ilogit(A4_raw)>0.5), nrow = n, ncol = n)


gam_0 = eigen(A1_raw %*% t(A1_raw))$vectors[, 1]
gam_1 = eigen(A1 %*% t(A1))$vectors[, 1]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 1]
gam_3 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3))$vectors[, 1]
gam_4 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3) + A4 %*% t(A4))$vectors[, 1]


# be careful with sign identification
par(mfrow = c(1, 4))
plot((U), main = "True U")
plot((gam_0), main = "Raw A = UD1V1, AA'") # this must align bc left singular vector = eigenvector of AA' 
plot((gam_1), main = "1st eignvec of A1A1'")
plot((gam_2), main = "1st eignvec of Probit A1A1' + A2A2'")



gam_1 = eigen(A1 %*% t(A1))$vectors[, 2]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 2]
gam_3 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3))$vectors[, 2]
gam_4 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3) + A4 %*% t(A4))$vectors[, 2]


# be careful with sign identification

par(mfrow = c(1, 4))
plot((U), main = "True U")
points((n-5):n, U[(length(U) - 5):length(U)], col = "red")   
plot((gam_1), main = "2nd eignvec of sum till A1A1'")
points((n-5):n, gam_1[(length(gam_1) - 5):length(gam_1)], col = "red")   
plot((gam_2), main = "2nd eignvec of sum till A2A2'")
points((n-5):n, gam_2[(length(gam_2) - 5):length(gam_2)], col = "red")   
plot((gam_3), main = "2nd eignvec of sum till A3A3'")
points((n-5):n, gam_3[(length(gam_3) - 5):length(gam_3)], col = "red")   




# Visualization
par(mfrow = c(2, 4))
gam_0 = eigen(A1_raw %*% t(A1_raw))$vectors[, 1]
gam_1 = eigen(A1 %*% t(A1))$vectors[, 1]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 1]
gam_3 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3))$vectors[, 1]
gam_4 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3) + A4 %*% t(A4))$vectors[, 1]
plot((U), main = "True U", ylab = "U")
plot((gam_0), main = "Raw A = UD1V1, AA'", ylab = "gam") # this must align bc left singular vector = eigenvector of AA' 
plot((gam_1), main = "1st eignvec of A1A1'", ylab = "gam")
plot((gam_2), main = "1st eignvec of A1A1' + A2A2'", ylab = "gam")
gam_1 = eigen(A1 %*% t(A1))$vectors[, 2]
gam_2 = eigen(A1 %*% t(A1) + A2 %*% t(A2))$vectors[, 2]
gam_3 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3))$vectors[, 2]
gam_4 = eigen(A1 %*% t(A1) + A2 %*% t(A2) + A3 %*% t(A3) + A4 %*% t(A4))$vectors[, 2]
plot((U), main = "True U", ylab = "U")
points((n-5):n, U[(length(U) - 5):length(U)], col = "red")   
plot((gam_1), main = "2nd eignvec of sum till A1A1'", ylab = "gam")
points((n-5):n, gam_1[(length(gam_1) - 5):length(gam_1)], col = "red")   
plot((gam_2), main = "2nd eignvec of sum till A2A2'", ylab = "gam")
points((n-5):n, gam_2[(length(gam_2) - 5):length(gam_2)], col = "red")   
plot((gam_3), main = "2nd eignvec of sum till A3A3'", ylab = "gam")
points((n-5):n, gam_3[(length(gam_3) - 5):length(gam_3)], col = "red")   



par(mfrow = c(1, 4))
plot((n-25):n, U[(length(U) - 25):length(U)], main = "True U", ylab = "U")
points((n-5):n, U[(length(U) - 5):length(U)], col = "red")   
plot((n-25):n, gam_1[(length(gam_1) - 25):length(gam_1)], main = "2nd eignvec of sum till A1A1'", ylab = "gam")
points((n-5):n, gam_1[(length(gam_1) - 5):length(gam_1)], col = "red")   
plot((n-25):n, gam_2[(length(gam_2) - 25):length(gam_2)], main = "2nd eignvec of sum till A2A2'", ylab = "gam")
points((n-5):n, gam_2[(length(gam_2) - 5):length(gam_2)], col = "red")   
plot((n-25):n, gam_3[(length(gam_3) - 25):length(gam_3)], main = "2nd eignvec of sum till A3A3'", ylab = "gam")
points((n-5):n, gam_3[(length(gam_3) - 5):length(gam_3)], col = "red")  



# augmentation with S number of times

mat_sum = matrix(0, nrow = n, ncol = n)
AAT_sum = matrix(0, nrow = n, ncol = n)

for (s in 1:S){
  mat = U %*% D[s] %*% t(V[[s]]) + + matrix(rnorm(n*n, 0,1), n, n)
  
  # mat_sum = mat_sum + mat %*% t(mat)
  
  A_s = matrix(data = as.numeric(mat>0), nrow = n, ncol = n)
  AAT_sum = AAT_sum + A_s %*% t(A_s)
}


gam1 = eigen(AAT_sum)$vectors[, 1] 
gam2 = eigen(AAT_sum)$vectors[, 2] 


# par(mfrow = c(1, 3))
# plot((U))
# plot((gam1), col = "blue")
# plot((gam2), col = "red")


par(mfrow = c(1, 3))
plot((U), main = "True U")
points((n-5):n, U[(length(U) - 5):length(U)], col = "red")
plot((gam1), main = "S Sum of AA', 1st eignvec")
points((n-5):n, gam1[(length(gam1) - 5):length(gam1)], col = "red")
plot((gam2), main = "S Sum of AA', 2nd eignvec")
points((n-5):n, gam2[(length(gam2) - 5):length(gam2)], col = "red")





