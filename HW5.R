setwd("/Users/alwinlin/Desktop/DSC 382 Regression")
# Read data
data <- read.csv(file = 'HW5_data.csv', header = TRUE)

# Q4: OLS using SVD
y <- as.matrix(data[, 1])
X <- as.matrix(data[, -1])
svd_X <- svd(X)
U <- svd_X$u
D <- svd_X$d #lambda
V <- svd_X$v

# OLS beta_hat
gamma_hat <- (1/D) * t(U) %*% y   # gamma_hat = D^{-1} * U^T * y
beta_hat <- V %*% gamma_hat       # beta_hat = V * gamma_hat
beta_hat

# Q5: PCA regression
lambda_sq <- D^2
pve <- cumsum(lambda_sq) / sum(lambda_sq)  # proportion of variance explained
m <- which(pve > 0.9)[1]                    # select # of components explaining >90% variance

# Project y onto the first m principal components
gamma_pca <- rep(0, length(D))
gamma_hat_full <- (1/D) * t(U) %*% y
gamma_pca[1:m] <- gamma_hat_full[1:m]

# Transform back to original coefficients
beta_pca <- V %*% gamma_pca

# Output
m
beta_pca