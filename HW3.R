setwd("/Users/alwinlin/Desktop/DSC 382 Regression")
data <- read.csv(file = 'HW3_data.csv', header = TRUE)
eq1 <- lm(y ~ x, data = data) #First equation
eq2 <- lm(m ~ x, data = data) #Second equation regress m on y
eq3 <- lm(y ~ m + x, data = data) #Third equation regress the last equation with m & x
beta11 <- coef(eq1)[1]
beta12 <- coef(eq1)[2]
beta21 <- coef(eq2)[1]
beta22 <- coef(eq2)[2]
beta31 <- coef(eq3)[1]
beta32 <- coef(eq3)[2]
beta33 <- coef(eq3)[3]
Matrix2 <- model.matrix(eq2)
vareq2 <- solve(t(Matrix2) %*% Matrix2)
var22 <- diag(vareq2)[2]   # Variance of beta22
Matrix3 <- model.matrix(eq3)
vareq3 <- solve(t(Matrix3) %*% Matrix3)
var32 <- diag(vareq3)[2]   # Variance of beta32
Z_sobel <- (beta12 - beta33) / sqrt((beta22^2 * var32) + (beta32^2 * var22))
p_val <- 2 * (1 - pnorm(abs(Z_sobel)))
