setwd("/Users/alwinlin/Desktop/DSC 382 Regression")
data <- read.csv(file = 'HW2_data.csv', header = TRUE)
model <- lm(y ~ . - 1, data = data)  # -1 because first column of X is 1 (intercept)
beta_h <- coef(model) # Pulls the betas
sd2_Cj <- vcov(model) # sd^2 * (X^T*X)^-1
sqrt_sd2_Cj <- sqrt(sd2_Cj[3, 3]) # 3,3 is location of sd^2*C33, sqrt of that is SE(beta 3)
#Q2
no_x3_model <- lm(y ~ x1 + x2, data = data) # Since x3 is 0
SoR_reduced <- sum(residuals(no_x3_model)^2) 
SoR_model <- sum(residuals(model)^2)
Ftest <- ((SoR_reduced - SoR_model) / (3 - 2)) / (SoR_model / (50 - 3))
Ftesta = qf(0.99, df1 = 1, df2 = 47)
#Q3
t = beta_h[3]/sqrt_sd2_Cj # Beta 3 divided by standard error of Beta 3
p_val = 2 * (1 - pt(t, df = 47, lower.tail = FALSE))
#Q4
h <- hatvalues(model) # Gets hii values
h[1:2] # Display values for first and second
#Q5
newdata <- data.frame(y = NA, x1 = 1, x2 = 0.12, x3 = 0.56)  # New df with given x values
predict(model, newdata = newdata, interval = "confidence", level = 0.95)
