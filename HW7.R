setwd("/Users/alwinlin/Desktop/DSC 382 Regression")
# Read data
data <- read.csv(file = 'HW7_data.csv', header = TRUE)
# Problem 1
y <- data$y
x <- data$x
n <- 100

# Initial value
t <- 1

# Newton–Raphson algorithm
for(k in 2:10){
  # First derivative of SSE
  u  <- -sum((y - exp(t * x)) * (x * exp(t * x)))
  # Second derivative
  d  <- sum((x * exp(t * x))^2 - (y - exp(t * x))*(x^2 * exp(t * x)))
  # Update theta
  t <- t - u/d
}
theta_hat <- t # This is for clarification that value 1.2468 is theta hat
theta_hat

# Problem 3
y_hat <- exp(theta_hat * x)
sigma_hat <- sqrt(mean((y - y_hat)^2))
B <- 100
theta_boot <- numeric(B)
for(b in 1:B){
  y_star <- exp(theta_hat * x) + sigma_hat * rnorm(n) # Parametric bootstrap parameter
  t_star <- 1
  for(k in 2:10){
    u  <- -sum((y_star - exp(t_star * x)) * (x * exp(t_star * x)))
    d  <- sum((x * exp(t_star * x))^2 - (y_star - exp(t_star * x))*(x^2 * exp(t_star * x)))
    t_star <- t_star - u/d
  }
  theta_boot[b] <- t_star
}

# Bootstrap variance
var_theta <- var(theta_boot)
var_theta

# Problem 5
x_bar <- mean(x)

log_yhat <- theta_hat * x_bar

# variance using delta method
var_logy <- x_bar^2 * var_theta
std_logy <- sqrt(var_logy)

CI_lower <- log_yhat - 1.96 * std_logy # 1.96 is z-score for 95% confidence
CI_upper <- log_yhat + 1.96 * std_logy

c(CI_lower, CI_upper)
