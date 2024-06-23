# Load required packages
library(MASS)
library(ggplot2)
library(plotly)

# Setting the seed
set.seed(0)

# Number of points in each class
n1 <- 500
n0 <- c(25, 500)[1] # (!) Set to 1 for unbalanced data, 2 for balanced data

# Define true logistic regression parameters
beta0 <- 2  # Adjusted to shift the boundary
beta1 <- -5
beta2 <- -4

# Function to generate class labels based on logistic regression model
generate_labels <- function(x1, x2, beta0, beta1, beta2) {
  log_odds <- beta0 + beta1 * x1 + beta2 * x2
  probs <- 1 / (1 + exp(-log_odds))
  labels <- rbinom(length(probs), 1, probs)
  return(labels)
}

# Generate the data for class 1
mu1 <- c(-1, -1)
sigma1 <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
x1_class1 <- mvrnorm(n1, mu = mu1, Sigma = sigma1)
labels_class1 <- generate_labels(x1_class1[, 1], x1_class1[, 2], beta0, beta1, beta2)

# Generate the data for class 0
mu0 <- c(1, 1)
sigma0 <- matrix(c(1, -0.8, -0.8, 1), nrow = 2)
x1_class0 <- mvrnorm(n0, mu = mu0, Sigma = sigma0)
labels_class0 <- generate_labels(x1_class0[, 1], x1_class0[, 2], beta0, beta1, beta2)

# Combine data into a single data frame
data <- data.frame(
  x1 = c(x1_class0[, 1], x1_class1[, 1]),
  x2 = c(x1_class0[, 2], x1_class1[, 2]),
  class = c(labels_class0, labels_class1)
)

# Ensure class labels are numeric
data$class <- as.numeric(data$class)

# Fit logistic regression model
model <- glm(class ~ x1 + x2, data = data, family = binomial)

# Create a grid of values for plotting the decision boundary
x1_grid <- seq(min(data$x1) - 1, max(data$x1) + 1, length = 200)
x2_grid <- seq(min(data$x2) - 1, max(data$x2) + 1, length = 200)
grid <- expand.grid(x1 = x1_grid, x2 = x2_grid)

# Compute standard errors for predictions
preds <- predict(model, newdata = grid, type = "link", se.fit = TRUE)
grid$fit <- preds$fit
grid$se.fit <- preds$se.fit
grid$upr <- grid$fit + 1.96 * grid$se.fit
grid$lwr <- grid$fit - 1.96 * grid$se.fit

# Transform back to probabilities
grid$prob <- 1 / (1 + exp(-grid$fit))
grid$upr_prob <- 1 / (1 + exp(-grid$upr))
grid$lwr_prob <- 1 / (1 + exp(-grid$lwr))

# Decision boundaries with confidence intervals
threshold <- 0.05  # Adjust threshold to capture more points
decision_boundary <- grid[abs(grid$prob - 0.5) < threshold, ]
decision_boundary_upr <- grid[abs(grid$upr_prob - 0.5) < threshold, ]
decision_boundary_lwr <- grid[abs(grid$lwr_prob - 0.5) < threshold, ]

ggplot(data, aes(x = x1, y = x2, color = as.factor(class))) +
  geom_point(size = 2) +
  labs(title = "Data and Estimated Logistic Regression Decision Boundary with 95% CI",
       x = "X1", y = "X2") +
  scale_color_manual(values = c("blue", "red"), name = "Class") +
  geom_smooth(data = decision_boundary, aes(x = x1, y = x2), color = "black", linetype = "dashed", lwd = 1) +
  geom_smooth(data = decision_boundary_upr, aes(x = x1, y = x2), color = "black", linetype = "dotted", lwd = 1) +
  geom_smooth(data = decision_boundary_lwr, aes(x = x1, y = x2), color = "black", linetype = "dotted", lwd = 1) +
  theme_minimal()


# Plot log-likelihood surface for beta1 and beta2
log_likelihood <- function(beta, x1, x2, y) {
  log_odds <- beta[1] + beta[2] * x1 + beta[3] * x2
  probs <- 1 / (1 + exp(-log_odds))
  eps <- 1e-6
  probs <- pmin(pmax(probs, eps), 1 - eps)
  ll <- sum(y * log(probs) + (1 - y) * log(1 - probs))
  return(-ll)  # Negative log-likelihood for minimization
}

plot_log_likelihood <- function(data, beta0, beta1_range, beta2_range) {
  ll_values <- matrix(NA, nrow = length(beta1_range), ncol = length(beta2_range))
  
  for (i in 1:length(beta1_range)) {
    for (j in 1:length(beta2_range)) {
      beta <- c(beta0, beta1_range[i], beta2_range[j])
      ll_values[i, j] <- log_likelihood(beta, data$x1, data$x2, data$class)
    }
  }
  
  plot_ly(x = ~beta1_range, y = ~beta2_range, z = ~ll_values, type = "surface", 
          colorscale = list(
            c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1),
            c("darkblue", "blue", "cyan", 
              "green", "yellowgreen", "yellow", 
              "orange", "orangered", "red", "darkred", 
              "purple", "magenta", "pink", 
              "white", "grey", "black")
          )) %>%
    layout(title = "Log-Likelihood Surface",
           scene = list(
             xaxis = list(title = "Beta1"),
             yaxis = list(title = "Beta2"),
             zaxis = list(title = "Log-Likelihood")  
           ))
}

# Define beta ranges
beta1_range <- seq(-20, 5, length.out = 200)
beta2_range <- seq(-20, 5, length.out = 200)

# Plot the log-likelihood surface for beta1 and beta2
plot_log_likelihood(data, beta0, beta1_range, beta2_range)

# Estimated model coefficients
summary(model)
