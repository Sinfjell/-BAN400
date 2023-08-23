# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Function to simulate a t-test for either Tweedie or normal distribution
simTest <- function(N, distribution = "normal", mu = 10000, phi = 100, power = 1.9) {
  if (distribution == "tweedie") {
    data <- rtweedie(N, mu = mu, phi = phi, power = power)
  } else {
    data <- rnorm(N, mean = mu, sd = sqrt(mu))
  }
  p_value <- t.test(data, mu = mu)$p.value
  return(p_value)
}

# Function to run multiple tests and return the percentage of rejections using a loop
MTests <- function(M, N, alpha, distribution) {
  count <- 0
  for(i in 1:M) {
    p_value <- simTest(N, distribution)
    if(p_value < alpha) {
      count <- count + 1
    }
  }
  percentage <- count / M
  return(percentage)
}

# Create a data frame to store results
sample_data <- 
  tibble(N = c(10, 100, 1000, 5000), M = 100, 
         share_reject_tweedie = NA, share_reject_normal = NA)

# Loop to fill in the share of rejections for both distributions
for(i in 1:nrow(sample_data)) {
  sample_data$share_reject_tweedie[i] <- MTests(sample_data$M[i], sample_data$N[i], 0.05, "tweedie")
  sample_data$share_reject_normal[i] <- MTests(sample_data$M[i], sample_data$N[i], 0.05, "normal")
}

# Reshape the data for plotting
sample_data_long <- sample_data %>%
  gather(key = "distribution", value = "share_reject", -N, -M)

# Create the plot
ggplot(sample_data_long, aes(x = N, y = share_reject, color = distribution)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  xlab("Sample Size (N)") +
  ylab("Share of Rejections") +
  ggtitle("Validity of t-test on Different Distributions") +
  scale_color_discrete(name = "Distribution", labels = c("Normal", "Tweedie"))
