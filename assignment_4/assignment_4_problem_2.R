# assignment url: https://hotneim.github.io/ban400/assignment-04.html 
# notion url: https://fjellestad.notion.site/Learning-notes-assignment-4-f1b27ffd1c654512a3a383affed55383?pvs=4


# Load the necessary library
library(tweedie)
library(ggplot2)

# Task 1
# Define the function simTweedieTest
simTweedieTest <- function(N) {
  # Parameters for the Tweedie distribution
  mu <- 10000
  phi <- 100
  power <- 1.9
  
  # Simulate data from the Tweedie distribution
  simulated_data <- rtweedie(n = N, mu = mu, phi = phi, power = power)
  
  # Perform a t-test on the simulated data with null hypothesis mu = 10000
  p_value <- t.test(simulated_data, mu = mu)$p.value
  
  # Return the p-value
  return(p_value)
}

# Example usage
N <- 100
p_value <- simTweedieTest(N)
print(p_value)

# Task 2: 
MTweedieTests <- function(M, N, alpha) {
  count <- 0
  for(i in 1:M) {
    p_value <- simTweedieTest(N)
    if(p_value < alpha) {
      count <- count + 1
    }
  }
  percentage <- count / M
  
  return(percentage)
}

MTweedieTests(10, 10, 0.05)

# Task 3
sample_data <- 
  tibble(N = c(10, 100, 1000, 5000), M = 100, 
         share_reject = NA)

for(i in 1:nrow(sample_data)) {
  result <- MTweedieTests(sample_data$M[i], 
                sample_data$N[i], 0.05)
  sample_data$share_reject[i] <- result
}

ggplot(sample_data, aes(x = N, y = share_reject)) +
  geom_point() +
  geom_line() +
  scale_x_log10() + # Optional: use a log scale for the x-axis
  xlab("Sample Size (N)") +
  ylab("Share of Rejections") +
  ggtitle("Validity of t-test on Tweedie Distribution")

# Task 4



