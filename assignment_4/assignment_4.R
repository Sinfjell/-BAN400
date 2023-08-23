# assignment url: https://hotneim.github.io/ban400/assignment-04.html 
# notion url: https://fjellestad.notion.site/Learning-notes-assignment-4-f1b27ffd1c654512a3a383affed55383?pvs=4

# Loading librarys
library(tidyr)
library(dplyr)


# PROBLEM 1
# Task 1



sim_norm <- function(N, mu = 0, sigma = 1) {
  observations <- rnorm(N, mean = mu, sd = sigma)
  sample_mean <- mean(observations)
  return(sample_mean)
}

# Task 2
# Define M
M <- 100
N = 10

# Create an empty vector of length M
sample_means <- rep(NA, M)

# Use a for-loop to fill the vector with sample means
for (i in 1:length(sample_means)) {
  sample_means[i] <- sim_norm(N) # You can change N as needed
}

# Print the sample means
print(sample_means)

# Task 3
observed_sd <- sd(sample_means)
true_sd <- sigma / sqrt(N)

# Print the results
cat("Observed standard deviation of sample means:", observed_sd, "\n")
cat("True standard deviation (standard error):", true_sd, "\n")
cat("Difference:", true_sd - observed_sd, "\n")

# Task 4
# Creating the initial tibble
dataset <- tibble(N = seq(from = 10, to = 200, by = 10), 
                  st_dev = NA,  sigma = 1, theoretical = NA,
                  )
# Setting up the teoretical column
dataset |> mutate(theoretical = sigma/sqrt(N))

# Task 5 

