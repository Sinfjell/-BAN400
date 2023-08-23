# assignment url: https://hotneim.github.io/ban400/assignment-04.html 
# notion url: https://fjellestad.notion.site/Learning-notes-assignment-4-f1b27ffd1c654512a3a383affed55383?pvs=4

# Loading librarys
library(tidyr)
library(dplyr)
library(ggplot2)


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
                  ) |>  mutate(theoretical = sigma/sqrt(N))


# Task 5 
# Assuming you have the dataset from Task 4
M <- 200 # Number of sample means

# Looping over all the rows of the dataset
# Assuming you have the dataset from Task 4 and M defined earlier
for(i in 1:nrow(dataset)) {  # "Outer" for loop, using "i" as counting index
  sample_means_2 <- rep(NA, M)
  
  # "Inner" loop to create a vector of sample means
  for(j in 1:M) {
    sample_means_2[j] <- sim_norm(dataset$N[i])
  }
  
  # Insert the standard deviation of the sample means into the data frame
  dataset$st_dev[i] <- sd(sample_means_2)
}

# Task 6: Plotting
# Create a data frame for the theoretical line
theoretical_data <- tibble(N = dataset$N, value = dataset$theoretical, type = "Theoretical")

# Create a data frame for the observed line
observed_data <- tibble(N = dataset$N, value = dataset$st_dev, type = "Observed")

# Combine the two data frames
plot_data <- bind_rows(theoretical_data, observed_data)

# Create the plot
ggplot(plot_data, aes(x = N, y = value, linetype = type, color = type)) +
  geom_line(size = 1) +
  labs(title = "Comparison of Observed and Theoretical Standard Deviations",
       x = "Sample Size (N)",
       y = "Standard Deviation") +
  theme_minimal() +
  scale_linetype_manual(values = c("Theoretical" = "dashed", "Observed" = "solid")) +
  scale_color_manual(values = c("Theoretical" = "red", "Observed" = "blue")) +
  theme(legend.title = element_blank())

       