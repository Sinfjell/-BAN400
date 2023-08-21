library(readr)
missile <- read_csv("data-missile.csv")
head(missile)

#mean
mean_apogee <- mean(missile$apogee, na.rm = TRUE)
  # The na.rm = TRUE argument ensures that any missing values (NAs) 
  # are removed before the calculations are performed
print(paste("Mean of apogee:", mean_apogee))
  
# median  
median_apogee <- median(missile$apogee, na.rm = TRUE)
print(paste("Median of apogee:", median_apogee))

# standard deviation
sd_apogee <- sd(missile$apogee, na.rm = TRUE)
print(paste("Standard deviation of apogee:", sd_apogee))