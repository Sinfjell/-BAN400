# assignment urls
  # https://hotneim.github.io/ban400/assignment-01.html

# Load necessary libraries
library(readxl)
library(dplyr)

# 1: Read dataset
data_nasdaq <- read_excel("data-nasdaq-returns.xls")
head(data_nasdaq)

# 2: Custom data frame
selected_data <- data_nasdaq %>% select(Date, NASDAQ, AAPL)
head(selected_data)

# 3: Scatterplot
plot(selected_data$NASDAQ, selected_data$AAPL, 
     xlab="NASDAQ Returns", 
     ylab="Apple Returns", 
     main="Scatterplot of NASDAQ vs. Apple Returns")

#4: Create sign columns
# function mutate is used to add new columns (variables) to the data frame
selected_data <- selected_data %>%
  mutate(sign_NASDAQ = sign(NASDAQ),
         sign_AAPL = sign(AAPL))
head(selected_data)

#5: 
selected_data <- selected_data %>% 
  mutate(sum = (sign_NASDAQ+sign_AAPL)/2)
head(selected_data)

#6: 
table(selected_data$sum)

#7: Write all lines of code under one singel operation
final_data <- read_excel("data-nasdaq-returns.xls") %>%
  select(Date, NASDAQ, AAPL) %>%
  mutate(sign_NASDAQ = sign(NASDAQ),
         sign_AAPL = sign(AAPL),
         sum = (sign_NASDAQ + sign_AAPL) / 2)
head(final_data)


