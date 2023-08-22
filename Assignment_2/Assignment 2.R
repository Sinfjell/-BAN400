library(foreign)
library(dplyr)

df <- 
  read.dta("data-ESS8NO.dta") %>%
  transmute(
    party = prtvtbno, 
    age = agea, 
    religiosity = rlgdgr, 
    income_decile = hinctnta)

# Problem 1: Age Summary by Party
# This section calculates the mean and standard devation for each party
# Group data by party and calculate mean age, standard deviation, and count for each party

age_summary <- df %>%
  group_by(party) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >= 25) 


# Identify the party with the oldest voters
oldest_voters_party <- age_summary$party[which.max(age_summary$mean_age)]

# Identify the party with the youngest voters
youngest_voters_party <- age_summary$party[which.min(age_summary$mean_age)]

# Identify the party with the largest standard deviation of voters' age
largest_sd_party <- age_summary$party[which.max(age_summary$sd_age)]

# Print the results
print(paste("Party with the oldest voters:", oldest_voters_party))
print(paste("Party with the youngest voters:", youngest_voters_party))
print(paste("Party with the largest standard deviation of voters' age:", largest_sd_party))
#--------------------------

# Problem 2: Handling Non-Responses
# Identify unique responses in religiosity, income_decile, and party columns
# Filter out non-responses and count them
unique_religiosity_responses <- unique(df$religiosity)
unique_income_responses <- unique(df$income_decile)
unique_party_responses <- unique(df$party)
print("Unique responses in the 'religiosity' column:")
print(unique_religiosity_responses)
print("Unique responses in the 'income_decile' column:")
print(unique_income_responses)
print("Unique responses in the 'party' column:")
print(unique_party_responses)

# Store non-response codes in a vector
non_response_codes <- c("Don't know", "No answer",  "Refusal", 
                        "other", "Not applicable", "Very religious", 
                        "Not at all religious")

filtered_df <- df %>%
  filter(
    !party %in% non_response_codes,
    !income_decile %in% non_response_codes,
    !religiosity %in% non_response_codes
  )


# Counting non-responses
non_responses <- df %>%
  summarise(
    no_response_party = sum(party %in% non_response_codes),
    no_response_income = sum(income_decile %in% non_response_codes),
    no_response_religiosity = sum(religiosity %in% non_response_codes),
    answered_both_party_income = sum(!party %in% non_response_codes & !income_decile %in% non_response_codes)
  )

#------------------------

# Problem 3:  Average Religiosity by Party
# Check the data type of the 'religiosity' column
religiosity_class <- class(df$religiosity)

# Print the result
print(paste("Data type of the 'religiosity' column:", religiosity_class))

# Convert the 'religiosity' column from factor to numeric
filtered_df$religiosity <- as.numeric(as.character(filtered_df$religiosity))

average_religiosity <- filtered_df |> 
  group_by(party) |> 
  summarise(avg_religiosity = mean(religiosity, na.rm = TRUE)) |> 
  arrange(desc(avg_religiosity))

# prints the end output for problem 3
print(average_religiosity)

# Problem 4: High-income to Low-income Ratio
# # Extract the decile number from the income_decile column
filtered_df$income_decile_num <- as.numeric(gsub(".*\\s-\\s(\\d+).*", "\\1", 
     filtered_df$income_decile))

# Only parties with more than 75 voters
filtered_parties <- filtered_df %>%
  group_by(party) %>%
  filter(n() > 75)

# Calculate the income_ratio variable
income_ratio <- filtered_parties %>%
  group_by(party) %>%
  summarise(
    high_income = sum(income_decile_num %in% c(9, 10), na.rm = TRUE),
    low_income = sum(income_decile_num %in% c(1, 2), na.rm = TRUE),
    ratio = ifelse(low_income > 0, high_income / low_income, NA)
  ) %>%
  filter(!is.na(ratio))

highest_ratio_party <- income_ratio$party[which.max(income_ratio$ratio)]

print(paste("Party with the highest high- to low-income voters:", highest_ratio_party))
cat("For every low-income voter, there are", max(income_ratio$ratio), 
    "high-income voters" )



