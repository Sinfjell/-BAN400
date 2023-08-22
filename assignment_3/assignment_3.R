library(readr)
library(dplyr)
library(ggplot2)

plastic <- read_csv("data-plastic.csv")

# Step 1: Basic Scatterplot
p1 <- plastic |> 
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged))) +
  geom_point()

# Step 2: Make the dots reflect country size and continent
p2 <- plastic |> 
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged), 
             size = population, color = region)) +
  geom_point() 

# Step 3: Fix Labels, Add Theme, Change Color Palette
# store 11 colours in my color palette
my_colors <- c("#E63946", "#F1FAEE", "#A8DADC", 
               "#457B9D", "#1D3557", "#F4A261", 
               "#2A9D8F", "#E76F51", "#2B9348", 
               "#F9C74F", "#577590")

p3 <- p2 + labs(y = "Per capita mismanaged plastic waste (kg/day, log)", 
          x = "Per capita GDP (log)") + 
  theme_classic() + 
  scale_color_manual(values = my_colors)
p3

# Step 4: Add text labels for the countries and a smoother
# Filter out rows with missing or infinite values in the relevant variables
plastic_filtered <- plastic |> 
  filter(!is.infinite(gdp), !is.na(gdp),
         !is.infinite(per_capita_mismanaged), !is.na(per_capita_mismanaged),
         !is.na(population), !is.na(region))

# Create the plot using the filtered data
p4 <- plastic_filtered %>%
  ggplot(aes(x = log(gdp), y = log(per_capita_mismanaged), 
             size = population, color = region)) +
  geom_point() +
  labs(y = "Per capita mismanaged plastic waste (kg/day, log)", 
       x = "Per capita GDP (log)") +
  theme_classic() +
  scale_color_manual(values = my_colors) +
  geom_text(aes(label = country), size = 2) +
  geom_smooth(method = "gam", color = "black", se = TRUE) # Add a linear smoother with a confidence interval
p4


