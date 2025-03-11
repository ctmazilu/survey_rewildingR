#Multiple Choice Analysis
install.packages("readxl")  # If not installed
install.packages("ggplot2") # If not installed

library(readxl)
library(ggplot2)
library(dplyr)

# Manually entering the data into a data frame
dataQ20 <- data.frame(
  Category = c(
    "Economic opportunities", 
    "Ecosystem recovery", 
    "Education and research opportunities", 
    "Enhanced green spaces", 
    "Connection between people", 
    "Connection with nature", 
    "Improved farmland", 
    "Flood protection", 
    "No benefits", 
    "Other"
  ),
  Count = c(50, 91, 84, 82, 69, 96, 48, 74, 3, 7)
)

# Calculate total responses
total_responses <- sum(dataQ20$Count)

# Add a percentage column
dataQ20$Percentage <- (dataQ20$Count / 101) * 100

# Frequency distribution (Count)
frequency_benefits <- dataQ20 %>%
  select(Category, Count)

# Percentage distribution
percentage_benefits <- dataQ20 %>%
  select(Category, Percentage)

# Descriptive statistics
summary_benefits <- dataQ20 %>%
  summarise(
    Mean = mean(Count),
    Median = median(Count),
    SD = sd(Count),
    Min = min(Count),
    Max = max(Count)
  )

# Print the results
print(frequency_benefits)
print(percentage_benefits)
print(summary_benefits)


ggplot(dataQ20, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  
  # Add labels inside the bars
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5),  # Center labels inside bars
            color = "black",  # White text for visibility
            size = 5) +  
  
  labs(title = "Perceived Benefits of Rewilding", 
       x = "Category", 
       y = "Number of Respondents") +
  
  theme_minimal() +
  coord_flip() +  # Flip for readability
  #scale_fill_brewer(palette = "Set2") +  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 10))

ggsave("barchartQ20.png", width = 10, height = 6, bg = "white")



###### Q20 #####

# Create the new data frame for challenges
dataQ21 <- data.frame(
  Category = c(
    "Disagreements over land use", 
    "'Messy'/unmanaged green spaces", 
    "Concern for lyme disease", 
    "Lack of awareness of rewilding projects", 
    "Financial constraints for participants", 
    "Loss of farmland", 
    "Lack of connection between people", 
    "Lack of education and research opportunities", 
    "Lack of funding", 
    "No challenges", 
    "Other"
  ),
  Count = c(87, 53, 46, 69, 40, 43, 9, 12, 66, 2, 4)
)


# Calculate total responses
total_responses <- sum(dataQ21$Count)

# Add a percentage column
dataQ21$Percentage <- (dataQ21$Count / 101) * 100

# Descriptive statistics
summary_benefits <- dataQ21 %>%
  summarise(
    Mean = mean(Count),
    Median = median(Count),
    SD = sd(Count),
    Min = min(Count),
    Max = max(Count)
  )

# Print the results
print(summary_benefits)

ggplot(dataQ21, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  
  # Add labels inside the bars
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5),  # Center labels inside bars
            color = "black",  # White text for visibility
            size = 5) +  
  
  labs(title = "Perceived Challanges of Rewilding", 
       x = "Category", 
       y = "Number of Respondents") +
  
  theme_minimal() +
  coord_flip() +  # Flip for readability
  #scale_fill_brewer(palette = "Set2") +  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 10))

ggsave("barchartQ21.png", width = 10, height = 6, bg = "white")


###### Q20 #####

# Create the new data frame for challenges
dataQ22 <- data.frame(
  Category =  c("Landowners", "Local communities", "Policy-advisors", "Scientists", "NGOs", "Farmers", "Other"),
  Count = c(81, 92, 42, 67, 34, 77, 8)
)


# Calculate total responses
total_responses <- sum(dataQ22$Count)

# Add a percentage column
dataQ22$Percentage <- (dataQ22$Count / 101) * 100

# Descriptive statistics
summary_benefits <- dataQ22 %>%
  summarise(
    Mean = mean(Count),
    Median = median(Count),
    SD = sd(Count),
    Min = min(Count),
    Max = max(Count)
  )

# Print the results
print(summary_benefits)

ggplot(dataQ22, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  
  # Add labels inside the bars
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5),  # Center labels inside bars
            color = "black",  # White text for visibility
            size = 5) +  
  
  labs(title = "Which stakeholder groups should be most involved in rewilding decisions?", 
       x = "Category", 
       y = "Number of Respondents") +
  
  theme_minimal() +
  coord_flip() +  # Flip for readability
  #scale_fill_brewer(palette = "Set2") +  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 10))

ggsave("barchartQ22.png", width = 10, height = 6, bg = "white")
