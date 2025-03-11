# For Binary Choice Questions

library(RColorBrewer)
# Load necessary libraries
library(ggplot2)
library(dplyr)


data <- read_excel("Policy, Place & Participation_ Community Action in Fife Rewilding Projects(1-101).xlsx", sheet = "Sheet1") 
response <- table(data$Q10)

# Calculate percentages
response_percentages <- round(100 * response / sum(response), 1)
# Create custom labels with frequencies and percentages, including a percentage sign
labels <- paste(names(response), "\n", response, " (", response_percentages, "%)", sep="")

# Proportions of "Yes" and "No"
response_proportions <- prop.table(response)
print(response_proportions)


# Create the pie chart
pie(response, 
    labels = labels, 
    col = c("lightblue", "lightgreen"), 
    main = "Responses to \n Q10:Do you consider Fife as 'wild'? \n Yes or No")

response <- table(data$Q11)

# Calculate percentages
response_percentages <- round(100 * response / sum(response), 1)
# Create custom labels with frequencies and percentages, including a percentage sign
labels <- paste(names(response), "\n", response, " (", response_percentages, "%)", sep="")

# Create the pie chart
pie(response, 
    labels = labels, 
    col = c("lightblue", "lightgreen"), 
    main = "Responses to \n Q11:Do you think Fife's landscape should become 'wilder'? \n Yes or No")



# Count the responses for Q10 and calculate percentages
response_counts <- data %>%
  group_by(Q10) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(100 * Frequency / sum(Frequency), 1))

# Create the pie chart
ggplot(response_counts, aes(x = "", y = Frequency, fill = Q10)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste(Frequency, " (", Percentage, "%)", sep = "")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Do you consider Fife as 'wild'?") +
  theme_void()  # Remove background and axes


ggsave("piechartQ10.png", width = 10, height = 6, bg = "white")

# Count the responses for Q10 and calculate percentages
response_counts <- data %>%
  group_by(Q11) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(100 * Frequency / sum(Frequency), 1))

# Create the pie chart
ggplot(response_counts, aes(x = "", y = Frequency, fill = Q11)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste(Frequency, " (", Percentage, "%)", sep = "")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Do you think Fife's landscape should become 'wilder'?") +
  theme_void()  # Remove background and axes


ggsave("piechartQ11.png", width = 10, height = 6, bg = "white")
