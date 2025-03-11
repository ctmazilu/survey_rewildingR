# Sun Feb 23rd 
# Survey Analysis

install.packages("readxl", "dplyr")
install.packages("likert")


# Load necessary libraries
library(likert)
library(dplyr)
library(ggplot2)
library(readxl)


# Read the data
data <- read_excel("Policy, Place & Participation_ Community Action in Fife Rewilding Projects(1-101).xlsx", sheet = "Sheet1") 
data$Education

data1$Age_group
age <- data1[data1$Age_group =="18-24", ]
head(age)

head(data)

str(data)

table(data$Gender)
table(data$Education)
prop.table(table(data$Gender))

table(data$Gardens)

prop.table(table(data$Gardens))

# Summary for one question
summary(data$Q14_1)

# Create a bar plot
ggplot(data, aes(x = Farmland)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Response Distribution", x = "Responses", y = "Frequency")

# Q1: Fife is wild because it contains natural coastal landscapes.
# Q2: Fife is wild due to its expansive green spaces, including forests and farmland
# Q3: Fife is wild because it provides habitats for wildlife.
# Q4: Fife is less wild than the Scottish Highlands.
# Q5: Fife is not wild because it is predominantly an agricultural region.
# Q6: Fife is not wild because it is a residential region with urban infrastructure.  


# Recode Likert responses manually using `factor()`
questions <- data.frame(
  # Positive
  Q1 = as.numeric(factor(data$Q14_1, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q3 = as.numeric(factor(data$Q14_3, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  # Negative 
  Q4 = as.numeric(factor(data$Q14_4, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)), # Negative
  Q5 = as.numeric(factor(data$Q14_5, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)), # Negative
  Q6 = as.numeric(factor(data$Q14_6, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5))  # Negative
)

print(questions)

# Reverse code negative items
inversequestions <- questions %>%
  mutate(Q4 = 6 - Q4,
         Q5 = 6 - Q5,
         Q6 = 6 - Q6)

# View the modified dataset
print(inversequestions)

# Convert Likert items back to factors with the same levels
questiondata <- inversequestions %>%
  mutate(across(everything(), ~factor(.,
                                      levels = c(1, 2, 3, 4, 5),
                                      labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), 
                                      ordered = TRUE)))
# Check the structure
str(questiondata)
# Check for missing data in your Likert data
summary(questiondata)

# Rename columns to reflect the statements about Fife
colnames(questiondata) <- c(
  "Fife is wild because it contains natural coastal landscapes.",
  "Fife is wild because it provides habitats for wildlife.",
  "Fife is more wild than the Scottish Highlands.",
  "Fife is wild because it is predominantly an agricultural region.",
  "Fife is wild because it is a residential region with urban infrastructure."
)

# Now you can use the likert function
library(likert)

likert_data <- likert(questiondata)

# Check the results
likert_plot <- plot(likert_data, legend.position="right")

print(likert_plot)

ggsave("likertQ14.png", width = 10, height = 6, bg = "white", plot = last_plot(), dpi = 300)


fdaff_likert <- data.frame(
  gender=factor(data$Gender), 
  Q1=factor(data$Q14_1,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q3=factor(data$Q14_3,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q4=factor(data$Q14_4,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q5=factor(data$Q14_5,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q6=factor(data$Q14_6,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
)

# Filter the data to include only "Man" and "Woman"
fdaff_likert <- fdaff_likert %>%
  filter(gender %in% c("Man", "Woman"))

# Rename the columns to reflect the specific statements
colnames(fdaff_likert)[2:6] <- c(
  "Fife is wild because it contains natural coastal landscapes.",
  "Fife is wild because it provides habitats for wildlife.",
  "Fife is more wild than the Scottish Highlands.",
  "Fife is wild because it is predominantly an agricultural region.",
  "Fife is wild because it is a residential region with urban infrastructure."
)


fdaff_likert2 <- likert(items=fdaff_likert[,2:6], grouping=fdaff_likert[,1])
plot(fdaff_likert2)

ggsave("likertQ14GENDER.png", width = 10, height = 6, bg = "white", plot = last_plot(), dpi = 300)



fdaff_likert <- data.frame(
  education=factor(data$Education), 
  Q1=factor(data$Q14_1,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q3=factor(data$Q14_3,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")),
  Q6=factor(data$Q14_6,levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
)



# Rename the columns to reflect the specific statements
colnames(fdaff_likert)[2:4] <- c(
  "Fife is wild because it contains natural coastal landscapes.",
  "Fife is wild because it provides habitats for wildlife.",
  "Fife is wild because it is a residential region with urban infrastructure."
)

# Filter the data to include only "Man" and "Woman"
fdaff_likert <- fdaff_likert %>%
  filter(education %in% c("Apprenticeship (Trade/technical/vocational training)", 
                       "University degree/postgraduate qualifications", 
                       "Secondary/High school", 
                       "No qualifications"))


# Recreate the factor levels for education
#fdaff_likert$education <- factor(fdaff_likert$education, 
#                                 levels = c("Apprenticeship", 
#                                            "University degree", 
#                                            "Secondary school", 
#                                            "No qualifications"))


fdaff_likert2 <- likert(items=fdaff_likert[,2:4], grouping=fdaff_likert[,1])
plot(fdaff_likert2, wrap.grouping = 90) 

print(summary(fdaff_likert2))

likert_education <- data %>% 
  group_by(Education) %>% 
  summarise(across(starts_with("F"), list())) %>% 
  unnest(cols = everything()) %>% 
  likert()

plot(likert_education)

unique_education_values <- unique(data$Education)
print(unique_education_values)



#### species re-introduction ####
# Q15_1: Reintroduction of wildcats to Fife are essential to rewilding.
# Q15_2: Wildcats are unlikely to attack humans.
# Q15_3: Reintroduction of beavers to Tayside are essential to rewilding.
# Q15_4: Rewilding is dangerous because it increases the risk of human- wildlife conflicts.
# Q15_5: Rewilding creates habitat corridors to help protect native species.
# Q15_6: Reintroduction of native wildflowers in parks are essential to rewilding.



# Recode Likert responses manually using `factor()`
questions <- data.frame(
  # Positive
  Q1 = as.numeric(factor(data$Q15_1, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q3 = as.numeric(factor(data$Q15_3, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q5 = as.numeric(factor(data$Q15_5, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)), 
  Q6 = as.numeric(factor(data$Q15_6, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5))  
)

print(questions)

# Reverse code negative items
#inversequestions <- questions %>%
#  mutate(Q4 = 6 - Q4)

# View the modified dataset
print(inversequestions)

# Convert Likert items back to factors with the same levels
questiondata <- questions %>%
  mutate(across(everything(), ~factor(.,
                                      levels = c(1, 2, 3, 4, 5),
                                      labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), 
                                      ordered = TRUE)))
# Check the structure
str(questiondata)
# Check for missing data in your Likert data
summary(questiondata)

# Rename columns to reflect the statements about Fife
colnames(questiondata) <- c(
  "Reintroduction of wildcats to Fife are essential to rewilding.",
  "Reintroduction of beavers to Tayside are essential to rewilding.",
  "Rewilding creates habitat corridors to help protect native species.",
  "Reintroduction of native wildflowers in parks are essential to rewilding."
)

# Now you can use the likert function
library(likert)

likert_data <- likert(questiondata)

# Check the results
likert_plot <- plot(likert_data, legend.position="right")

print(likert_plot)

ggsave("likertQ15b.png", width = 10, height = 6, bg = "white", plot = last_plot(), dpi = 300)



#### People in rewilding ####

# Q16_1: The key to rewildings' success is people
# Q16_2: Rewilding projects should be done working with local communities.
# Q16_3: Rewilding is harmful because it prioritises wildlife over the needs of local communities.
# Q16_4: Rewilding does not embrace the role of people
# Q16_5: Rewilding is only possible by removing people from land.
# Q16_6: Rewilding is a way to revitalise rural economies and communities.
# Q16_7: Rewilding enhances peoples connections to the land.


# Recode Likert responses manually using `factor()`
questions <- data.frame(
  # Positive
  Q1 = as.numeric(factor(data$Q16_1, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q2 = as.numeric(factor(data$Q16_2, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q3 = as.numeric(factor(data$Q16_3, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)), 
  Q4 = as.numeric(factor(data$Q16_4, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),  
  Q5 = as.numeric(factor(data$Q16_5, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)),
  Q6 = as.numeric(factor(data$Q16_6, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5)), 
  Q7 = as.numeric(factor(data$Q16_7, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), labels = 1:5))  
  )

print(questions)

# Reverse code negative items
inversequestions <- questions %>%
  mutate(Q3 = 6 - Q3,
         Q4 = 6 - Q4,
         Q5 = 6 - Q5)

# View the modified dataset
print(inversequestions)

# Convert Likert items back to factors with the same levels
questiondata <- inversequestions %>%
  mutate(across(everything(), ~factor(.,
                                      levels = c(1, 2, 3, 4, 5),
                                      labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), 
                                      ordered = TRUE)))
# Check the structure
str(questiondata)
# Check for missing data in your Likert data
summary(questiondata)

# Rename columns to reflect the statements about Fife
colnames(questiondata) <- c(
  "The key to rewildings' success is people.",
  "Rewilding projects should be done working with local communities.",
  "Rewilding prioritises local communities over the needs of wildlife.",
  "Rewilding embraces the role of people.",
  "Rewilding is possible without removing people from land.",
  "Rewilding is a way to revitalise rural economies and communities.",
  "Rewilding enhances peoples connections to the land."
)

# Now you can use the likert function
library(likert)

likert_data <- likert(questiondata)

# Check the results
likert_plot <- plot(likert_data, legend.position="right")

print(likert_plot)

ggsave("likertQ16.png", width = 10, height = 6, bg = "white", plot = last_plot(), dpi = 300)





