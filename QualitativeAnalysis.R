# Word Cloud

install.packages("wordcloud")   # Word cloud generation package
install.packages("RColorBrewer") # Color palettes for word clouds
install.packages("tidyverse")
install.packages("text")
install.packages("wordcloud2")
install.packages("textstem")
install.packages("tidytext")


library(wordcloud)
library(RColorBrewer)
library(readxl)
library(tidyverse)
library(text)
library(wordcloud2)
library(tidytext)
library(textstem)


data <- read_excel("Policy, Place & Participation_ Community Action in Fife Rewilding Projects(1-101).xlsx", sheet = "Sheet1") 

responses <- data$Q9 # Replace with the actual column name
print(responses)
# Remove NA or empty responses


# Clean and preprocess text
cleaned_data <- tibble(text = responses) %>%
  filter(!is.na(text) & nchar(text) > 1) %>%  # Remove NA and very short text
  mutate(text = tolower(text),  # Convert to lowercase
         text = str_replace_all(text, "[[:punct:]]", ""),  # Remove punctuation
         text = str_replace_all(text, "[[:digit:]]", ""),  # Remove digits
         text = str_squish(text))  # Remove extra whitespace

# Tokenize the text into words
tokens <- cleaned_data %>%
  unnest_tokens(word, text)  # Tokenize into individual words

# Custom mapping to treat "reintroduction" as "reintroduce"
tokens$word <- ifelse(tokens$word == "reintroduction", "reintroduce", tokens$word)
tokens$word <- ifelse(tokens$word %in% c("management", "managing"), "manage", tokens$word)

# Lemmatize the words
tokens$word <- lemmatize_words(tokens$word)

# Print the updated tokens
print(tokens)

# Remove stop words
data_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Count word frequencies
word_freq <- data_clean %>%
  count(word, sort = TRUE)

# Select top 25 words
top_words <- head(word_freq, 25)
print(n = 30, top_words)

##### WORDCLOUD ######
# Define a color palette
#palette <- brewer.pal(n = nrow(top_words), name = "Set5")

# Create the word cloud with the color palette
#wordcloud2(top_words, color = palette)

# Generate the word cloud with top 25 words
wordcloud2(top_words)

#### REWILDING #####

responses <- data$Q15 # Replace with the actual column name
print(responses)
# Remove NA or empty responses


# Clean and preprocess text
cleaned_data <- tibble(text = responses) %>%
  filter(!is.na(text) & nchar(text) > 1) %>%  # Remove NA and very short text
  mutate(text = tolower(text),  # Convert to lowercase
         text = str_replace_all(text, "[[:punct:]]", ""),  # Remove punctuation
         text = str_replace_all(text, "[[:digit:]]", ""),  # Remove digits
         text = str_squish(text))  # Remove extra whitespace

# Tokenize the text into words
tokens <- cleaned_data %>%
  unnest_tokens(word, text)  # Tokenize into individual words

# Custom mapping to treat "reintroduction" as "reintroduce"
tokens$word <- ifelse(tokens$word == "reintroduction", "reintroduce", tokens$word)
tokens$word <- ifelse(tokens$word %in% c("management", "managing"), "manage", tokens$word)

# Lemmatize the words
tokens$word <- lemmatize_words(tokens$word)

# Print the updated tokens
print(tokens)

# Remove stop words
data_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Count word frequencies
word_freq <- data_clean %>%
  count(word, sort = TRUE)

# Select top 25 words
top_words <- head(word_freq, 25)
print(n = 30, top_words)

##### WORDCLOUD ######
# Define a color palette
#palette <- brewer.pal(n = nrow(top_words), name = "Set5")

# Create the word cloud with the color palette
#wordcloud2(top_words, color = palette)

# Generate the word cloud with top 25 words
wordcloud2(top_words)

# Generate the word cloud
# Word cloud for Question 2
library("wordcloud")
wordcloud(top_words, 
          min.freq = 1, 
          max.words = 25,
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

#wordcloud2(top_words, size = 1.5, minSize = 0.5,, backgroundColor = "white")
##### SENTIMENT #####

sentiment <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, sort = TRUE)

print(sentiment)

sentiment_data <- cleaned_data %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") %>%
  group_by(sentiment) %>%
  summarise(word_count = n_distinct(word)) %>%
  arrange(desc(word_count))

print(sentiment_data)

# Visualize the sentiment distribution
ggplot(sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis of Wilderness Discussions", x = "Sentiment", y = "Frequency")

#### TOPIC MODELS #####
install.packages("topicmodels")
library(topicmodels)

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(data_clean)

# Fit LDA model with a specified number of topics
lda_model <- LDA(dtm, k = 2)  # 2 topics
topics <- tidy(lda_model, matrix = "beta")
#topics <- tidy(lda_model, matrix = "gamma")  # Gamma represents the topic distribution for each document
print(topics)

# View the top 10 terms for each topic
topics %>%
  group_by(topic) %>%
  top_n(3, beta) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Terms", y = "Beta (Word-Topic Probability)", title = "Top Terms for Each Topic") +
  theme_minimal()


##### WORD FREQ #####

top_words %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Word Frequencies")

ggsave("wordfeqQ9.png", width = 10, height = 6, bg = "white")


ngrams <- cleaned_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

ngrams %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Word Frequencies")

#### COLLOCATION #####
# Assuming 'cleaned_data' is your dataframe and 'text' is the column containing the text

bigrams <- cleaned_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  # Use n = 2 for bigrams

# View the first few bigrams
head(bigrams)

# View the top bigrams
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

head(bigram_freq, 10)  # Show the top 10 most frequent bigrams

# Filter for the most frequent bigrams
top_bigrams <- bigram_freq %>%
  filter(n > 5)  # Adjust the threshold based on your data

# View the results
head(top_bigrams)

# Visualize bigrams using a word cloud
library(wordcloud)
wordcloud(words = top_bigrams$bigram, freq = top_bigrams$n, min.freq = 1)

# Extract trigrams
trigrams <- cleaned_data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

# Count trigram frequencies
trigram_freq <- trigrams %>%
  count(trigram, sort = TRUE)
  filter(n >= 1)  # Filter bigrams that appear at least 5 times

# Visualize the top trigrams
wordcloud(words = trigram_freq$trigram, freq = trigram_freq$n, min.freq = 1)

wilderness_bigrams <- bigrams %>%
  filter(str_detect(bigram, "human"))  # Filter for bigrams containing 'wilderness'

# Visualize the most common bigrams containing 'wilderness'
wilderness_bigrams %>%
  count(bigram, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Bigrams", y = "Frequency", title = "Top Bigrams Associated with Wilderness")

### NETWORK GRAPH ####
install.packages("igraph")    
library(igraph)

# Create a network graph of words associated with wilderness
wilderness_network <- trigrams %>%
  filter(str_detect(trigram, "land")) %>%
  count(trigram) %>%
  graph_from_data_frame()

# Visualize the network graph
plot(wilderness_network, vertex.size = 10, vertex.label.cex = 0.8)
