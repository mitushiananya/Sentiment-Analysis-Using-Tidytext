# all the required packages
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(dplyr)

# Tidytext will allow us to perform efficient text analysis on our data. Tidytext package that comprises of sentiment lexicons that are present in the dataset of ‘sentiments’
sentiments

# There are three general purpose lexicons: 1. AFINN 2. bing 3. loughran
# We will make use of bing lexicon
get_sentiments("bing")

# The janeaustenr package will provide us with the textual data in the form of books authored by Jane Austen
# We will convert the text of our books into a tidy format using unnest_tokens() function
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Making use of the “bing” lexicon to and implement filter() over the words that correspond to joy
# Using the book "Sense and Sensibility" and derive its words to implement the sentiment analysis model
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_data %>%
  filter(book == "Sense and Sensibility") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

# Using spread() function to segregate our data into separate columns of positive and negative sentiments 
# Using  the mutate() function to calculate the total sentiment, that is, the difference between positive and negative sentiment
bing <- get_sentiments("bing")
SS_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Sense and Sensibility" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Visualizing the words present in the book “Sense and Sensibility” based on their corresponding positive and negative scores
ggplot(SS_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Counting the most common positive and negative words that are present in the novel
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

# Visualization of our sentiment score
# Plotting the scores along the axis that is labeled with both positive as well as negative words against the sentiment score 
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

# Creating wordcloud
# cUsing omparision.cloud() function to plot both negative and positive words in a single wordcloud
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("dark blue", "orange"),
                   max.words = 100)


