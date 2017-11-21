# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load Jane Austen books into a dataframe using the austen_books() function
books <- austen_books()

# How many books are in the dataset?
num.books <- length(unique(books$book))

# Which book has the most lines?
most.lines <- books %>% 
  group_by(book) %>% 
  summarize(lines = n()) %>% 
  filter(lines == max(lines)) %>% 
  select(book)

# Use the unnest_tokens function to generate the full list of words
word.list <- books %>% unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
all.words <- word.list %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- all.words %>% 
  anti_join(stop_words, by="word")

# Which non stop-words are most common?
no.stop.words <- no.stop.words %>%
  arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no.stop.words %>% 
  filter(count > 800) %>% 
  mutate(word = reorder(word, count)) %>% 
  ggplot(aes(word, count)) +
    geom_col() + 
    coord_flip()
