library(dplyr)
library(ggplot2)
library(tidytext)

#set up color palette for each decade
my_colors <- c("#758c33", "#f8ca38", "#ca7cd8", "#287e9e", "#ff6200", "#fe3c71")

#create custom theme
theme_songs <- function(aticks = element_blank(),
                        gminor = element_blank(),
                        gmajor = element_blank(),
                        lp = "none") 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = aticks,
        panel.grid.minor = gminor,
        panel.grid.major = gmajor,
        legend.position = lp)
}

#read in data
setwd("~/Spring 2019 Classes/BANA8083-Case Study")
songs <- read.csv(paste(getwd(), "/data/final_song_data.csv", sep = ""), stringsAsFactors = FALSE)

#quickly examine data
dim(songs)
str(songs)

#add decades column
songs <- songs %>%
  mutate(decade = 
           ifelse(songs$year == 1959, "1950s",
           ifelse(songs$year %in% 1960:1969, "1960s",
           ifelse(songs$year %in% 1970:1979, "1970s",
           ifelse(songs$year %in% 1980:1989, "1980s",
           ifelse(songs$year %in% 1990:1999, "1990s",
           ifelse(songs$year %in% 2000:2009, "2000s",
           ifelse(songs$year %in% 2010:2019, "2010s", NA))))))))

#make sure no songs are NA
sum(is.na(songs$decade))

########################
#descriptive statistics#
########################
#how many songs each decade?
songs %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, fill = decade), stat = "identity") +
  theme_songs() +
  scale_fill_manual(values = c("red", my_colors)) +
  ggtitle("Number 1 Hits by Decade") +
  labs(x = NULL, y = "Song Count")




#############
#text mining#
#############
#first remove unnecessary words - list used from Debbie Liske
undesirable_words <- c("chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

#also need to remove stop_words
head(sample(stop_words$word, 15), 15)

#songs filtered is unnested, tokenization of unique words in lyrics
songs_filtered <- songs %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

class(songs_filtered)
dim(songs_filtered)

songs_filtered %>%
  filter(word == "love") %>%
  select(word, song, artist, year) %>%
  arrange() %>%
  top_n(10, song)

#word count by decade - full length
full_word_count <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, decade, year) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#songs with most total words
full_word_count[1:10,] %>%
  select(song, num_words, year)

#songs with least total words
full_word_count %>%
  filter(!num_words == 1) %>%
  tail(10) %>%
  select(song, num_words, year) %>%
  arrange(num_words)

#visualize word count distribution
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = decade), binwidth = 25) +
  ylab("Song Count") +
  xlab("Total number of words") +
  ggtitle("Distribution of total words per song") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank())

#summary statistics
mean(full_word_count$num_words)
sd(full_word_count$num_words)

#lets repeat above steps, but only looking at unique words
partial_word_count <- songs %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  group_by(song, decade, year) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#songs with most unique, distinct words
partial_word_count[1:10,] %>%
  select(song, num_words, year)

#songs with least total words
partial_word_count %>%
  filter(!num_words == 1) %>%
  tail(10) %>%
  select(song, num_words, year) %>%
  arrange(num_words)

#visualize
partial_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words), binwidth = 5) +
  ylab("Song Count") +
  xlab("Total number of words") +
  ggtitle("Distribution of total words per song") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank())

#most frequently used words
songs_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

#popular words
popular_words <- songs_filtered %>%
  group_by(decade) %>%
  filter(!decade == "1950s") %>%
  count(word, decade, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot() +
  geom_col(aes(row, n, fill = decade), show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Decade") +
  facet_wrap(~decade, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = popular_words$row, labels = popular_words$word) + 
  coord_flip()

#word lengths
word_lengths <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word))

word_lengths %>%
  filter(!decade == "1950s") %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), binwidth = 10) +
  geom_histogram(aes(fill = decade), breaks = seq(1, 25, by = 2), show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  facet_wrap(~decade, scales = "free") +
  xlab("Word Length") +
  ylab("Song Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

#lexical diversity - number of total unique words
lex_div <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))

lex_div %>%
  ggplot(aes(year, lex_diversity)) +
  geom_point(color = "forestgreen", alpha = 0.4, size = 3, position = "jitter") +
  geom_smooth(aes(x = year, y = lex_diversity), se = FALSE, color = "blue") +
  ggtitle("Lexical Diversity") +
  xlab('') +
  ylab('') +
  theme(plot.title = element_text(hjust = 0.5))

#lexical density - unique words / total words, shows reptition
lex_den <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, year) %>%
  summarise(lex_density = n_distinct(word) / n()) %>%
  arrange(desc(lex_density))

lex_den %>%
  ggplot(aes(year, lex_density)) +
  geom_point(color = "red", alpha = 0.4, size = 3, position = "jitter") +
  stat_smooth(color = "black", method = "lm", se = FALSE) +
  geom_smooth(aes(x = year, y = lex_density), se = FALSE, color = "blue") +
  ggtitle("Lexical Density") +
  xlab('') +
  ylab('') +
  theme(plot.title = element_text(hjust = 0.5))

############
###TF-IDF###
############
#IDF attaches lower weights to commonly used words + vice versa
#TF: term frequency, number of times term occurs in document
#DF: document frequency, number of documents with a word
#IDF: 1/DF
#TF-IDF = TF * IDF
popular_tfidf <- songs %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  filter(!decade == "1950s") %>%
  count(decade, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, decade, n)

head(popular_tfidf)

top_popular_tfidf <- popular_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(decade) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf %>%
  ggplot(aes(x = row, tf_idf, fill = decade)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  xlab(NULL) +
  ylab("TF-IDF") +
  ggtitle("Important Words by TF-IDF Level by Decade") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~decade, scales = "free") +
  scale_x_continuous(breaks = top_popular_tfidf$row,
                     labels = top_popular_tfidf$word) +
  coord_flip()