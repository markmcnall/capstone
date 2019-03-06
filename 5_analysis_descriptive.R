library(dplyr)
library(ggplot2)
library(tidytext)

#set up color palette for each decade
my_colors <- c("#e81b23", "#758c33", "#f8ca38", "#ca7cd8", "#287e9e", "#ff6200", "#fe3c71")

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
  scale_fill_manual(values = my_colors) +
  ggtitle("Number 1 Hits by Decade") +
  labs(x = NULL, y = "Song Count")

#songs per year
num_songs_year <- songs %>%
  select(song, year, decade) %>%
  group_by(year, decade) %>%
  summarise(song_count = n())

#radial chart - songs per year
#!!!!!!!!!!!need to fix
id <- seq_len(nrow(songs_year))
songs_year <- cbind(songs_year, id)
label_data = songs_year
number_of_bar = nrow(label_data) #Calculate the ANGLE of the labels
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar #Center things
label_data$hjust <- ifelse(angle < -90, 1, 0) #Align label
label_data$angle <- ifelse(angle < -90, angle + 180, angle) #Flip angle
ggplot(songs_year, aes(x = as.factor(id), y = song_count)) +
  geom_bar(stat = "identity", fill = alpha("purple", 0.7)) +
  geom_text(data = label_data, aes(x = id, y = song_count + 10, label = year, hjust = hjust), color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-20, 150) + #Size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))

#same thing, but more traditional plot
num_songs_year %>%
  ggplot(aes(year, song_count, fill = decade)) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE, aes(group = 1), color = "#5d5f63") +
  scale_fill_manual(values = my_colors) +
  theme_songs() +
  ggtitle("#1 Hits Per Year") +
  labs(x = NULL, y = "Song Count")

#lowest and highest years
num_songs_year %>%
  arrange(song_count) %>%
  head()

songs %>%
  filter(year == 2016) %>%
  select(song, artist)

num_songs_year %>%
  arrange(desc(song_count)) %>%
  head()

#total unique artists - first have to split artists
songs %>%
  mutate(artist_split = as.character(sapply(strsplit(artist," featuring "), "[", 1))) %>%
  group_by(artist_split) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5)

#how many artists have only appeared once
songs %>%
  mutate(artist_split = as.character(sapply(strsplit(artist," featuring "), "[", 1))) %>%
  group_by(artist_split) %>%
  summarise(count = n()) %>%
  filter(count == 1)


##################
#text mining prep#
##################
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
#what are stop words?
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

#some testing
songs_filtered %>%
  filter(word == "love") %>%
  select(word, song, artist, year) %>%
  arrange() %>%
  top_n(10, song)

################
#Word frequency#
################
#word frequency by decade - full length
full_word_count_song <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, decade, year) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#songs with most total words
#!!! would be interesting to compare to length of song (time)
full_word_count_song[1:10,] %>%
  select(song, num_words, year)

#some summary stats
mean(full_word_count_song$num_words)
sd(full_word_count_song$num_words)

#songs with least total words
full_word_count_song %>%
  filter(!num_words == 1) %>%
  tail(10) %>%
  select(song, num_words, year) %>%
  arrange(num_words)

#visualize word count distribution
#!!!!!!!!make prettier
full_word_count_song %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = decade), binwidth = 25) +
  ylab("Song Count") +
  xlab("Total Number of Words") +
  ggtitle("Total Words Per Song")
  #theme_songs()

#lets repeat above steps, but only looking at unique words
partial_word_count_song <- songs %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  group_by(song, decade, year) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#songs with most unique, distinct words
partial_word_count_song[1:10,] %>%
  select(song, num_words, year)

#songs with least total words
partial_word_count_song %>%
  filter(!num_words == 1) %>%
  tail(10) %>%
  select(song, num_words, year) %>%
  arrange(num_words)

#check out Love Me Do
songs %>%
  filter(song == "Love Me Do") %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  select(word)

#visualize
#!!!!!!!!!!make prettier
partial_word_count_song %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = decade), binwidth = 5) +
  ylab("Song Count") +
  xlab("Total number of words") +
  ggtitle("Distribution of total words per song") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank())

#number of words used over time - both total and unique
#average words per year
full_word_count_year <- songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(year, decade) %>%
  summarise(num_words = n())

#note we join with num_songs_year to calculate the average per year 
full_word_count_year %>%
  inner_join(num_songs_year) %>%
  ggplot(aes(year, num_words/song_count, fill = decade)) +
  geom_col() +
  scale_fill_manual(values = my_colors) +
  theme_songs() +
  labs(x = NULL, y = "Average Total Words") +
  ggtitle("Average Total Words By Year")

#average unique words per year
unique_word_count_year <- songs %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  group_by(year, decade) %>%
  summarise(num_words = n())

unique_word_count_year %>%
  inner_join(num_songs_year) %>%
  ggplot(aes(year, num_words/song_count, fill = decade)) +
  geom_col() +
  scale_fill_manual(values = my_colors) +
  theme_songs() +
  labs(x = NULL, y = "Average Unique Count") +
  ggtitle("Average Unique Words By Year")

############
#Word count#
############
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

#most used words by decade
popular_words %>%
  ggplot() +
  geom_col(aes(row, n, fill = decade), show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  labs(x = NULL, y = NULL) +
  ggtitle("Popular Words by Decade") +
  facet_wrap(~decade, scales = "free") +
  theme_songs() +
  scale_x_continuous(breaks = popular_words$row, labels = popular_words$word) + 
  scale_y_continuous(labels = NULL) +
  coord_flip()

#############
#word length#
#############
#word lengths
#first must remove all instrumental songs
word_lengths <- songs %>%
  filter(!is.na(lyrics)) %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word))

#total distribution of word length
word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length)) +
  geom_histogram(fill = "forestgreen", color = "black", breaks = seq(1, 20, by = 1)) +
  ggtitle("Word Length") +
  labs(x = "Word Length", y = NULL) +
  theme_songs()

#word length by decade
word_lengths %>%
  filter(!decade == "1950s") %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), binwidth = 10) +
  geom_histogram(aes(fill = decade), breaks = seq(1, 20, by = 1), show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  facet_wrap(~decade, scales = "free") +
  xlab("Word Length") +
  ylab("Song Count") +
  ggtitle("Word Length Distribution") +
  theme_songs()

#longest words
word_lengths %>%
  filter(word_length >= 15) %>%
  arrange(desc(word_length))

#!!!!this brings up the issue of some songs' lyrics being wrong - The Hills, Dominique

###################
#lexical diversity#
###################
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

#total words per year
songs %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, year) %>%
  summarise(count = n()) %>%
  ggplot(aes(year, count)) +
  geom_point(color = "orange", alpha = 0.4, size = 3, position = "jitter") +
  stat_smooth(color = "black", method = "lm", se = FALSE) +
  geom_smooth(aes(x = year, y = count), se = FALSE, color = "blue") +
  ggtitle("Total Words") +
  labs(x = "Year", y = NULL) +
  theme_songs()

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