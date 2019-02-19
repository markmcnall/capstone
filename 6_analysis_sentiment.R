############################
#PART 2: Sentiment Analysis#
############################
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(widyr)

#set up color palette for each decade
my_colors <- c("#758c33", "#f8ca38", "#ca7cd8", "#287e9e", "#ff6200", "#fe3c71")
other_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#define my custom ggplot theme
theme_songs <- function(aticks = element_blank(),
                        pgminor = element_blank(),
                        lt = element_blank(),
                        lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center title
        axis.ticks = aticks, #set axis ticks off or on
        panel.grid.minor = pgminor, #set minor grid lines on or off
        legend.title = lt, #turn legend title off or on
        legend.position = lp) #turn legend on or off
}

#read in data
setwd("~/Spring 2019 Classes/BANA8083-Case Study")
songs <- read.csv(paste(getwd(), "/data/final_song_data.csv", sep = ""), stringsAsFactors = FALSE)

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

glimpse(songs)

#undesirables
undesirable_words <- c("chorus", "repeat", "lyrics",
                       "theres", "bridge", "fe0f", "yeah", "baby",
                       "alright", "wanna", "gonna", "chorus", "verse",
                       "whoa", "gotta", "make", "miscellaneous", "2",
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121",
                       "matic", " ai ", " ca ", " la ", "hey", " na ",
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats", "la", "da", "uh", "ah")

#create tidy version
songs_tidy <- songs %>%
  unnest_tokens(word, lyrics) %>%
  filter(!word %in% undesirable_words) %>%
  filter(!nchar(word) < 3) %>%
  anti_join(stop_words)

glimpse(songs_tidy)

#pirate plot - lexical diversity
word_summary <- songs_tidy %>%
  group_by(decade, song) %>%
  filter(!decade %in% "1950s") %>%
  mutate(word_count = n_distinct(word)) %>%
  select(song, Released = decade, word_count) %>%
  distinct %>%
  ungroup()

pirateplot(formula = word_count ~ Released,
           data = word_summary,
           xlab = NA,
           ylab = "Song Distinct Word Count",
           main = "Lexical Diversity per song",
           pal = my_colors,
           point.o = .2,
           avg.line.o = 1,
           theme = 0,
           point.pch = 16,
           point.cex = 1.5,
           jitter.val = .1,
           cex.lab = .9, cex.names = .7)

#songs per year
songs_year <- songs %>%
  select(song, year) %>%
  group_by(year) %>%
  summarise(song_count = n())

#radial chart
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

#lexicons and lyrics
#tidytext has a few preset lexicons - 
#1. AFINN - assigns words score -5 to 5 
#2. Bing - assigns words into negative and positive categories
#3. NRC - assigns words into one or more of 10 categories

new_sentiments <- sentiments %>%
  filter(lexicon != "loughran") %>% #removes financial lexicon
  mutate(sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive", 
                            ifelse(lexicon == "AFINN" & score < 0, "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

#which lexicon is most applicable to our lyrics?
songs_tidy %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words),
         match_ratio = lex_match_words/words_in_lyrics) %>%
  select(lexicon, lex_match_words, words_in_lyrics, match_ratio)

#read more on this topic...

#create new datasets with each of the lexicons
songs_bing <- songs_tidy %>%
  inner_join(get_sentiments("bing"))

songs_nrc <- songs_tidy %>%
  inner_join(get_sentiments("nrc"))

songs_nrc_sub <- songs_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

#create basic plots
songs_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) +
  ggtitle("All Songs NRC Sentiment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL, y = "Word Count") +
  coord_flip()

#bing lexicon
songs_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) +
  ggtitle("All Songs Bing Sentiment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL, y = "Word Count") +
  coord_flip()

#by decade
songs_polarity_decade <- songs_bing %>%
  filter(!decade %in% "1950s") %>%
  count(sentiment, decade) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative, 
         percent_positive = positive / (positive + negative) * 100)

plot1 <- songs_polarity_decade %>%
  ggplot(aes(decade, polarity, fill = decade)) +
  geom_col() +
  scale_fill_manual(values = my_colors) +
  theme_songs() + theme(plot.title = element_text(size = 11)) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = NULL, y = NULL) +
  ggtitle("Polarity by decade")

#could also do this plot with the 50s
plot2 <- songs_polarity_decade %>%
  ggplot(aes(decade, percent_positive, fill = decade)) + 
  geom_col() +
  scale_fill_manual(values = my_colors) +
  geom_hline(yintercept = 0, color = "black") +
  theme_songs() +
  labs(x = NULL, y = NULL) +
  ggtitle("Percent Positive By Year")

#grid.arrange(plot1, plot2, ncol = 2)

#view polarity over time changes
songs_polarity_year <- songs_bing %>%
  count(sentiment, year) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative, 
         percent_positive = positive / (positive + negative) * 100)

polarity_over_time <- songs_polarity_year %>%
  ggplot(aes(year, polarity, color = ifelse(polarity >= 0, other_colors[5], other_colors[4]))) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = other_colors[1])) +
  theme_songs() + theme(plot.title = element_text(size = 11)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Polarity Over Time")

relative_polarity_over_time <- songs_polarity_year %>%
  ggplot(aes(year, percent_positive, color = ifelse(polarity >= 0, other_colors[5], other_colors[4]))) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = other_colors[1])) +
  theme_songs() + theme(plot.title = element_text(size = 11)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Relative Polarity Over Time")

#circle thingy
library(circlize)

decade_mood <- songs_nrc %>%
  filter(decade != "1950s" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, decade) %>%
  group_by(decade, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

grid.col = c("1960s" = my_colors[1], "1970s" = my_colors[2], "1980s" = my_colors[3],
             "1990s" = my_colors[4], "2000s" = my_colors[5], "2010s" = my_colors[6],
             "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey",
             "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

circos.clear()
#set gap size
circos.par(gap.after = c(rep(5, length(unique(decade_mood[[1]])) - 1), 15,
                         rep(5, length(unique(decade_mood[[2]])) - 1), 15))
chordDiagram(decade_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Decade")

#radar charts
library(radarchart)

decade_sentiment_nrc <- songs_nrc_sub %>%
  group_by(decade, sentiment) %>%
  count(decade, sentiment) %>%
  select(decade, sentiment, sentiment_count = n)

#not unique
total_sentiment_decade <- songs_nrc_sub %>%
  count(decade) %>%
  select(decade, decade_total = n)

decade_sentiment_chart <- decade_sentiment_nrc %>%
  inner_join(total_sentiment_decade, by = "decade") %>%
  mutate(percent = sentiment_count / decade_total * 100) %>%
  filter(!decade %in% "1950s") %>%
  select(-sentiment_count, -decade_total) %>%
  spread(decade, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE, main = "NRC Decades Radar")

#bigrams - don't just look at one word!
songs_bigrams <- songs %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)

#must split up to remove any undesirable words and stop words
bigrams_separated <- songs_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)

bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(songs) %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

bigram_decade %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = bigram_decade$row, # Notice need to reuse data frame
    labels = bigram_decade$bigram) +
  theme_songs() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per Decade") +
  coord_flip()

#bigram sentiment analysis
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_songs() +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * Number of Occurrences") +
  ggtitle("Polar Sentiment of Words Preceded by Not") +
  coord_flip()

#additional topics include trigrams, 