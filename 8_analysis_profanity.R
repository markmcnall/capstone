library(dplyr)
library(ggplot2)

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

###########
#Profanity#
###########
profane_list <- read.csv(paste(getwd(), "/data/profanity.csv", sep = ""), stringsAsFactors = FALSE)[[1]]

#create list of only profane words
profanity <- songs %>%
  unnest_tokens(word, lyrics) %>%
  select(word, song, year, decade) %>%
  filter(word %in% profane_list)

#total profane by decade
profanity %>%
  group_by(decade) %>%
  filter(decade != "1950s") %>%
  summarise(num_words = n()) %>%
  ggplot(aes(decade, num_words, fill = decade)) +
  geom_col() +
  scale_fill_manual(values = my_colors[2:7]) +
  theme_songs() +
  ggtitle("Profane Words By Decade") +
  labs(x = NULL, y = NULL) +
  coord_flip()

#total profane by year
profanity %>%
  group_by(year, decade) %>%
  summarise(num_words = n()) %>%
  ggplot(aes(year, num_words, fill = decade)) +
  geom_col() +
  scale_fill_manual(values = my_colors) +
  theme_songs() +
  ggtitle("Profane Words Over Time") +
  labs(x = NULL, y = NULL)

#profane per song
profanity %>%
  group_by(year, decade) %>%
  summarise(num_words = n()) %>%
  inner_join(songs_year) %>%
  ggplot(aes(year, num_words/song_count, fill = decade)) +
  geom_col()

#most profane words in them?
profanity %>%
  group_by(song) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

profanity %>%
  group_by(year, song) %>%
  summarise(test = n()) %>%
  count() %>%
  inner_join(songs_year) %>%
  ggplot(aes(year, n/song_count, fill = decade)) + 
  geom_col() +
  scale_fill_manual(values = my_colors) +
  theme_songs() + 
  labs(x = NULL, y = NULL) +
  ggtitle("Something")

#most used profane words
profanity1 <- profanity %>%
  count(word, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange((n)) %>%
  mutate(row = row_number())

profanity1 %>%
  ggplot() +
  geom_col(aes(row, n)) +
  scale_x_continuous(breaks = profanity1$row, labels = profanity1$word) +
  coord_flip()