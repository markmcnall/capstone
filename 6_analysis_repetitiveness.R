library(dplyr)
library(ggplot2)

#set up color palette for each decade
my_colors <- c("#e81b23", "#758c33", "#f8ca38", "#ca7cd8", "#287e9e", "#ff6200", "#fe3c71")
gradient_palette <- c("#f9f871", "#ffc75f", "#ff9761", "#ff6f91", "#d65db1", "#845ec2")

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

#compress all lyrics and calculate the compression rate
compression <- songs %>%
  select(song, artist, year, lyrics) %>%
  mutate(len = nchar(lyrics)) %>%
  mutate(comp_lyrics = sapply(lyrics, memCompress, "gzip")) %>%
  mutate(comp = sapply(comp_lyrics, length)) %>%
  mutate(ratio = 1 - (comp / len)) %>%
  select(-comp_lyrics, -lyrics)

#view most and least compressed songs
#most
compression %>%
  select(song, year, artist, ratio) %>%
  arrange(desc(ratio)) %>%
  head(10)

#least
compression %>%
  select(song, artist, ratio) %>%
  arrange(ratio) %>%
  head(10)

#histogram of all compression ratios
#first, need to find midpoint
mid <- mean(compression$ratio[!is.na(compression$ratio)])*100

compression %>%
  filter(!is.na(ratio)) %>%
  ggplot(aes(x = ratio * 100, fill = ..x..)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_x_continuous(breaks = c(50, 60, 70, 80, 90)) +
  scale_y_continuous(labels = NULL) +
  scale_fill_gradientn(colors = gradient_palette) +
  ggtitle("Distribution of Compression Ratio of #1 Hits") +
  labs(x = "Reduction in Size", y = NULL) +
  theme_songs()

#compression ratio by year - have songs gotten more repetitive over time?
year_avg <- compression %>%
  filter(!is.na(ratio)) %>%
  group_by(year) %>%
  summarise(avg = mean(ratio))

year_avg %>%
  ggplot() +
  geom_line(aes(x = year, y = zoo::rollmean(avg, 5, na.pad = TRUE)))
  
#min and max years
#min
year_avg %>%
  arrange(avg) %>%
  head(5)

compression %>%
  filter(year == 1991) %>%
  select(song, artist, ratio) %>%
  arrange(desc(ratio))

#max
year_avg %>%
  arrange(desc(avg)) %>%
  head(5)

compression %>%
  filter(year %in% c(2015, 2016)) %>%
  select(year, song, artist, ratio) %>%
  arrange(year, desc(ratio))
