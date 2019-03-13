#we need to prep the data for scraping lyrics
library("dplyr")

setwd("~/Spring 2019 Classes/BANA8083-Case Study")

#entire dataset
modern.era <- read.csv(file = paste(getwd(), "/data/modern_era_raw.csv", sep = ""), stringsAsFactors =  FALSE)

#remove all duplicate songs - only keep first occurrence of each
#NOTE: initially was deleting duplicate song titles of different artists (!)
old <- modern.era %>%
  distinct(song, .keep_all = TRUE)

new <- modern.era %>%
  distinct(song, artist, .keep_all = TRUE)

#get ONLY missing songs
missing <- new %>%
  anti_join(old)

#view missing songs
head(missing)

######
#PREP#
######
#fix song titles to remove quotes
#create custom function
remove_quotes <- function(title) {
  return(substr(title, 2, nchar(title) - 1))
}

missing$song <- remove_quotes(missing$song)

#any manual changes
missing$artist[3] <- "Paul McCartney"
missing$artist[4] <- "Carole King"
missing$artist[10] <- "Bill Withers"
missing$artist[19] <- "Christina Aguilera"
missing$artist[27] <- "Ed Sheeran"

#change encoding of artists and songs to remove any accented characters
missing$artist <- iconv(missing$artist, to = "ASCII//TRANSLIT")
missing$song <- iconv(missing$song, to = "ASCII//TRANSLIT")

########
#SCRAPE#
########
library(rvest)

#create new column for lyrics
missing$lyrics <- NA

#scraping songs from azlyrics.com
for(i in 19:nrow(missing)) {
  #fetch artist and song
  artist <- missing$artist[i]
  song <- missing$song[i]
  
  #notify which song is being processed
  print(paste("Now processing:", i, song, "by", artist))
  
  #for artist, need to deal with special exception of featured artists or multiple artists
  artist <- unlist(strsplit(artist, " featuring "))[1]
  artist <- unlist(strsplit(artist, " and "))[1]
  
  #fix any ampersands
  song <- gsub("&", "and", song)
  
  #remove THE from start of artist
  artist <- gsub("The ", "", artist)
  
  #search for any non-alpha characters and remove
  artist <- gsub("[[:punct:][:blank:]]+", "", artist)
  song <- gsub("[[:punct:][:blank:]]+", "", song)
  
  #make lower case and remove all spaces to work in html path
  artist <- gsub(" ", "", artist, fixed = TRUE)
  song <- gsub(" ", "", song, fixed = TRUE)
  
  artist <- lapply(artist, tolower)
  song <- lapply(song, tolower)
  
  #set correct path
  path <- paste0("https://www.azlyrics.com/lyrics/", artist, "/", song, ".html")
  
  #read and scrape lyrics
  obj <- read_html(path)
  
  temp <- obj %>%
    html_nodes("div") %>%
    html_text()
  
  missing$lyrics[i] <- (temp[22])
  
  Sys.sleep(5)
}

###############
#FINAL TOUCHES#
###############
#function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

#get rid of contractions
missing$lyrics <- sapply(missing$lyrics, fix.contractions)

#function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

#remove special characters
missing$lyrics <- sapply(missing$lyrics, removeSpecialChars)

#convert everything to lower case
missing$lyrics <- sapply(missing$lyrics, tolower)

#export these songs
write.csv(missing, file = paste(getwd(), "/data/missing_final.csv", sep = ""), row.names = FALSE)

#import the already cleaned dataset
songs <- read.csv(file = paste(getwd(), "/data/final_song_data.csv", sep = ""), stringsAsFactors = FALSE)

#combine the two datasets
total <- songs %>%
  full_join(missing)

#rearrange by date
total <- total %>%
  arrange(year)

head(total)

#output
write.csv(total, file = paste(getwd(), "/data/final_song_data.csv", sep = ""), row.names = FALSE)