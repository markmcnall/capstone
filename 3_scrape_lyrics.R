library(rvest)
library(dplyr)

setwd("~/Spring 2019 Classes/BANA8083-Case Study")

#####################
#Web scraping lyrics#
#####################

#read in data
modern.era <- read.csv(file = paste(getwd(), "/data/modern_era_pre_scrape.csv", sep = ""), stringsAsFactors = FALSE)

#change encoding of artists and songs to remove any accented characters
modern.era$artist <- iconv(modern.era$artist, to = "ASCII//TRANSLIT")
modern.era$song <- iconv(modern.era$song, to = "ASCII//TRANSLIT")

#create new column for lyrics
modern.era$lyrics <- NA

#list of all instrumental songs to skip
instrumental <- c(6, 12, 20, 38, 66, 75, 191, 197, 209, 286, 308, 359,
                  377, 392, 421, 459, 501, 569)

#list of songs that don't exist on azlyrics
others <- c(7, 16, 19, 26, 50, 56, 63, 78, 83, 86, 95, 115, 125, 126, 149, 171, 177, 233, 265, 271, 290,
            299, 314, 320, 362, 371, 378, 380, 382, 383, 393, 397, 401, 407, 417, 424, 436, 468, 472)

manual <- c(30, 47, 416, 460, 491, 565, 641, 659, 664, 705, 707, 710, 721, 722, 726, 751, 812)

#234:264

#create list of songs to scrape
to.scrape <- c(1:nrow(modern.era))
to.scrape <- to.scrape[-instrumental] 
to.scrape <- to.scrape[-others]
to.scrape <- to.scrape[-manual]

#scraping songs from azlyrics.com
for(i in 256:264) {
  #fetch artist and song
  artist <- modern.era$artist[i]
  song <- modern.era$song[i]
  
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
  
  modern.era$lyrics[i] <- (temp[22])
  
  Sys.sleep(5)
}



#scraping the remainder from oldielyrics.com
for(i in others) {
  artist <- modern.era$artist[i]
  song <- modern.era$song[i]
  
  #notify which song is being processed
  print(paste("Now processing", i, song, "by", artist))
  
  #change ampersand
  artist <- gsub("&", "and", artist)
  
  #remove some punctuation
  song <- gsub("'", "", song)
  artist <- gsub("'", "", artist)
  song <- gsub("[.,()]", "", song)
  artist <- gsub("[.,]", "", artist)
  
  
  #make lower case and change all spaces to underscores to work in html path
  artist <- gsub(" ", "_", artist, fixed = TRUE)
  song <- gsub(" ", "_", song, fixed = TRUE)
  
  artist <- lapply(artist, tolower)
  song <- lapply(song, tolower)
  
  #set correct path
  path <- paste("https://www.oldielyrics.com/lyrics/", artist, "/", song, ".html", sep = "")
  
  #try to scrape
  obj <- read_html(path)
  
  #if worked, scrape!
  temp <- obj %>%
    html_nodes("div") %>%
    html_text()
  
  modern.era$lyrics[i] <- (temp[10])
  
  Sys.sleep(10)
}
############################################################
#####Manual Songs: the rest need to be pulled###############
#            invidiually, weren't on any sites##############
############################################################
#song 30: Mr. Custer - Larry Verne
path <- "https://genius.com/Larry-verne-please-mr-custer-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[30] <- (temp[21])

#song 47: Quarter to Three - Gary U.S. Bonds
path <- "https://genius.com/Gary-us-bonds-quarter-to-three-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[47] <- (temp[21])

#song 416: 
path <- "https://genius.com/Gonna-fly-now-rocky-theme-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[416] <- (temp[21])

#song 460: Pop Muzik - M
path <- "https://genius.com/M-robin-scott-pop-muzik-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[460] <- (temp[25])

#song 491: Stars on 45 Medley - Stars on 45
path <- "https://www.flashlyrics.com/lyrics/stars-on-45/medley-32"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[491] <- (temp[42])

#song 565: OH Sheila - Ready for the World
path <- "https://genius.com/Ready-for-the-world-oh-sheila-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[565] <- (temp[25])

#song 641: Wishing Well - Terence Trent D'Arby
path <- "https://genius.com/Terence-trent-darby-wishing-well-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[641] <- (temp[22])

#song 659: Baby, I Love Your Way / Freebird Medley - Will to Power
modern.era$lyrics[659] <- 
"Shadows grow so long before my eyes
And they're moving across the page
Suddenly the day turns into night
Far away from the city

But don't hesitate
'Cause your love, your love won't wait, oh no no

(Ooh) Baby, I love your way every day
(Ooh) Wanna tell you I love your way every day
(Ooh) Wanna be with you night and day
Hey, I love your way

If I leave here tomorrow
Would you still remember me?
For I must be traveling on now
There's so many places I've gotta see

But if I stayed here with you, girl
Things just wouldn't be the same
'Cause I'm as free as a bird now
And this bird will never change
And this bird will never change

Shadows grow so long before my eyes
With the help of some fireflies
I wonder how they have the power to shine
I can see them under the pines

But don't hesitate
'Cause your love, your love won't wait

Ooh baby, I love your way (every day)
(Ooh) Wanna tell you I love your way (night and day)
Oh, baby, I love your way (every day)
Every day now
Ooh wanna tell you I love your way (night and day, night and day)

I love your way
Ooh baby, I love your way ('cause I'm as free as a bird now)
Give me you way, wanna tell you I love your way (A free bird now)
Ooh baby, I love your ways

Ooh baby, I love your way (every day)
Ooh wanna tell you I love your way (night and day)
Ooh baby, I love your way (every day, every day)
Ooh wanna tell you I love your way (night and day, night and day, oh)
Ooh baby, I love your way (night and day)"

#song 664: When I'm With You - Sheriff
path <- "https://genius.com/Sheriff-when-im-with-you-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[664] <- (temp[25])

#song 705: She Ain't Worth It - Glenn Medeiros ft. Bobby Brown
path <- "https://genius.com/Glenn-medeiros-she-aint-worth-it-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[705] <- (temp[25])

#song 707: If Wishes Came True - Sweet Sensation
path <- "https://genius.com/Sweet-sensation-if-wishes-came-true-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[707] <- (temp[25])

#song 710: (Can't Live Without Your) Love and Affection - Nelson
path <- "https://genius.com/Nelson-cant-live-without-your-love-and-affection-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[710] <- (temp[23])

#song 721: The First Time - Surface
path <- "https://genius.com/Surface-the-first-time-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[721] <- (temp[25])

#song 722: Gonna Make You Sweat (Everybody Dance Now) - C+C Music Factory
path <- "https://genius.com/Justice-crew-gonna-make-you-sweat-everybody-dance-now-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[722] <- (temp[25])

#song 726: I've Been Thinking About You - Londonbeat
path <- "https://genius.com/London-beat-ive-been-thinking-about-you-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[726] <- (temp[25])

#song 751: How Do You Talk to an Angel - The Heights
path <- "https://genius.com/The-heights-musical-how-do-you-talk-to-an-angel-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[751] <- (temp[24])

#song 812: Lately - Divine 
path <- "https://genius.com/Divine-lately-lyrics"

obj <- read_html(path)

temp <- obj %>%
  html_nodes("div") %>%
  html_text()

modern.era$lyrics[812] <- (temp[25])

#write out final data
write.csv(modern.era, file = paste(getwd(), "/data/modern_era_post_scrape.csv", sep = ""), row.names = FALSE)
