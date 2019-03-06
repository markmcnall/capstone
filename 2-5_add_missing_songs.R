#we need to prep the data for scraping lyrics
library("dplyr")

setwd("~/Spring 2019 Classes/BANA8083-Case Study")

modern.era <- read.csv(file = paste(getwd(), "/data/modern_era_raw.csv", sep = ""), stringsAsFactors =  FALSE)

#remove all duplicate songs - only keep first occurrence of each
#NOTE: initially was deleting duplicate song titles of different artists (!)
modern.era <- modern.era %>%
  distinct(song, .keep_all = TRUE)