library(rvest)
library(dplyr)

setwd("~/Spring 2019 Classes/BANA8083-Case Study")

####################
#Web scraping songs#
####################

#create dataframe for modern era - 1959 onwards
modern.era <- data.frame(issue.date = character(), 
                         song = character(),
                         artist = character(), 
                         year = integer(), 
                         stringsAsFactors = FALSE)

#define vectors with all proper col names
cols <- c("issue.date", "song", "artist")

#define vector with years that table is third
years_three <- c(1970, 1971, 1972, 1994, 1997)

#loop through every other year
for(year in 1959:2018) {
  print(paste("Now processing ", year))
  
  #determine which position table is on html file
  if(year %in% years_three) {
    table.no <- 3
  } else if(year == 1989) {
    table.no <- 1
  } else table.no <- 2
  
  #dynamically create path for each year's link
  path <- paste("https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number-one_singles_of_", year, sep = "")
  
  #read html file
  current_year <- read_html(path)
  
  #scrape table of songs
  temp <- current_year %>%
    html_nodes("table") %>%
    .[[table.no]] %>%
    html_table()
  
  #get rid of reference column
  temp <- temp[,1:3]
  
  #rename columns to fit modern.era
  colnames(temp) <- cols
  
  #add year column
  temp$year <- year
  
  #bind to master list of all songs
  modern.era <- rbind(modern.era, temp)
  
  #wait 5 seconds for the next year
  Sys.sleep(5)
}

#save all raw scraped data before editing it at all
write.csv(modern.era, file = paste(getwd(), "/data/modern_era_raw.csv", sep = ""), row.names = FALSE)
