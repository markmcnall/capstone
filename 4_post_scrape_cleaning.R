setwd("~/Spring 2019 Classes/BANA8083-Case Study")

modern.era <- read.csv(paste(getwd(), "/data/modern_era_post_scrape.csv", sep = ""), stringsAsFactors = FALSE)

#fix back any artists that were used to scrape lyrics from cover versions
modern.era$artist[3] <- "Lloyd Price"
modern.era$artist[4] <- "Frankie Avalon"
modern.era$artist[c(5,14)] <- "The Fleetwoods"
modern.era$artist[11] <- "The Browns"
modern.era$artist[15] <- "Guy Mitchell"
modern.era$artist[18] <- "Johnny Preston"
modern.era$artist[24] <- "Hollywood Argyles"
modern.era$artist[34] <- "Maurice Williams and the Zodiacs"
modern.era$artist[36] <- "Bert Kaempfert"
modern.era$artist[37] <- "The Shirelles"
modern.era$artist[41] <- "The Marcels"
modern.era$artist[43] <- "Ernie K-Doe"
modern.era$artist[46] <- "Pat Boone"
modern.era$artist[48] <- "Bobby Lewis"
modern.era$artist[49] <- "Joe Dowell"
modern.era$artist[53] <- "Dion"
modern.era$artist[57] <- "Joey Dee and the Starliters"
modern.era$artist[58] <- "Gene Chandler"
modern.era$artist[59] <- "Bruce Channel"
modern.era$artist[61] <- "Shelley Fabares"
modern.era$artist[64] <- "Mr. Acker Bilk"
modern.era$artist[77] <- "The Rooftop Singers"
modern.era$artist[80] <- "Ruby & the Romantics"
modern.era$artist[81] <- "The Chiffons"
modern.era$artist[82] <- "Little Peggy March"
modern.era$artist[87] <- "Jan and Dean"
modern.era$artist[88] <- "The Tymes"
modern.era$artist[92] <- "Jimmy Gilmer and the Fireballs"
modern.era$artist[c(93, 94)] <- "Nino Tempo and April Stevens"
modern.era$artist[103] <- "The Dixie Cups"
modern.era$artist[104] <- "Peter and Gordon"
modern.era$artist[112] <- "Mannfred Mann"
modern.era$artist[114] <- "The Shangri-Las"
modern.era$artist[121] <- "Gary Lewis & the Playboys"
modern.era$artist[138] <- "The McCoys"
modern.era$artist[147] <- "Lou Christie"
modern.era$artist[151] <- "The Young Rascals"
modern.era$artist[164] <- "? & the Mysterians"
modern.era$artist[168] <- "The New Vaudeville Band"
modern.era$artist[183] <- "Bobbie Gentry"
modern.era$artist[241] <- "The Osmonds"
modern.era$song[242] <- "Me and Bobby McGee"
modern.era$artist[246] <- "Honey Cone"
modern.era$artist[256] <- "Melanie"
modern.era$artist[283] <- "Vicki Lawrence"
modern.era$artist[292] <- "Stories"
modern.era$artist[302] <- "Charlie Rich"
modern.era$artist[305] <- "Al Wilson"
modern.era$artist[319] <- "The Hues Coporation"
modern.era$artist[334] <- "Billy Swan"
modern.era$artist[349] <- "Minnie Ripperton"
modern.era$artist[354] <- "Freddy Fender"
modern.era$artist[357] <- "Captain & Tennille"
modern.era$artist[372] <- "C.W. McCall"
modern.era$artist[395] <- "Steve Miller Band"
modern.era$artist[408] <- "Thelma Houston"
modern.era$artist[418] <- "Shaun Cassidy"
modern.era$artist[428] <- "Yvonne Elliman"
modern.era$artist[431] <- "Olivia-Newton John and John Travolta"
modern.era$artist[448] <- "Amii Stewart"
modern.era$song[465] <- "Escape (The Pina Colada Song)"
modern.era$song[542] <- "Let's Go Crazy"
modern.era$artist[563] <- "John Parr"
modern.era$artist[604] <- "Billy Vera and the Beaters" 
modern.era$artist[c(611,617)] <- "U2"
modern.era$artist[674] <- "Michael Damien"
modern.era$artist[730] <- "Hi-Five"
modern.era$artist[767] <- "Lisa Loeb & Nine Stories"
modern.era$artist[c(952,964)] <- "Kesha"

#must fix 85 to ONLY english
modern.era$lyrics[85] <- unlist(strsplit(modern.era$lyrics[85], split = "[English translation:]", fixed = TRUE))[2]
modern.era$lyrics[85]

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
modern.era$lyrics <- sapply(modern.era$lyrics, fix.contractions)

#function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

#remove special characters
modern.era$lyrics <- sapply(modern.era$lyrics, removeSpecialChars)

#convert everything to lower case
modern.era$lyrics <- sapply(modern.era$lyrics, tolower)

#check out the results
head(modern.era$lyrics)
tail(modern.era$lyrics)

#save out the cleaned dataset, ready to use!
write.csv(modern.era, file = paste(getwd(), "/data/final_song_data.csv", sep = ""), row.names = FALSE)
