#we need to prep the data for scraping lyrics
library("dplyr")

setwd("~/Spring 2019 Classes/BANA8083-Case Study")

modern.era <- read.csv(file = paste(getwd(), "/data/modern_era_raw.csv", sep = ""), stringsAsFactors =  FALSE)

#remove all duplicate songs - only keep first occurrence of each
modern.era <- modern.era %>%
  distinct(song, .keep_all = TRUE)

#fix song titles to remove quotes
#create custom function
remove_quotes <- function(title) {
  return(substr(title, 2, nchar(title) - 1))
}

modern.era$song <- remove_quotes(modern.era$song)

#check results
head(modern.era)

#fix all weeks that had more than one song

#first, what are the indices of these songs?
multi.songs <- grep(" /\ ", modern.era$song)

#what are they?
modern.era[multi.songs,]$song

#so need to split up into two lines, one for each song
for(i in multi.songs) {
  first.song <- (unlist(strsplit(modern.era$song[i], split = '" /\ "', fixed = TRUE)))[1]
  second.song <- (unlist(strsplit(modern.era$song[i], split = '" /\ "', fixed = TRUE)))[2]
  
  artist <- modern.era$artist[i]
  id <- modern.era$issue.date[i]
  yr <- modern.era$year[i]
  
  modern.era$song[i] <- first.song
  modern.era <- rbind(modern.era, c(id, second.song, artist, yr))
  
}

################
#manual changes#
################

#these are all the changes that needed to made for songs to correctly match azlyrics.com
#many songs were scraped from covers, original artist noted in comment to change back later
modern.era$artist[1] <- "Alvin and the Chipmunks"
modern.era$song[1] <- "The Chipmunk Song (Christmas Don't Be Late) (Classic Version)"
modern.era$artist[3] <- "The Youngbloods" #same song, but Lloyd Price version lyrics not available
modern.era$artist[4] <- "Barry Manilow" #same song, but can't find Frankie Avalon lyrics
modern.era$artist[c(5,14)] <- "Bobby Vinton" #the fleetwoods
modern.era$artist[11] <- "Roy Orbison" #The Browns
modern.era$artist[15] <- "Ray Price" #Guy Mitchell
modern.era$artist[18] <- "The Youngbloods" #Johnny Preston
modern.era$artist[24] <- "The Beach Boys" #Hollywood Argyles
modern.era$artist[34] <- "The Four Seasons" #Maurice Williams and the Zodiacs
modern.era$artist[36] <- "Engelbert Humperdinck" #Bert Kaempfert
modern.era$artist[37] <- "Carole King" #The Shirelles
modern.era$artist[41] <- "Frank Sinatra" #The Marcels
modern.era$artist[43] <- "Herman's Hermits" #Ernie K-Doe
modern.era$artist[46] <- "Frank Sinatra" #Pat Boone
modern.era$artist[48] <- "The Marvelettes" #Bobby Lewis
modern.era$artist[49] <- "Bobby Vinton" #Joe Dowell
modern.era$artist[53] <- "The Overtones" #Dion
modern.era$artist[57] <- "Sweet" #Joey Dee and the Starliters
modern.era$artist[58] <- "New Edition" #Gene Chandler
modern.era$artist[59] <- "Anne Murray" #Bruce Channel
modern.era$artist[61] <- "The Carpenters" #Shelley Fabares
modern.era$artist[64] <- "Andy Williams" #Mr. Acker Bilk
modern.era$artist[c(69, 76)] <- "Carole King"
modern.era$artist[72] <- "Bobby Pickett"
modern.era$artist[77] <- "Janis Joplin" #The Rooftop Singers
modern.era$artist[80] <- "Brenda Lee" #Ruby & The Romantics
modern.era$artist[81] <- "Dionne Bromfield" #The Chiffons
modern.era$artist[82] <- "Petula Clark" #Little Peggy March
modern.era$song[85] <- "Sukiyaki (Ue Wo Muite Arukou)"
modern.era$artist[87] <- "Jan Dean" #Jan and Dean
modern.era$artist[88] <- "Jay The Americans" #The Tymes
modern.era$artist[89] <- "Stevie Wonder"
modern.era$song[89] <- "Fingertips Part 2"
modern.era$artist[92] <- "Del Shannon" #Jimmy Gilmer and the Fireballs
modern.era$artist[c(93, 94)] <- "Donny Osmond" #Nino Tempo and April Stevens
modern.era$song[94] <- "I'm Leaving It (All) Up To You"
modern.era$artist[103] <- "Bette Midler" #The Dixie Cups
modern.era$artist[104] <- "The Supremes" #Peter and Gordon
modern.era$song[110] <- "House of the Rising Sun"
modern.era$artist[112] <- "Jan Dean" #Manfred Mann
modern.era$artist[114] <- "Bette Midler" #The Shangri-Las
modern.era$artist[121] <- "Del Shannon" #Gary Lewis & the Playboys
modern.era$artist[138] <- "The Supremes" #The McCoys
modern.era$song[142] <- "Turn! Turn! Turn!"
modern.era$artist[147] <- "Jan Dean" #Lou Christie
modern.era$artist[151] <- "Grateful Dead" #The Young Rascals
modern.era$artist[164] <- "Aretha Franklin" #? and the Mysterians
modern.era$artist[168] <- "Frank Sinatra" #The New Vaudeville Band
modern.era$artist[176] <- "Frank Sinatra"
modern.era$artist[178] <- "The Rascals"
modern.era$artist[183] <- "Paula Cole" #Bobbie Gentry
modern.era$song[183] <- "Ode to Billy Joe"
modern.era$song[207] <- "Aquarius / Let the Sunshine In"
modern.era$artist[208] <- "The Beatles"
modern.era$artist[210] <- "Zager & Evans"
modern.era$artist[218] <- "Peter, Paul, Mary"
modern.era$artist[219] <- "Diana Ross and the Supremes"
modern.era$artist[c(240,352)] <- "Tony Orlando & Dawn"
modern.era$artist[241] <- "Aaron Carter" #The Osmonds
modern.era$song[242] <- "Me Bobby McGee" #Me and Bobby McGee
modern.era$artist[246] <- "Taylor Dayne" #Honey Cone
modern.era$artist[248] <- "Paul Revere & The Raiders"
modern.era$artist[251] <- "Paul McCartney"
modern.era$song[253] <- "Gypsies, Tramps And Thieves"
modern.era$artist[256] <- "Deana Carter" #Melanie
modern.era$artist[259] <- "Harry Nilsson"
modern.era$song[270] <- "Black White"
modern.era$artist[283] <- "Reba McEntire" #Vicki Lawrence
modern.era$artist[284] <- "Tony Orlando Dawn"
modern.era$artist[292] <- "Hot Chocolate" #Stories
modern.era$artist[295] <- "Grand Funk Railroad"
modern.era$artist[298] <- "Gladys Knight"
modern.era$artist[302] <- "Andy Williams" #Charlie Rich
modern.era$artist[305] <- "Johnny Mathis" #Al Wilson
modern.era$song[306] <- "You're Sixteen (You're Beautiful and You're Mine)"
modern.era$artist[c(316,358, 384, 429)] <- "Paul McCartney and the Wings"
modern.era$artist[317] <- "Paper Lace"
modern.era$artist[319] <- "Inner Circle" #The Hues Corporation
modern.era$song[331] <- "You Haven't Done Nothing"
modern.era$artist[332] <- "B.T.O - Bachman-Turner Overdrive"
modern.era$song[333] <- "Whatever Gets You Through The Night"
modern.era$artist[334] <- "Elvis Presley" #Billy Swan
modern.era$song[344] <- "The Best of My Love"
modern.era$artist[349] <- "Shanice" #Minnie Ripperton
modern.era$artist[350] <- "Elton John"
modern.era$artist[354] <- "Dolly Parton" #Freddy Fender
modern.era$artist[357] <- "Neil Sedaka" #Captain & Tennille
modern.era$artist[372] <- "Paul Brandt" #C.W. McCall
modern.era$artist[395] <- "Status Quo" #Steve Miller Band
modern.era$artist[c(405,488,496,498,512,546)] <- "Hall & Oates"
modern.era$song[408] <- "Don't Leave Me This Way (Parts 1 & 2)"
modern.era$artist[408] <- "Teddy Pendergrass" #Thelma Houston
modern.era$song[415] <- "Got to Give It Up"
modern.era$artist[418] <- "The Carpenters" #Shaun Cassidy
modern.era$artist[428] <- "Bee Gees" #Yvonne Elliman
modern.era$artist[431] <- "Olivia-Newton John" #also with John Travolta
modern.era$artist[448] <- "Otis Redding" #Amii Stewart
modern.era$song[465] <- "Escape (The Pia Colada Song)" #to avoid the ñ
modern.era$song[471] <- "Call Me"
modern.era$song[506] <- "Hard to Say I'm Sorry / Get Away"
modern.era$artist[507] <- "John Mellencamp"
modern.era$artist[515] <- "James Ingram"
modern.era$song[527] <- "All Night Long"
modern.era$song[542] <- "Let's Go Grazy" #mispelled online
modern.era$artist[549] <- "George Michael"
modern.era$artist[552] <- "Michael Jackson" #technically USA for Africa
modern.era$song[552] <- "We're the World - USA for Africa"
modern.era$artist[563] <- "David Essex" #John Parr
modern.era$artist[c(570,577, 608)] <- "Jefferson Starship"
modern.era$artist[574] <- "Dionne Warwick"
modern.era$artist[601] <- "Bruce Hornsby and the Range"
modern.era$artist[604] <- "Tom Jones" #Billy Vera and the Beaters
modern.era$artist[c(611,617)] <- "U2band" #not sure why U2 is listed as this...
modern.era$artist[c(613,623)] <- "Lisa Lisa & Cult Jam"
modern.era$song[614] <- "I Wanna Dance with Somebody"
modern.era$artist[620] <- "Michael Jackson and Siedah Garrett"
modern.era$artist[627] <- "Jennifer Warnes and Bill Medley"
modern.era$artist[635] <- "Expose" #removes accent
modern.era$artist[667] <- "Mike & the Mechanics"
modern.era$artist[674] <- "David Essex" #Michael Damian
modern.era$artist[695] <- "Paula Abdul and The Wild Pair"
modern.era$artist[700] <- "Sinead O'Connor" #removes accent
modern.era$artist[730] <- "PYT" #Hi-Five
modern.era$song[758] <- "(I Can't Help) Falling In Love With You"
modern.era$artist[763] <- "Bryan Adams and Rod Stewart and Sting"
modern.era$artist[767] <- "New Found Glory" #Lisa Loeb & Nine Stories
modern.era$artist[c(782,802)] <- "Celine Dion" #removes accent
modern.era$artist[839] <- "Sisqo"
modern.era$song[785] <- "How Do You Want It"
modern.era$song[797] <- "Candle in the Wind"
modern.era$artist[c(800,840,849)] <- "Janet Jackson"
modern.era$song[830] <- "Thanks God I Found You"
modern.era$artist[837] <- "Matchbox 20"
modern.era$song[842] <- "Come on Over (All I Want Is You) (Radio Edit)"
modern.era$song[844] <- "Independent Women"
modern.era$artist[851] <- "Christina Aguilera featuring Lil' Kim, Mya and Pink"
modern.era$song[854] <- "I'm Real (Remix)"
modern.era$artist[c(870,910,939)] <- "Beyonce Knowles"
modern.era$artist[871] <- "Nelly featuring P. Diddy and Murphy Lee"
modern.era$artist[872] <- "Beyonce Knowles featuring Sean Paul"
modern.era$artist[879] <- "Fantasia Barrino"
modern.era$artist[896] <- "Beyonce Knowles featuring Slim Thug"
modern.era$song[901] <- "S.O.S. (Rescue Me)"
modern.era$song[912] <- "What Goes Around...Comes Around (Interlude)"
modern.era$artist[916] <- "Timbaland featuring Nelly Furtado and Justin Timberlake"
modern.era$artist[923] <- "Soulja Boy"
modern.era$song[923] <- "Crank Dat (Soulja Boy)"
modern.era$artist[942] <- "Eminem featuring Dr. Dre and 50 Cent"
modern.era$song[946] <- "I Gotta Feelin'"
modern.era$song[974] <- "SM"
modern.era$song[984] <- "What Doesn't Kill You (Stronger)"
modern.era$song[993] <- "Thrift Shop"
modern.era$artist[993] <- "Macklemore and Ryan Lewis featuring Wanz"
modern.era$artist[997] <- "Macklemore and Ryan Lewis featuring Ray Dalton"
modern.era$song[1005] <- "Happy"
modern.era$artist[1005] <- "Pharrell Williams"
modern.era$song[1015] <- "I Can't Feel My Face"
modern.era$artist[1020] <- "Zayn Malik"
modern.era$artist[1038] <- "Ed Sheeran"
modern.era$artist[c(952,964)] <- "Keha" #Kesha
modern.era$song[1052] <- "No Sugar Tonight / New Mother Nature"
modern.era$song[1054] <- "Isn't It a Pity (Version 1)"

#changes to match oldieslyrics.com
modern.era$song[26] <- "Itsy Bitsy Teenie Weenie Yellow Polka-Dot Bikini"
modern.era$song[50] <- "Michael Row the Boat Ashore"
modern.era$song[149] <- "The Ballad of the Green Berets"
modern.era$song[299] <- "Keep On Truckin'"
modern.era$artist[314] <- "MFSB"
modern.era$song[314] <- "T S O P (The Sound of Philadelphia)"
modern.era$song[378] <- "Love Machine"
modern.era$song[393] <- "Disco Duck"
modern.era$artist[393] <- "Rick Dees"
modern.era$artist[397] <- "Marilyn McCoo, Billy Davis Jr."

#write out to be ready for scraping
write.csv(modern.era, file = paste(getwd(), "/data/modern_era_pre_scrape.csv", sep = ""), row.names = FALSE)
