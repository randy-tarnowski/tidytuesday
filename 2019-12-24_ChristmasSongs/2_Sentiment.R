#2_Sentiment Analysis
#RUN 1 for cleaned doc
####SENTIMENT ANALYSIS
library(syuzhet)

agg<-read.csv("christmaslyrics.csv")

# Getting the sentiment value for the lyrics
agg$lyrics<-as.character(agg$lyrics)
agg<-agg%>%
  mutate(found_L1=ifelse(lyrics=="", "No", "Yes"), 
         lyrics=ifelse(performer=="Kenny G", "", lyrics), 
         lyrics=ifelse(lyrics=="Instrumental", "", lyrics),
         instrumental=ifelse(songid=="Auld Lang SyneKenny G", "Yes"))

agg_notfound<-agg%>%
  filter(lyrics=="")%>%
#Removing instrumentals
  filter(performer!="Kenny G",
         song!="My Favorite Things",
         performer!="Santo & Johnny")


#add urls 
agg_notfound$URL<-c("https://genius.com/The-killers-a-great-big-sled-lyrics",
                    "https://genius.com/Burl-ives-have-a-holly-jolly-christmas-lyrics",
                    "https://genius.com/Elvis-presley-blue-christmas-lyrics",
                    "https://genius.com/Bobby-helms-jingle-bell-rock-lyrics",
                    "https://genius.com/Bobby-boris-pickett-monsters-holiday-lyrics",
                    "https://genius.com/Alvin-and-the-chipmunks-the-chipmunk-song-christmas-dont-be-late-lyrics",
                    "https://genius.com/The-chad-mitchell-trio-marvelous-toy-lyrics",
                    "https://genius.com/98-this-gift-lyrics",
                    "https://genius.com/The-drifters-white-christmas-lyrics")

###Pulls songs that have
for (i in 1:length(agg_notfound$songid)){
  print(agg_notfound[i,2])
  q<-genius_url(paste(agg_notfound[i,"URL"]))
  agg_notfound[i,"lyrics"]<-paste(unlist(q$lyric), collapse =" ") 
}

##merge back in lyrics
agg<-agg%>%
  mutate(lyrics=ifelse(songid %in% agg_notfound$songid, agg_notfound$lyrics, lyrics))


##SENTIMENT ANALYSIS!
agg$sentiment <- get_sentiment(agg$lyrics, method="afinn")

##MERGE INTO MAIN
christmas_songs<-merge(christmas_songs, agg, by="songid")

#Chose afinn because it uses affective measures of microblogs (twitter)
#Leaving off songs that are instrumentals


#save
write.csv(agg, "christmaslyrics_agg.csv")
write.csv(christmas_songs, "christmaswithsentiment.csv")