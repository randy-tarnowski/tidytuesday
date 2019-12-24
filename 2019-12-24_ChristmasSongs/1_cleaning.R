##Tidy Tuesday
# 
# Christmas Music Billboards
# This week's data is about Christmas songs on the hot-100 list! Clean data comes from Kaggle and originally from data.world.
# 
# The lyrics come courtest of Josiah Parry's genius R package.
#It has several useful functions, mainly built around grabbing lyrics for specific artists, songs, or albums.
library(SentimentAnalysis)
library(ggplot2)
library(tidyverse)
library(genius)
library(purrr)
genius_token() #ZPt5IdZpPpnk_ga5ac5JH8iaPyVtEkrUI3kMhzNphG-yKavGOX5goK3ZnP0uVXZf

#PULL DATA
christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")
christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")

#Adjust Title of Christmas Songs
christmas_songs$song<-str_to_title(christmas_songs$song)

#Condense down to run actual lyrics search
agg <- christmas_songs %>%
  group_by(songid, song, performer)%>%
  summarise(n=n())

###Pulls songs that have
for (i in 1:length(agg$songid)){
  print(agg[i,2])
  q<-possible_lyrics(artist = paste(agg[i,3]), song = paste(agg[i,2]))
  agg[i,5]<-paste(unlist(q$lyric), collapse =" ") 
  }


##Add non-genius lyrics
agg<-agg%>%
  rename(lyrics=V5)%>%
  mutate(lyrics=ifelse(song=="Merry Christmas In The Nfl", 
                       "They break out of the huddle The center snaps the ball The quarterback's fading He throws long, he's got it Touchdown Twas the night before Christmas And all through my brain There's footballs and thoughts Of the upcoming game My team colors were Hung by the television set In hopes they'd make it To the Super Bowl yet When out on the street There arose such a clatter I sprang from my bed To see what was the matter And what, with my Wondering eyes should be seen But Howard Cosell in a black limosine (CHORUS) (It's Merry Christmas) in the NFL (Merry Christmas) and we wish you well (Here's a helmet, a whistle too) (And cheerleader pictures Autographed to you) He warmed up, then sprinted And dodged as he came Whistling and shouting And calling by name Now Houston, now Dallas Now Pittsburgh, now Green Bay On Denver, on Cleveland On Oakland and L.A. His eyes, how they twinkled His nose like a cherry He brought Christmas cards From Franco and Terry And best wishes from Fran And Dandy Don, too Along with a pair of Their old football shoes (CHORUS) He opened his bag Full of presents for me Jerseys and kneepads And kicking tees And record books Containing all of the facts And highlight tapes For my Betamax He selected the tape Of Super Bowl Seven And we recapped the game 'Til well past eleven When the last play was run He picked his bag off the floor Ran a play up the middle And right out the door He lit a cigar, smiled and then Motioned his arm First down and ten And I heard him exclaim As he drove out of sight Merry Christmas to all See you next Monday night (CHORUS) (It's Merry Christmas) in the NFL (Merry Christmas) Hey, that's Pete Rozelle (Here's a helmet, a whistle too) Oh, jingle bells, jingle bells (And cheerleader pictures) Jingle all the way (Autographed to you) (It's Merry Christmas) Hey, Cosell How about next Christmas (Merry Christmas) Bringing Don Meridith He's my favorite TV actor (Here's a helmet, a whistle too) Aw, I love football (And cheerleader pictures)",
                       as.character(lyrics)))%>%
  mutate(lyrics=ifelse(song=="The Happy Reindeer",
                       "My Name Is Prancer Duh, uh, I'm Dancer My name is uh, my name is uh Say are you Nervous Nope We are Santa's Reindeer We've learned to sing this year So we can tell everyone Christmas day is near Oh, we're on our way with A great big sleigh full of toys Choo choo trains and candy canes For all you girls and boys Jingle bells, oh, jingle bells Ring your happy tune Riding high up in the sky Hi there, Mr. Moon Gee, that was a lot of fun Let's do it again We are Santa's Reindeer We've learned to sing this year So we can tell everyone Christmas day is near Oh, we're on our way with A great big sleigh full of toys Choo choo trains and candy canes For all you girls and boys Jingle bells, oh, jingle bells Ring your happy tune Riding high up in the sky Hi there, Mr. Moon And remember, everybody We're on our way to see you And you and you and you...",
                       as.character(lyrics)))%>%
  mutate(lyrics=ifelse(performer=="The Star Wars Intergalactic Droid Choir & Chorale",
                       "What can you get a wookie for christmas? when he already owns a comb? What can you get in a hurry for a furry kind of friend like that to take home? Oh he doesn't need a tie clip and he doesn't use shaving foam so what can you get a wookie for christmas when he already owns a comb What can you get a wookie for christmas? when he already owns a comb? What can you get in a hurry for a furry kind of friend like that to take home? No He'll never wear goulashes or a hat upon his furry dome so what can you get a wookie for christmas when he already owns a comb Let's give him love and understanding good will to men wrap it all up in bright colored ribbon and give it to him all over again that's what you get a wookie for christmas when he already owns a comb love and understanding good will to men wrap it all up in bright colored ribbon and give it to him all over again Oh that's what you get a wookie for christmas when he already owns a comb Let's give him love and understanding good will to men wrap it all up in bright colored ribbon and give it to him all over again that's what you get a wookie for christmas when he already owns a comb that's what you get a wookie for christmas when he already owns a comb",
                       as.character(lyrics)))%>%
  mutate(lyrics=ifelse(performer=="Michael Holm",
                       "A ray of hope flickers in the sky A tiny star lights up in your cry All across the land dawns a brand new morn' This comes to pass when a child is born The silent fish sails the seven seas The winds of change whisper in the trees And the words of doubt crumble, toss and turn This comes to pass when a child is born A rosy hue settles all around You've got the feel You're on solid ground Who first fell up no one seems forlorn This comes to pass when a child is born And all of this happens because The world is waiting, waiting for one child Black, white, yellow No one knows but a child that will grow up And turn tears into laughter Hate to love, war to peace And everyone to everyone's neighbor And misery and suffering will be words To be forgotten forever It's all a dream illusion now It must come true sometimes soon somehow All across the land dawns the brand new morn' This comes to pass when a child is born Hum, all across the land dawns a brand new morn' This comes to pass when a child is born", 
                       as.character(lyrics)))


#remove instrumental from those that are instrumentals
##add lyrics to those that were not found



write.csv(agg, "christmaslyrics.csv")



