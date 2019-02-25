#clean up the enviornment
rm(list = ls())

#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(directlabels)
library(gganimate)
library(ggrepel)

#set working directory
setwd("~/Documents/hockey/cow")

#read csv files
mydata<- read_csv("cow18-19.csv")

#select columnns required
mydata <- mydata %>%
  select(player, pos, season_start, pts_gp1, pts_gp2, pts_gp3, pts_gp4, pts_gp5) %>%
  rename(`0` = "season_start", `1` = "pts_gp1", `2` = "pts_gp2", `3` = "pts_gp3", `4` = "pts_gp4",
         `5` = "pts_gp5")

#wide to long
mydata_tidy <- gather(data = mydata, key = segment, value = pts_gp,
                      -player, -pos)

#convert segment to character
mydata_tidy$segment <- as.numeric(as.character(mydata_tidy$segment))

#filter players
mydata_tidy_players <- mydata_tidy %>%
  filter(player == "JOHNNY HARRISON" |
         player == "TOMAS QUICK" |
         player == "KIERAN FLYNNN" |
         player == "JAKE TRAYNOR" |
         player == "ALEC BROWN")

#filter by position
mydata_tidy_defense <- mydata_tidy %>%
  filter(pos == "d")

#filter by position
mydata_tidy_forward <- mydata_tidy %>%
  filter(pos == "f")

#*********************************************************************
#plot all players filtered in mydata_tidy_players with labels
labels_plot_players <- ggplot(mydata_tidy_players,
                              aes(x=segment, y=pts_gp, 
                                  group = player, color = player)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(x = segment, label = player, color = player),
  direction = 'y', nudge_x = 1.5, segment.color = 'black') +
  coord_cartesian(clip = 'off') +
  #theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line = element_line(colour ="black")) +
  theme(legend.position = "none") +
  guides(colour = FALSE) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5),labels=c("start","1","2","3","4","5"),
                     expand=c(0,1), limits=c(0,7)) +
  ylab("PPG") +
  xlab("Segments") +
  ggtitle("COW 2018-19", subtitle = "Points per Game by Segment") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  transition_reveal(along = segment) +
  ease_aes('linear')

animate(labels_plot_players, 100, 10)

#save the animation
anim_save(labels_plot_players, filename = "players.gif")

#write file
write.csv(mydata_tidy, file = "~/Documents/hockey/cow/points_per_game.csv", row.names = F)
