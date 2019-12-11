library(tidyverse)
library(readxl)
library(png)
library(grid)
library(RColorBrewer)

# load functions
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data_processing.R")
source("plotting.R")


# these four functions loads data, processes it, and plots a barplot over the best attempt at becoming "invincibles" 
# each season can be generalized to many different leagues (see raw excel data for more information) by changing the
# parameter in loadRawData to another excel sheet name or sheet page
# "E0" = Premier League, "E1" Championship, etc. 
# sheetnames from other leagues follow similar patterns (see excel file), "I1" = Serie A, "F1" = Ligue Un etc.

df = loadRawData("E0")
pivotdf = pivotByTeam(df)
longest_byseason = getFirstLosses(pivotdf)
byseason_barplot(pivotdf)


#### below is some manual additions specifically for the premier league 
#### (including adding the 1992-93 season manually which was unavailable in the source data)
custom_theme <- function() {
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  theme_bw(base_size=9) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major.y = element_line(size = 0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# adds 1992-93's ipswich and factor level for 92-93
longest_byseason %>%
  add_row(Date = as.Date("1992-09-19"),
          season = "1992-93",
          home_away = "AwayTeam",
          team = "Ipswich",
          goals_for = 2,
          goals_against = 4,
          pts = 0,
          round = 8,
          current_season = F,
          did_lose = T) %>%
  mutate(season = fct_reorder(season, Date)) %>% 
  arrange(season) -> longest_byseason

segments = data.frame(x = c(0.5,3.5), xend = c(3.5,28.5), y=c(42, 38), yend = c(42,38))
# plotting without fill = team
p = longest_byseason %>%
  ggplot() + 
  geom_col(aes(season, round, alpha = 0+current_season, fill = did_lose)) +
  labs(x = "Season", y = "Number of (initial) matches unbeaten") +
  scale_y_continuous(breaks = c(0,10,20,30,38,42), limits = c(0,42)) +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  scale_alpha(range = c(0.7,1)) +
  geom_segment(data = segments, aes(x=x, xend=xend, y=y, yend=yend), linetype=2) +
  custom_theme() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none")
p

# load logos into R
teams = c("arsenal", "astonvilla", "chelsea", "everton",
          "leicester", "liverpool", "manutd", "mancity", "tottenham",
          "leeds", "coventry", "newcastle", "nottingham", "newcastle+nottingham", "ipswich")
paths = paste0("images/", teams, ".png")

grab_img = function(path) {
  img <- readPNG(path)
  g <- rasterGrob(img, interpolate=TRUE)
  return(g)
}

imgs = vector("list", length(teams))
k = 1
for(i in paths){
  imgs[[k]] = grab_img(i)
  k = k+1
}

# extracts y-axis values to generate y-coordinates for logos
vals = longest_byseason$round
team_index = c(15, 11, 14, 13, 7, 1, 2, 7, 5, 10, 
               6, 1, 1, 3, 2, 1, 6, 3, 7, 8, 8,
               4, 3, 5, 9, 8, 6, 6) #(indices of team w longest unbeaten run/season relating to [teams] vector above)

# adds logos to plot
for(i in 1:length(vals)){
  #if a tie (= team name previously changed to teamA/teamB) adjust ymax to fit the combined logo
  if(grepl("/", longest_byseason$team)[i]){
    p = p + annotation_custom(imgs[[team_index[i]]], ymin = vals[i], ymax = vals[i]+6, xmin = i-0.5, xmax = i+0.5)
  } else{
    p = p + annotation_custom(imgs[[team_index[i]]], ymin = vals[i], ymax = vals[i]+4, xmin = i-0.5, xmax = i+0.5)
  }
}

p

ggsave("invinciblesPremierLeague.png", last_plot(), dpi = 1080)
