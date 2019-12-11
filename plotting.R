byseason_barplot = function(df){
  # custom ggplot theme
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
  
  # extract coords used for geom_segment lines
  df %>% 
    group_by(season) %>% 
    summarize(rounds = max(round)) %>%
    mutate(x_axis = 0.5:nrow(.))  %>%
    mutate(x_axis = ifelse(row_number() == n(), x_axis+1, x_axis),
           # extend rounds in current season to match previous season (since data is incomplete for current)
           # NOTE: will be incorrect under the very rare circumstance when totalrounds_currentyear != totalrounds_lastyear
           rounds = ifelse(row_number() == n(), lag(rounds), rounds)) -> coords
  
  
  # generate limits and reasonable tick interval on y axis
  maxround = max(coords$rounds)
  axisbrks = if((maxround %% 10) > 5){
    c(seq(0,maxround,10),distinct(coords, rounds) %>% pull())
  } else{
    c(seq(0,(maxround-10),10),distinct(coords, rounds) %>% pull())
  }
  
  # line segment coords
  coords %>%
    filter(row_number() %in% c(1, n())  | (rounds != lag(rounds))) -> coords
  
  segment_data = data.frame(
    x = coords %>% filter(row_number() != n()) %>% pull(x_axis),
    xend = coords %>% filter(row_number() != 1) %>% pull(x_axis),
    y = coords %>% filter(row_number() != n()) %>% pull(rounds),
    yend = coords %>% filter(row_number() != n()) %>% pull(rounds)
  )
  
  longest_byseason = getFirstLosses(df)
  
  p = longest_byseason %>%
    ggplot() + 
    geom_col(aes(season, round, fill = team)) +
    labs(x = "Season", y = "Number of matches unbeaten", fill = "Team") +
    scale_y_continuous(breaks = axisbrks, limits = c(0,maxround+2)) +
    geom_segment(data = segment_data, aes(x=x, xend=xend, y=y, yend=yend), lty=2) +
    custom_theme() +
    theme(axis.text.x = element_text(angle=90))
  
  return(p)
}

