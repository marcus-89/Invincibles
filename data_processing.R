# functions for various stages of data processing
# seasonal data can be downloaded in xls @ http://www.football-data.co.uk/downloadm.php


# loading raw data and cleaning the basics (relevant columns, NA removal etc)
loadRawData = function(sheetname) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  xls_data = dir("data/", full.names=T)
  nseason = length(xls_data)
  all_data = vector("list", nseason)
  
  # load data from all available seasons
  for(i in 1:nseason){
    all_data[[i]] = read_excel(xls_data[i], sheet = sheetname) #sheet 1 = EPL
    
    # because varying information is available for each season, we drop useless columns already here
    # to allow data to be combined smoother
    all_data[[i]] %>%
      select(Date, HomeTeam, AwayTeam, FTHG, FTAG) -> all_data[[i]]
  }
  
  # gather into one data frame
  df = do.call("rbind", all_data)
  
  # omits some inexplicable trailing NA rows present in the first few seasons
  df %>%
    drop_na() -> df
  
  # add factor variable for season
  first_available_season = min(as.numeric(format(df$Date, "%Y")))
  last_available_season = max(as.numeric(format(df$Date, "%Y")))+1
  years = first_available_season:last_available_season
  matches = df %>%
    mutate(season = cut(df$Date, breaks=as.POSIXct(paste(years,"-07-01", sep="")), 
                        labels = paste(years[-length(years)],
                                       sprintf('%02d', (years[-length(years)]+1) %% 100),sep="-")))
  
  # returns df with one row/match
  return(matches)
}


# pivot to one row per team/match. accepts a df returned by loadRawData()
pivotByTeam = function(df) {
  by_team = df %>%
    pivot_longer(c(HomeTeam, AwayTeam), names_to = "home_away", values_to = "team") %>%
    mutate(goals_for = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           goals_against = ifelse(home_away == "HomeTeam", FTAG, FTHG),
           pts = ifelse(goals_for > goals_against, 3, 
                        ifelse(goals_for == goals_against, 1, 0)),
           round_counter = 1) %>%
    group_by(season, team) %>%
    mutate (round = cumsum(round_counter)) %>%
    select(-round_counter, -FTAG, -FTHG)
  
  # returns df with one row/team/match
  return(by_team)
}

# extracting information about rounds until first loss by team/season. accepts a df returned by pivotByTeam()
getFirstLosses = function(df) {
  
  # we begin by identifying which teams/seasons did not lose (or have not yet lost in current season)
  havent_lost = df %>% 
    group_by(season, team) %>% 
    mutate(losses = sum(pts == 0)) %>%
    filter(losses == 0) %>%
    arrange(desc(Date)) %>%
    distinct(season, team, .keep_all = T) %>%
    select(-losses)
  # (this gives warning and leaves an empty df if nobody went through a season without losing, ignore it)
  
  # extract which round teams first lost in (subtract one to get the more useful "matches without loss" statistic)
  until_loss = df %>%
    ungroup() %>%
    filter(pts == 0) %>%
    distinct(season, team, .keep_all = T) %>%
    mutate(round = round - 1) %>%
    # adds the set of teams that havent/never lost in a season
    full_join(havent_lost) %>% 
    # adds current season factor for future plot convenience
    mutate(current_season = Date > as.Date("2019-07-10")) %>% 
    arrange(Date)
  
  
  # filter for longest unbeaten start/season
  longest_byseason = until_loss %>%
    group_by(season) %>%
    filter(round == max(round)) %>%
    arrange(Date) %>%
    ungroup() %>%
    mutate(did_lose = pts == 0)
  
  # identify seasons with more than one winner
  longest_byseason %>% 
    group_by(season) %>% 
    count() %>%
    filter(n > 1) -> ties
  
  longest_byseason %>%
    filter(season %in% ties$season) %>%
    group_by(season) %>%
    mutate(tied_teams = paste0(team, collapse="/")) %>%
    select(season, tied_teams, team) %>%
    distinct(season, .keep_all = T) %>%
    ungroup() -> tied_teams
  
  # edit longest by season factor to include all tied teams after distinct by season
  longest_byseason %>% 
    distinct(season, .keep_all = T) %>%
    left_join(tied_teams) %>% 
    mutate(team = if_else(is.na(tied_teams), team, tied_teams)) %>%
    select(-tied_teams) -> longest_byseason
}




