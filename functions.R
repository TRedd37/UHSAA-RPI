getTeamSchedule <- function(team_name, url) {
  tables <- url %>%
    read_html() %>%
    html_table()
  schedule <- tables[[2]] %>%
    mutate(Team = team_name,
           Home = !str_detect(Opponent, "at "),
           Team = str_replace(Team, "Utah Military Academy - Hill Field",
                              "Utah Military Academy-Hillfield"),
           Opponent = str_replace(Opponent, "vs ", ""),
           Opponent = str_replace(Opponent, "at ", ""),
           Result = str_replace(Result, " OT", ""),
           Result = str_replace(Result, fixed(" (F)"), ""),
           OwnScore = str_split_fixed(Result, " - ", 2)[,1] %>%
             as.numeric(),
           OpponentScore = str_split_fixed(Result, " - ", 2)[,2] %>% 
             as.numeric()) %>%
    select(-c(Time, Fix, Result, Location)) %>%
    suppressWarnings()
  return(schedule)
}

calculateWP <- function(team_name, schedule) {
  schedule %>%
    filter(Team == team_name,
           !is.na(OwnScore)) %>%
    summarize(WP = mean(OwnScore > OpponentScore))
}

calculateIndividualOWP <- function(team_name, opponent, schedule){
  WP <- schedule %>%
    filter(Team == opponent,
           Opponent != team_name,
           !is.na(OwnScore)) %>%
    summarize(WP = mean(OwnScore > OpponentScore))
  data.frame(Opponent = opponent, WP = WP)
}

calculateOWP <- function(team_name, schedule){
  schedule %>%
    filter(Team == team_name) %>%
    rename(team_name = Team, opponent = Opponent) %>%
    select(team_name, opponent) %>%
    pmap_dfr(calculateIndividualOWP, schedule = schedule) %>%
    summarize(OWP = mean(WP))
}

calculateRPI <- function(team_name, schedule) {
  WP <- team_name %>%
    calculateWP(schedule)
  OWP <- calculateOWP(team_name, schedule)
  
  opponents <- schedule %>%
    filter(Team == team_name) %>%
    pull(Opponent)
  
  opp_WP <- data.frame(team_name = opponents) %>%
    pmap_dfr(calculateOWP, schedule)
  OOWP <- data.frame(Opponent = opponents, WP = opp_WP) %>%
    summarize(OOWP = mean(OWP))
  
  output <- data.frame(WP = WP, 
                       OWP = OWP, 
                       OOWP = OOWP,
                       RPI = as.numeric(0.45 * WP + 0.45 * OWP + 0.1 * OOWP))
  return(output)
}