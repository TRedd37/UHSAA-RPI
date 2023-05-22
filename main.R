library("googlesheets4")
library(dplyr)
library(purrr)
library(rvest)
library(lubridate)
library(stringr)
setwd("~/Personal/Git/UHSAA-RPI/")

source("functions.R")

output_list <- getCompleteGames()
completed_schedule <- output_list[["schedule"]]
team_info <- output_list[["team_info"]]
teams <- getTeamList()
updateRPISheet(completed_schedule, teams, team_info)
runScenarioGenerator(completed_schedule, teams)

girls_output_list <- getCompleteGames("Girls")
girls_completed_schedule <- girls_output_list[["schedule"]]
girls_team_info <- girls_output_list[["team_info"]]
girls_teams <- getTeamList("Girls")
updateRPISheet(girls_completed_schedule, teams, girls_team_info, "RPI_Girls")

scenario_df_2 <- list(Game1 = 2, 
     Game2 = 2, 
     Game3 = 2,
     Game4 = 1:2, 
     Game5 = 1:2, 
     Game6 = 1,
     Game7 = 2, 
     Game8 = 2, 
     Game9 = 1:2,
     Game10 = 1, 
     Game11 = 1, 
     Game12 = 1:2,
     Game13 = 1, 
     Game14 = 1, 
     Game15 = 1) %>%
  expand.grid()  %>%
  as.data.frame() 

scenario_df %>%
  mutate(scenarioID = 1:n()) %>%
  filter(Game11 == 1) %>%
  filter(Game1 == 2) %>%
  filter(Game7 == 2)
  
  

output_2 <- scenario_df_2 %>%
  t() %>%
  as.data.frame() %>%
  map(generateRPIForScenario, completed_schedule, teams)

getPath <- function(ll){
  timpview_rank <- ll %>% 
    filter(Classification == "5A") %>% 
    filter(Team == "Timpview") %>%
    pull(RPI_Rank)
  
  if(timpview_rank == 10){
    first_game <- ll %>% 
      filter(Classification == "5A") %>% 
      filter(RPI_Rank == 23) %>%
      pull(Team)
    second_game <- ll %>% 
      filter(Classification == "5A") %>% 
      filter(RPI_Rank == 7) %>%
      pull(Team)
  } else if (timpview_rank == 11){
    first_game <- ll %>% 
      filter(Classification == "5A") %>% 
      filter(RPI_Rank == 22) %>%
      pull(Team)
    second_game <- ll %>% 
      filter(Classification == "5A") %>% 
      filter(RPI_Rank == 6) %>%
      pull(Team)
  } else {
    first_game <- "unknown"
    second_game <- "unknown"
  }
  print(paste(first_game, ">", second_game))
  invisible(NULL)
}

output %>%
  map(getPath )


RPIs <- data.frame(team_name = girls_teams) %>%
  pmap_dfr(calculateRPI, 
           schedule = girls_completed_schedule, 
           team_info = girls_team_info) %>%
  mutate(Team = teams) 

RPIs %>%
  left_join(girls_team_info %>% select("Team Name", "Classification"),
            by = c("Team" = "Team Name")) %>% 
  arrange(desc(Classification), desc(RPI)) %>%
  group_by(Classification) %>%
  mutate(RPI_Rank = row_number()) %>%
  relocate(RPI_Rank, Team, RPI) %>%
  print(n= 100)



unfinished_games <- full_schedule %>%
  filter(is.na(OwnScore)) %>%
  filter(Team %in% teams)

unfinished_games %>%
  filter(Home) %>%
  select(Opponent, Team, Date) %>%
  sheet_write("https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846",
              sheet = "All Remaining")




teams <- team_info %>%
  filter(`LaxNums ID` != "") %>%
  pull("Team Name")

RPIs <- data.frame(team_name = teams) %>%
  pmap_dfr(calculateRPI, 
           schedule = completed_schedule, 
           team_info = team_info) %>%
  mutate(Team = teams) 

RPIs %>%
  left_join(team_info %>% select("Team Name", "Classification"),
            by = c("Team" = "Team Name")) %>% 
  arrange(desc(Classification), desc(RPI)) %>%
  group_by(Classification) %>%
  mutate(RPI_Rank = row_number()) %>%
  relocate(RPI_Rank, Team, RPI) %>%
  sheet_write("https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846",
              sheet = "RPI")



finished_games <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846" %>%
  read_sheet(sheet = "Finished Games") %>%
  rename("OwnScore" = "Own Score",
         "OpponentScore" = "Opponent Score")

finished_games_doubled <- finished_games %>%
  mutate(TeamNew = Opponent,
         Opponent = Team,
         Team = TeamNew,
         Temp = `OwnScore`,
         `OwnScore` = `OpponentScore`,
         `OpponentScore` = Temp,
         Home = FALSE) %>%
  select(-c(Temp, TeamNew)) %>%
  bind_rows(finished_games)

test_schedule <- completed_schedule %>%
  bind_rows(finished_games_doubled)


RPIs <- data.frame(team_name = teams) %>%
  pmap_dfr(calculateRPI, schedule = test_schedule, 
           team_info = team_info) %>%
  mutate(Team = teams)

RPIs %>%
  left_join(team_info %>% select("Team Name", "Classification"),
            by = c("Team" = "Team Name")) %>% 
  arrange(desc(Classification), desc(RPI)) %>%
  group_by(Classification) %>%
  mutate(RPI_Rank = row_number()) %>%
  relocate(RPI_Rank, Team, RPI) %>%
  sheet_write("https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846",
              sheet = "RPI_finished")
