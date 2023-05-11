library("googlesheets4")
library(dplyr)
library(purrr)
library(rvest)
library(lubridate)
library(stringr)
setwd("~/Personal/Git/UHSAA-RPI/")

source("functions.R")

completed_schedule <- getCompleteGames()
teams <- getTeamList()
updateRPISheet(completed_schedule, teams)
runScenarioGenerator(completed_schedule, teams)

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
