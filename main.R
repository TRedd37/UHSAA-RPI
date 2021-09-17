library("googlesheets4")
library(dplyr)
library(purrr)
library(rvest)
library(lubridate)
library(stringr)
setwd("~/Personal/Git/UHSAA-RPI/")

source("functions.R")

team_info <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846" %>%
  read_sheet(sheet = "Team Information")

full_schedule <- team_info %>%
  select("Team Name", "Schedule URL") %>%
  rename(team_name = "Team Name", url = "Schedule URL") %>%
  as.data.frame() %>%
  pmap_dfr(getTeamSchedule)

finished_games <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846" %>%
  read_sheet(sheet = "Finished Games") %>%
  filter(!is.na(Winner)) %>%
  select(-c(Date, Lock)) %>%
  createSenarios()

scenarios <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846" %>%
  read_sheet(sheet = "All Remaining") %>%
  filter(!is.na(Winner)) %>%
  select(-c(Date, Lock)) %>%
  createSenarios()
  
# test_schedule <- full_schedule %>%
#   mutate(Date = as.Date(Date)) %>%
#   filter(Date < as.Date("2021-05-04")) %>%
#   mutate(OwnScore = ifelse((Team == "Ridgeline" & 
#                             Opponent =="Logan" & 
#                             Date == "2021-04-30"), 50, OwnScore)) %>%
#   mutate(OwnScore = ifelse((Opponent == "Ridgeline" & 
#                             Team =="Logan" & 
#                               Date == "2021-04-30"), 0, OwnScore))

full_schedule <- full_schedule %>%
  filter(!is.na(OwnScore))

test_schedule <- full_schedule %>%
  filter(!is.na(OwnScore)) %>%
  rbind(scenarios)

teams <- test_schedule %>%
  pull(Team) %>%
  unique()

RPIs <- data.frame(team_name = teams) %>%
  pmap_dfr(calculateRPI, schedule = full_schedule) %>%
  mutate(Team = teams) %>%
  arrange(desc(RPI)) %>%
  mutate(RPI_Rank = row_number()) %>%
  relocate(RPI_Rank, Team, RPI) 

RPIs %>%
  sheet_write("https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846",
              sheet = "RPI")

calculateRPI("Alta", test_schedule)
calculateWP("Bountiful", test_schedule)
calculateIndividualOWP("Alta", "Timpview", full_schedule)
team_name <- "Olympus"

full_schedule %>%
  filter(Team == team_name) %>%
  rename(team_name = Team, opponent = Opponent) %>%
  select(team_name, opponent) %>%
  pmap_dfr(calculateOWP, schedule = test_schedule) %>%
  summarize(OWP = mean(WP))

full_schedule %>%
  filter(is.na(OwnScore)) %>% 
  nrow()
