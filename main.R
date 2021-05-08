library("googlesheets4")
library(dplyr)
library(purrr)
library(rvest)
library(lubridate)
library(stringr)
setwd("~/Personal/Git/UHSAA-RPI/")

source("functions.R")


getTeamSchedule("Farmington", "https://www.laxnumbers.com/team_info.php?t=14162")

team_info <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846" %>%
  read_sheet(sheet = "Team Information")

full_schedule <- team_info %>%
  select("Team Name", "Schedule URL") %>%
  rename(team_name = "Team Name", url = "Schedule URL") %>%
  as.data.frame() %>%
  pmap_dfr(getTeamSchedule)
  

test_schedule <- full_schedule %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date < as.Date("2021-05-04")) %>%
  mutate(OwnScore = ifelse((Team == "Ridgeline" & 
                            Opponent =="Logan" & 
                            Date == "2021-04-30"), 50, OwnScore)) %>%
  mutate(OwnScore = ifelse((Opponent == "Ridgeline" & 
                            Team =="Logan" & 
                              Date == "2021-04-30"), 0, OwnScore))



scenario <- data.frame(Date= as.Date(today()),
                       Opponent = c("Skyline", "Alta"),
                       Team = c("Alta", "Skyline"),
                       Home = FALSE,
                       OwnScore = c(1, 0),
                       OpponentScore = c(0, 1))
scenario <- data.frame(Date= as.Date(today()),
                       Opponent = c("Pleasant Grove", "Herriman", 
                                    "Orem", "Timpview", 
                                    "Waterford", "Utah Military Academy-Hillfield"),
                       Team = c("Herriman", "Pleasant Grove", "Timpview", "Orem",
                                "Utah Military Academy-Hillfield", "Waterford"),
                       Home = FALSE,
                       OwnScore = c(0, 1, 1, 0, 0, 1),
                       OpponentScore = c(1, 0, 0, 1, 1, 0))

test_schedule <- full_schedule %>%
  filter(!is.na(OwnScore)) %>%
  rbind(scenario)

test_schedule <- full_schedule %>%
  filter(!is.na(OwnScore))

         
teams <- test_schedule %>%
  pull(Team) %>%
  unique()

RPIs <- data.frame(team_name = teams) %>%
  pmap_dfr(calculateRPI, schedule = test_schedule) %>%
  mutate(Team = teams) %>%
  relocate(Team) %>%
  arrange(desc(RPI))

RPIs %>%
  sheet_write("https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846",
              sheet = "TaylorRPI")

calculateRPI("Alta", test_schedule)
calculateWP("Bountiful", test_schedule)
calculateIndividualOWP("Alta", "Timpview", full_schedule)
team_name <- "American Fork"

full_schedule %>%
  filter(Team == team_name) %>%
  rename(team_name = Team, opponent = Opponent) %>%
  select(team_name, opponent) %>%
  pmap_dfr(calculateOWP, schedule = test_schedule) %>%
  summarize(OWP = mean(WP))
