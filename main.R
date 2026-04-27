library("googlesheets4")
library(dplyr)
library(purrr)
library(rvest)
library(lubridate)
library(stringr)
setwd("~/Documents/GitHub/UHSAA-RPI/")

source("functions.R")

output_list <- getCompleteGames()
completed_schedule <- output_list[["schedule"]]
team_info <- output_list[["team_info"]]
teams <- getTeamList()
updateRPISheet(completed_schedule, teams, team_info)
runScenarioGenerator(completed_schedule, teams, team_info)

girls_output_list <- getCompleteGames("Girls")
girls_completed_schedule <- girls_output_list[["schedule"]]
girls_team_info <- girls_output_list[["team_info"]]
girls_teams <- getTeamList("Girls")
updateRPISheet(girls_completed_schedule, girls_teams, girls_team_info, "RPI_Girls")
