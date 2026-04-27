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
