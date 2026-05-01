library("googlesheets4")
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(rvest)
library(lubridate)
library(stringr)
library(jsonlite)
setwd("~/Documents/GitHub/UHSAA-RPI/")

plan(multisession)

source("functions.R")

output_list <- getCompleteGames()
completed_schedule <- output_list[["schedule"]]
full_schedule     <- output_list[["full_schedule"]]
team_info         <- output_list[["team_info"]]
teams             <- getTeamList()
writeRemainingGames(full_schedule, teams)
updateRPISheet(completed_schedule, teams, team_info)
runScenarioGenerator(completed_schedule, teams, team_info)
simulateSeeds(completed_schedule, teams, team_info)
