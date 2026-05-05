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

plan(multisession, workers = 8)

source("functions.R")

source("test_vectorized_rpi.R")
source("compare_rpi.R")
source("analyze_prob_scale.R")

output_list <- getCompleteGames()
completed_schedule <- output_list[["schedule"]]
full_schedule     <- output_list[["full_schedule"]]
team_info         <- output_list[["team_info"]]
teams             <- getTeamList()
ratings <- getLaxNumsRatings()
writeRemainingGames(full_schedule, teams, ratings = ratings)
updateRPISheet(completed_schedule, teams, team_info)
runScenarioGenerator(completed_schedule, teams, team_info)
simulateSeeds(completed_schedule, teams, team_info, ratings = ratings, n_sims = 80000, certain_cutoff = 15, prob_scale = 5)

