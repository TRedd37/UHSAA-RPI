SHEET_URL <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846"
LAXNUMS_BASE <- "https://www.laxnumbers.com/team_info.php"

getOpponentIDs <- function(url){
  url %>%
    read_html() %>%
    html_nodes(xpath = "//table//a") %>%
    html_attrs() %>%
    unlist() %>%
    as.data.frame() %>%
    filter(str_detect(., "team_info")) %>%
    mutate(team_id = str_replace(., "team_info\\.php\\?y=\\d\\d\\d\\d&t=", "")) %>%
    select(team_id)
}

getTeamSchedule <- function(team_id, year = year(today())) {
  url <- str_glue("{LAXNUMS_BASE}?y={year}&t={team_id}")

  team_name <- url %>%
    read_html() %>%
    html_elements("p") %>%
    html_text() %>%
    as.data.frame() %>%
    filter(str_detect(., "Season Totals")) %>%
    mutate(team_name = str_replace(., "\\d\\d\\d\\d Season Totals for the ", "")) %>%
    mutate(team_name = str_replace(team_name, fixed("(Report missing game scores)"), "")) %>%
    mutate(team_name = str_trim(team_name)) %>%
    pull(team_name)

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
           Result = str_replace(Result, " \\d*OT", ""),
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
    mutate(Opponent = ifelse(Opponent == "SteamboSprings",
                        "Steamboat Springs", Opponent)) %>%
    filter(Team == team_name) %>%
    rename(team_name = Team, opponent = Opponent) %>%
    select(team_name, opponent) %>%
    pmap_dfr(calculateIndividualOWP, schedule = schedule) %>%
    summarize(OWP = mean(WP))
}

calculateRPI <- function(team_name, schedule, team_info) {
  WP <- team_name %>%
    calculateWP(schedule)

  OWP <- calculateOWP(team_name, schedule)

  opponents <- schedule %>%
    filter(Team == team_name) %>%
    distinct() %>%
    pull(Opponent)

  opp_WP <- data.frame(team_name = opponents) %>%
    pmap_dfr(calculateOWP, schedule) %>%
    mutate(Team = opponents) %>%
    left_join(team_info %>% select("Team Name", UtahNeighbor),
              by = c("Team" = "Team Name")) %>%
    mutate(OWP = ifelse(UtahNeighbor, OWP, 0.5)) %>%
    select(OWP)

  OOWP <- data.frame(Opponent = opponents, WP = opp_WP) %>%
    summarize(OOWP = mean(OWP))

  output <- data.frame(WP = WP,
                       OWP = OWP,
                       OOWP = OOWP,
                       RPI = as.numeric(0.45 * WP + 0.45 * OWP + 0.1 * OOWP))
  return(output)
}

createGames <- function(Team1, Team2, Winner){
  if(!(Winner %in% c(Team1, Team2))){
    stop("Winner must be Team1 or Team 2")
  }

  games <- data.frame(Date= as.Date(today()),
            Opponent = c(Team1, Team2),
            Team = c(Team2, Team1),
            Home = FALSE,
            OwnScore = as.numeric(c(Team2 == Winner, Team1 == Winner)),
            OpponentScore = as.numeric(c(Team1 == Winner, Team2 == Winner)))
  return(games)
}

createSenarios <- function(scenarios){
  all_games <- scenarios %>%
    pmap_dfr(createGames)
  return(all_games)
}

generateRPIForScenario <- function(picks, completed_schedule, teams, team_info){

  scenarios_game_names <- SHEET_URL %>%
    read_sheet(sheet = "Taylor's Guess") %>%
    select(-Pick)

  scenarios <- scenarios_game_names %>%
    mutate(Pick = picks)

  scenario_first_half <- scenarios %>%
    mutate(Team = Team1,
           Opponent = Team2,
           Home = TRUE,
           OwnScore = as.numeric(Pick == 1),
           OpponentScore = as.numeric(Pick == 2)) %>%
    select(Date, Opponent, Team, Home, OwnScore, OpponentScore)

  scenario_second_half <- scenarios %>%
    mutate(Team = Team2,
           Opponent = Team1,
           Home = FALSE,
           OwnScore = as.numeric(Pick == 2),
           OpponentScore = as.numeric(Pick == 1)) %>%
    select(Date, Opponent, Team, Home, OwnScore, OpponentScore)

  scenario_games <- scenario_first_half %>%
    bind_rows(scenario_second_half)

  scenario_schedule <- completed_schedule %>%
    bind_rows(scenario_games)

  RPIs <- data.frame(team_name = teams) %>%
    pmap_dfr(calculateRPI,
             schedule = scenario_schedule,
             team_info = team_info) %>%
    mutate(Team = teams)

  RPIs %>%
    left_join(team_info %>% select("Team Name", "Classification"),
              by = c("Team" = "Team Name")) %>%
    arrange(desc(Classification), desc(RPI)) %>%
    group_by(Classification) %>%
    mutate(RPI_Rank = row_number()) %>%
    relocate(RPI_Rank, Team, RPI)
}

runScenarioGenerator <- function(completed_schedule, teams, team_info,
                                 sheet_out = "RPI_scenarios"){
  scenarios <- SHEET_URL %>%
    read_sheet(sheet = "Taylor's Guess") %>%
    mutate(Date = as.character(as.Date(Date)))

  scenario_first_half <- scenarios %>%
    mutate(Team = Team1,
           Opponent = Team2,
           Home = TRUE,
           OwnScore = as.numeric(Pick == 1),
           OpponentScore = as.numeric(Pick == 2)) %>%
    select(Date, Opponent, Team, Home, OwnScore, OpponentScore)

  scenario_second_half <- scenarios %>%
    mutate(Team = Team2,
           Opponent = Team1,
           Home = FALSE,
           OwnScore = as.numeric(Pick == 2),
           OpponentScore = as.numeric(Pick == 1)) %>%
    select(Date, Opponent, Team, Home, OwnScore, OpponentScore)

  scenario_games <- scenario_first_half %>%
    bind_rows(scenario_second_half)

  scenario_schedule <- completed_schedule %>%
    bind_rows(scenario_games)

  RPIs <- data.frame(team_name = teams) %>%
    pmap_dfr(calculateRPI,
             schedule = scenario_schedule,
             team_info = team_info) %>%
    mutate(Team = teams)

  RPIs %>%
    left_join(team_info %>% select("Team Name", "Classification"),
              by = c("Team" = "Team Name")) %>%
    arrange(desc(Classification), desc(RPI)) %>%
    group_by(Classification) %>%
    mutate(RPI_Rank = row_number()) %>%
    relocate(RPI_Rank, Team, RPI) %>%
    sheet_write(SHEET_URL, sheet = sheet_out)
}

getTeamList <- function(sheet_name = "Team Information"){
  utah_team_info <- SHEET_URL %>%
    read_sheet(sheet = sheet_name) %>%
    filter(!is.na(Classification))

  teams <- utah_team_info %>%
    filter(`LaxNums ID` != "") %>%
    pull("Team Name")

  return(teams)
}

buildOutOfStateTeamInfo <- function(missing_ids, year = year(today())){
  paste0(LAXNUMS_BASE, "?y=", year, "&t=", missing_ids) %>%
    map_dfr(buildOutOfStateRow)
}

buildOutOfStateRow <- function(url){
  state <- url %>%
    read_html() %>%
    html_nodes(xpath = "//table//a") %>%
    as.character() %>%
    as.data.frame() %>%
    slice(3) %>%
    str_replace('.* - ', "") %>%
    str_replace('</a>', "")

  team_name <- url %>%
    read_html() %>%
    html_elements("p") %>%
    html_text() %>%
    as.data.frame() %>%
    filter(str_detect(., "Season Totals")) %>%
    mutate(team_name = str_replace(., "\\d\\d\\d\\d Season Totals for the ", "")) %>%
    mutate(team_name = str_replace(team_name, fixed("(Report missing game scores)"), "")) %>%
    mutate(team_name = str_trim(team_name)) %>%
    pull(team_name)

  data.frame("Team Name" = team_name,
             UtahNeighbor = ifelse(state %in% c("Colorado"), TRUE, FALSE),
             "Schedule URL" = url,
             check.names = FALSE)
}

getCompleteGames <- function(sheet_name = "Team Information", year = year(today())){
  utah_team_info <- SHEET_URL %>%
    read_sheet(sheet = sheet_name) %>%
    filter(!is.na(Classification))

  utah_opponent_ids <- utah_team_info %>%
    filter(`LaxNums ID` != "") %>%
    mutate(url = paste0(LAXNUMS_BASE, "?y=", year, "&t=", `LaxNums ID`)) %>%
    pull(url) %>%
    map_dfr(getOpponentIDs) %>%
    pull(team_id) %>%
    unique()

  missing_ids <- utah_opponent_ids %>% setdiff(utah_team_info$`LaxNums ID`)
  missing_team_info <- buildOutOfStateTeamInfo(missing_ids, year)

  out_of_state_ids <- missing_team_info %>%
    filter(UtahNeighbor) %>%
    pull(`Schedule URL`) %>%
    map_dfr(getOpponentIDs) %>%
    pull(team_id) %>%
    unique()

  all_ids <- c(utah_opponent_ids, out_of_state_ids) %>% unique()

  team_info <- utah_team_info %>%
    bind_rows(missing_team_info)

  full_schedule <- all_ids %>%
    map_dfr(getTeamSchedule, year = year)

  completed_schedule <- full_schedule %>%
    filter(!is.na(OwnScore))

  list(schedule = completed_schedule, team_info = team_info)
}

updateRPISheet <- function(completed_schedule, teams, team_info, sheet_name = "RPI"){
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
    sheet_write(SHEET_URL, sheet = sheet_name)
  invisible(NULL)
}
