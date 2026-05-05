library(furrr)

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1Qfa8i306cl47qistk-3D9NEapFn5XLG-8QGn006ySJ4/edit#gid=229011846"
LAXNUMS_BASE <- "https://www.laxnumbers.com/team_info.php"
LAXNUMS_RATINGS_BASE <- "https://www.laxnumbers.com/ratings.php"

BOYS_CLASSIFICATION_VIEWS <- c("6A" = 3531, "5A" = 3532, "4A" = 3533)

# Games excluded from RPI (out-of-state opponents that UHSAA doesn't count).
EXCLUDED_GAMES_2026 <- data.frame(
  Team     = c("Lehi", "Skyridge", "Skyridge"),
  Opponent = c("Palo Verde", "San Marcos SD", "St Augustine"),
  stringsAsFactors = FALSE
)

# Upcoming games not yet on LaxNumbers — added to full_schedule so they appear
# in writeRemainingGames and simulateSeeds. Both perspectives needed per game.
MANUAL_REMAINING_GAMES_2026 <- data.frame(
  Date         = c("2026-05-06", "2026-05-06", "2026-05-06", "2026-05-06"),
  Team         = c("Waterford", "Jordan", "Highland", "Juan Diego Catholic"),
  Opponent     = c("Jordan", "Waterford", "Juan Diego Catholic", "Highland"),
  Home         = c(TRUE, FALSE, TRUE, FALSE),
  OwnScore     = c(NA_real_, NA_real_, NA_real_, NA_real_),
  OpponentScore= c(NA_real_, NA_real_, NA_real_, NA_real_),
  stringsAsFactors = FALSE
)

# Games missing from LaxNumbers that should count toward RPI.
# Each matchup needs two rows (both perspectives). Scores only need to reflect W/L.
MANUAL_GAMES_2026 <- data.frame(
  Date         = c("2026-04-25", "2026-04-25"),
  Team         = c("Cedar Valley", "Canyon View"),
  Opponent     = c("Canyon View",  "Cedar Valley"),
  Home         = c(TRUE, FALSE),
  OwnScore     = c(1, 0),
  OpponentScore= c(0, 1),
  stringsAsFactors = FALSE
)

readHtmlWithRetry <- function(url, attempts = 3, wait = 5) {
  for (i in seq_len(attempts)) {
    result <- tryCatch({
      resp <- curl::curl_fetch_memory(url)
      if (resp$status_code %in% c(403L, 404L))
        stop("HTTP error ", resp$status_code)
      read_html(rawToChar(resp$content))
    }, error = function(e) e)
    if (!inherits(result, "error")) return(result)
    msg <- conditionMessage(result)
    if (grepl("403|404|HTTP error", msg, ignore.case = TRUE)) stop(result)
    if (i < attempts) Sys.sleep(wait)
  }
  stop("Failed to fetch ", url, " after ", attempts, " attempts")
}

getUHSAAClassifications <- function(view_ids = BOYS_CLASSIFICATION_VIEWS,
                                    year = NULL) {
  if (is.null(year)) year <- lubridate::year(today())
  map2_dfr(names(view_ids), unname(view_ids), function(class_name, view_id) {
    url <- str_glue("https://www.laxnumbers.com/ratings/service?y={year}&v={view_id}")
    teams <- NULL
    for (i in seq_len(5)) {
      teams <- tryCatch(jsonlite::fromJSON(url), error = function(e) e)
      if (!inherits(teams, "error")) break
      Sys.sleep(10 * i)
    }
    if (inherits(teams, "error")) stop("Failed to fetch classifications for ", class_name)
    data.frame(
      "Team Name"      = teams$name,
      "LaxNums ID"     = as.character(teams$team_nbr),
      "Classification" = class_name,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
}

getOpponentIDs <- function(url){
  url %>%
    readHtmlWithRetry() %>%
    html_nodes(xpath = "//table//a") %>%
    html_attrs() %>%
    unlist() %>%
    as.data.frame() %>%
    filter(str_detect(., "team_info")) %>%
    mutate(team_id = str_replace(., "team_info\\.php\\?y=\\d\\d\\d\\d&t=", "")) %>%
    select(team_id)
}

getTeamSchedule <- function(team_id, year = NULL) {
  if (is.null(year)) year <- lubridate::year(today())
  url <- paste0(LAXNUMS_BASE, "?y=", year, "&t=", team_id)

  page <- tryCatch(
    readHtmlWithRetry(url),
    error = function(e) NULL
  )
  if (is.null(page)) return(data.frame())

  team_name <- page %>%
    html_elements("p") %>%
    html_text() %>%
    as.data.frame() %>%
    filter(str_detect(., "Season Totals")) %>%
    mutate(team_name = str_replace(., "\\d\\d\\d\\d Season Totals for the ", "")) %>%
    mutate(team_name = str_replace(team_name, fixed("(Report missing game scores)"), "")) %>%
    mutate(team_name = str_trim(team_name)) %>%
    pull(team_name)

  tables <- page %>%
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
    mutate(Date = as.character(Date)) %>%
    suppressWarnings()
  return(schedule)
}

getTeamRatingMath <- function(team_id, year = NULL) {
  if (is.null(year)) year <- lubridate::year(today())
  url <- paste0(LAXNUMS_BASE, "?y=", year, "&t=", team_id, "&s=1")

  page <- tryCatch(readHtmlWithRetry(url), error = function(e) NULL)
  if (is.null(page)) return(data.frame())

  team_name <- page %>%
    html_elements("p") %>%
    html_text() %>%
    as.data.frame() %>%
    filter(str_detect(., "Season Totals")) %>%
    mutate(team_name = str_replace(., "\\d\\d\\d\\d Season Totals for the ", ""),
           team_name = str_replace(team_name, fixed("(Report missing game scores)"), ""),
           team_name = str_trim(team_name)) %>%
    pull(team_name)
  if (length(team_name) == 0) team_name <- NA_character_

  tables <- html_table(page)

  # Find the rating math table by column signature (has "+/-" column)
  rm_idx <- which(sapply(tables, function(t)
    "+/-" %in% names(t) || "GD" %in% names(t)))
  if (length(rm_idx) == 0) return(data.frame())

  tbl <- tables[[rm_idx[1]]]
  names(tbl)[names(tbl) == "+/-"] <- "PlusMinus"

  tbl %>%
    mutate(Team       = team_name,
           team_id    = as.character(team_id),
           PlusMinus  = suppressWarnings(as.numeric(PlusMinus)),
           GD         = suppressWarnings(as.numeric(GD)),
           Opp        = suppressWarnings(as.numeric(Opp)),
           Date       = as.character(Date),
           Opponent   = str_trim(Opponent)) %>%
    filter(!is.na(PlusMinus)) %>%
    select(Team, team_id, Date, Opponent, `W/L/T`, GD, Opp, PlusMinus)
}

calculateWP <- function(team_name, schedule) {
  schedule %>%
    filter(Team == team_name,
           !is.na(OwnScore)) %>%
    summarize(WP = mean(OwnScore > OpponentScore)) %>%
    pull(WP)
}

calculateIndividualOWP <- function(team_name, opponent, schedule){
  games <- schedule %>%
    filter(Team == opponent, Opponent != team_name, !is.na(OwnScore))
  WP <- if (nrow(games) == 0) NA_real_ else mean(games$OwnScore > games$OpponentScore)
  data.frame(Opponent = opponent, WP = WP)
}

calculateOWP <- function(team_name, schedule){
  opponents_df <- schedule %>%
    mutate(Opponent = ifelse(Opponent == "SteamboSprings",
                        "Steamboat Springs", Opponent)) %>%
    filter(Team == team_name) %>%
    rename(team_name = Team, opponent = Opponent) %>%
    select(team_name, opponent)

  if (nrow(opponents_df) == 0) return(0.5)

  owp_values <- opponents_df %>%
    pmap_dfr(calculateIndividualOWP, schedule = schedule)

  result <- mean(owp_values$WP, na.rm = TRUE)
  if (is.nan(result)) 0.5 else result
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
    pmap_dbl(calculateOWP, schedule) %>%
    {data.frame(Team = opponents, OWP = .)} %>%
    left_join(team_info %>% select("Team Name", UtahNeighbor),
              by = c("Team" = "Team Name")) %>%
    mutate(OWP = ifelse(!is.na(UtahNeighbor) & UtahNeighbor, OWP, 0.5))

  OOWP <- mean(opp_WP$OWP, na.rm = TRUE)

  output <- data.frame(WP = WP,
                       OWP = OWP,
                       OOWP = OOWP,
                       RPI = 0.45 * WP + 0.45 * OWP + 0.1 * OOWP)
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
                                 sheet_in = "Remaining Games",
                                 sheet_out = "RPI_scenarios"){
  scenarios <- SHEET_URL %>%
    read_sheet(sheet = sheet_in) %>%
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

  getRanksForSchedule(scenario_schedule, teams, team_info) %>%
    sheet_write(SHEET_URL, sheet = sheet_out)
}

buildScenarioSchedule <- function(games_df, picks, base_schedule) {
  scenario <- games_df %>% mutate(Pick = picks)
  first_half <- scenario %>%
    transmute(Date, Team = Team1, Opponent = Team2, Home = TRUE,
              OwnScore = as.numeric(Pick == 1), OpponentScore = as.numeric(Pick == 2))
  second_half <- scenario %>%
    transmute(Date, Team = Team2, Opponent = Team1, Home = FALSE,
              OwnScore = as.numeric(Pick == 2), OpponentScore = as.numeric(Pick == 1))
  base_schedule %>% bind_rows(first_half) %>% bind_rows(second_half)
}

getRanksForSchedule <- function(schedule, teams, team_info) {
  completed <- schedule %>%
    filter(!is.na(OwnScore)) %>%
    mutate(Opponent = ifelse(Opponent == "SteamboSprings", "Steamboat Springs", Opponent))

  # WP: win rate per team
  team_totals <- completed %>%
    group_by(Team) %>%
    summarise(total_wins  = sum(OwnScore > OpponentScore),
              total_games = n(), .groups = "drop")

  # Adjusted WP: each team's win rate excluding games against a specific opponent
  vs_stats <- completed %>%
    group_by(Team, Opponent) %>%
    summarise(wins_vs  = sum(OwnScore > OpponentScore),
              games_vs = n(), .groups = "drop")

  adj_wp <- vs_stats %>%
    left_join(team_totals, by = "Team") %>%
    mutate(adj_games = total_games - games_vs,
           adj_wp    = ifelse(adj_games == 0, NA_real_,
                              (total_wins - wins_vs) / adj_games)) %>%
    select(Team, Opponent, adj_wp)

  # OWP: for each (A, B) opponent pair, get B's adj_wp excluding A, then average
  team_opponents <- completed %>% select(Team, Opponent)

  owp_all <- team_opponents %>%
    left_join(adj_wp, by = c("Team" = "Opponent", "Opponent" = "Team")) %>%
    group_by(Team) %>%
    summarise(OWP = mean(adj_wp, na.rm = TRUE), .groups = "drop") %>%
    mutate(OWP = ifelse(is.nan(OWP), 0.5, OWP))

  # OOWP: average opponents' OWP, capping non-neighbors at 0.5
  oowp_all <- team_opponents %>%
    left_join(owp_all, by = c("Opponent" = "Team")) %>%
    left_join(team_info %>% select("Team Name", UtahNeighbor),
              by = c("Opponent" = "Team Name")) %>%
    mutate(OWP = ifelse(!is.na(UtahNeighbor) & UtahNeighbor, OWP, 0.5),
           OWP = ifelse(is.na(OWP) | is.nan(OWP), 0.5, OWP)) %>%
    group_by(Team) %>%
    summarise(OOWP = mean(OWP, na.rm = TRUE), .groups = "drop")

  data.frame(Team = teams) %>%
    left_join(team_totals %>% mutate(WP = total_wins / total_games) %>%
                select(Team, WP), by = "Team") %>%
    left_join(owp_all,  by = "Team") %>%
    left_join(oowp_all, by = "Team") %>%
    mutate(WP   = replace_na(WP,   0),
           OWP  = replace_na(OWP,  0.5),
           OOWP = replace_na(OOWP, 0.5),
           RPI  = 0.45 * WP + 0.45 * OWP + 0.1 * OOWP) %>%
    left_join(team_info %>% select("Team Name", "Classification"),
              by = c("Team" = "Team Name")) %>%
    arrange(desc(Classification), desc(RPI)) %>%
    group_by(Classification) %>%
    mutate(Seed = row_number()) %>%
    ungroup() %>%
    select(Classification, Seed, Team, RPI, WP, OWP, OOWP)
}

win_prob <- function(rating_diff, scale = 5) {
  1 / (1 + exp(-rating_diff / scale))
}

plotWinProb <- function(scales = c(1.5, 3, 5, 7), diff_range = c(-15, 15)) {
  curve_df <- expand.grid(
      diff  = seq(diff_range[1], diff_range[2], by = 0.1),
      scale = scales
    ) %>%
    mutate(prob  = win_prob(diff, scale = scale),
           scale = factor(paste0("scale = ", scale),
                          levels = paste0("scale = ", scales)))

  ggplot2::ggplot(curve_df, ggplot2::aes(diff, prob, color = scale)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = c(0.25, 0.5, 0.75),
                        lty = 2, color = "gray60") +
    ggplot2::geom_vline(xintercept = 0, lty = 2, color = "gray60") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = seq(0, 1, 0.1)) +
    ggplot2::labs(x = "Rating Difference (Team1 − Team2)",
                  y = "P(Team1 wins)",
                  title = "Win Probability by prob_scale",
                  color = NULL) +
    ggplot2::theme_minimal()
}

simulateSeeds <- function(completed_schedule, teams, team_info,
                          sheet_in        = "Remaining Games",
                          sheet_out       = "Seed Simulation",
                          n_sims          = 1000,
                          prob_scale      = 7,
                          certain_cutoff  = 10,
                          ratings         = getLaxNumsRatings()) {
  games <- read_sheet(SHEET_URL, sheet = sheet_in) %>%
    mutate(Date = as.character(as.Date(Date)))

  games_rated <- games %>%
    left_join(ratings, by = c("Team1" = "Team Name")) %>% rename(Rating1 = Rating) %>%
    left_join(ratings, by = c("Team2" = "Team Name")) %>% rename(Rating2 = Rating) %>%
    mutate(
      diff       = abs(coalesce(Rating1, 0) - coalesce(Rating2, 0)),
      fixed_pick = ifelse(coalesce(Rating1, 0) >= coalesce(Rating2, 0), 1L, 2L)
    )

  certain_games   <- games_rated %>% filter(diff >= certain_cutoff)
  uncertain_games <- games_rated %>% filter(diff < certain_cutoff)
  n_uncertain     <- nrow(uncertain_games)
  cat(sprintf("%d uncertain games, running %d Monte Carlo simulations\n",
              n_uncertain, n_sims))

  # Win probability for Team1 in each uncertain game
  p1 <- win_prob(
    coalesce(uncertain_games$Rating1, 0) - coalesce(uncertain_games$Rating2, 0),
    scale = prob_scale
  )

  # Bake in certain outcomes once
  base_schedule <- if (nrow(certain_games) > 0)
    buildScenarioSchedule(certain_games, certain_games$fixed_pick, completed_schedule)
  else
    completed_schedule

  # Only rank 5A/6A teams — no need to calculate 4A each sim
  teams_sim <- team_info %>%
    filter(Classification %in% c("5A", "6A")) %>%
    pull("Team Name") %>%
    intersect(teams)

  # Run sims in sequential batches (each batch parallelised across workers),
  # printing a progress bar with elapsed time and ETA after each batch.
  n_workers   <- max(1L, future::nbrOfWorkers())
  n_batches   <- 20L
  batch_sims  <- ceiling(n_sims / n_batches)
  t_start     <- proc.time()[["elapsed"]]

  all_ranks <- map_dfr(seq_len(n_batches), function(b) {
    actual <- min(batch_sims, n_sims - (b - 1L) * batch_sims)
    if (actual <= 0L) return(NULL)

    result <- seq_len(actual) %>%
      future_map_dfr(function(i) {
        picks    <- ifelse(runif(n_uncertain) < p1, 1L, 2L)
        schedule <- buildScenarioSchedule(uncertain_games, picks, base_schedule)
        getRanksForSchedule(schedule, teams_sim, team_info)
      }, .options = furrr_options(seed = TRUE))

    elapsed <- proc.time()[["elapsed"]] - t_start
    pct     <- b / n_batches
    eta     <- elapsed / pct * (1 - pct)
    bar     <- paste0(strrep("=", floor(30 * pct)),
                      strrep(" ", 30L - floor(30 * pct)))
    cat(sprintf("\r[%s] %3.0f%%  elapsed: %.0fs  ETA: %.0fs   ",
                bar, 100 * pct, elapsed, eta))
    flush.console()
    result
  })
  cat("\n")

  expected_seeds <- all_ranks %>%
    group_by(Classification, Team) %>%
    summarise(Expected_Seed = mean(Seed), .groups = "drop")

  summary_wide <- all_ranks %>%
    count(Classification, Team, Seed) %>%
    mutate(Prob = round(n / n_sims, 3)) %>%
    select(-n) %>%
    pivot_wider(names_from = Seed, names_prefix = "Seed_",
                values_from = Prob, values_fill = NA_real_) %>%
    select(Classification, Team, any_of(paste0("Seed_", 1:24))) %>%
    left_join(expected_seeds, by = c("Classification", "Team")) %>%
    arrange(Classification, Expected_Seed) %>%
    select(-Expected_Seed)

  sheet_write(summary_wide, SHEET_URL, sheet = sheet_out)

  # Format: percent display + white-to-green gradient per cell value
  ss       <- gs4_get(SHEET_URL)
  sheet_id <- ss$sheets$id[ss$sheets$name == sheet_out]

  seed_cols <- which(startsWith(names(summary_wide), "Seed_"))
  n_rows    <- nrow(summary_wide)

  requests <- unlist(lapply(seq_len(n_rows), function(i) {
    lapply(seed_cols, function(j) {
      v <- summary_wide[[j]][i]
      if (is.na(v)) v <- 0
      color <- list(red   = 1 - 0.80 * v,
                    green = 1 - 0.27 * v,
                    blue  = 1 - 0.61 * v)
      list(repeatCell = list(
        range  = list(sheetId          = sheet_id,
                      startRowIndex    = i,
                      endRowIndex      = i + 1L,
                      startColumnIndex = j - 1L,
                      endColumnIndex   = j),
        cell   = list(userEnteredFormat = list(
                      backgroundColor = color,
                      numberFormat    = list(type = "PERCENT", pattern = "0.0%"))),
        fields = "userEnteredFormat.backgroundColor,userEnteredFormat.numberFormat"
      ))
    })
  }), recursive = FALSE)

  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(spreadsheetId = ss$spreadsheet_id, requests = requests)
  )
  request_make(req)

  invisible(summary_wide)
}

getTeamList <- function(view_ids = BOYS_CLASSIFICATION_VIEWS, year = NULL){
  if (is.null(year)) year <- lubridate::year(today())
  getUHSAAClassifications(view_ids, year) %>%
    pull("Team Name")
}

buildOutOfStateTeamInfo <- function(missing_ids, year = NULL){
  if (is.null(year)) year <- lubridate::year(today())
  paste0(LAXNUMS_BASE, "?y=", year, "&t=", missing_ids) %>%
    future_map_dfr(buildOutOfStateRow)
}

buildOutOfStateRow <- function(url){
  page <- readHtmlWithRetry(url)

  state <- page %>%
    html_nodes(xpath = "//table//a") %>%
    as.character() %>%
    as.data.frame() %>%
    slice(3) %>%
    str_replace('.* - ', "") %>%
    str_replace('</a>', "")

  team_name <- page %>%
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

getCompleteGames <- function(year = NULL,
                             classification_views = BOYS_CLASSIFICATION_VIEWS){
  if (is.null(year)) year <- lubridate::year(today())

  utah_team_info <- getUHSAAClassifications(classification_views, year) %>%
    mutate(UtahNeighbor = TRUE)

  utah_opponent_ids <- utah_team_info %>%
    mutate(url = paste0(LAXNUMS_BASE, "?y=", year, "&t=", `LaxNums ID`)) %>%
    pull(url) %>%
    future_map_dfr(getOpponentIDs) %>%
    pull(team_id) %>%
    unique()

  missing_ids <- utah_opponent_ids %>% setdiff(utah_team_info$`LaxNums ID`)
  missing_team_info <- buildOutOfStateTeamInfo(missing_ids, year)

  out_of_state_ids <- missing_team_info %>%
    filter(UtahNeighbor) %>%
    pull(`Schedule URL`) %>%
    future_map_dfr(getOpponentIDs) %>%
    pull(team_id) %>%
    unique()

  all_ids <- c(utah_opponent_ids, out_of_state_ids) %>% unique()

  team_info <- utah_team_info %>%
    bind_rows(missing_team_info)

  full_schedule <- all_ids %>%
    future_map_dfr(getTeamSchedule, year = year) %>%
    bind_rows(MANUAL_REMAINING_GAMES_2026)

  completed_schedule <- full_schedule %>%
    filter(!is.na(OwnScore)) %>%
    anti_join(EXCLUDED_GAMES_2026, by = c("Team", "Opponent")) %>%
    anti_join(EXCLUDED_GAMES_2026, by = c("Team" = "Opponent", "Opponent" = "Team")) %>%
    bind_rows(MANUAL_GAMES_2026)

  list(schedule = completed_schedule, full_schedule = full_schedule, team_info = team_info)
}

getLaxNumsRatings <- function(view_ids = BOYS_CLASSIFICATION_VIEWS, year = NULL) {
  if (is.null(year)) year <- lubridate::year(today())
  map2_dfr(names(view_ids), unname(view_ids), function(class_name, view_id) {
    url <- str_glue("https://www.laxnumbers.com/ratings/service?y={year}&v={view_id}")
    data <- jsonlite::fromJSON(url)
    data.frame("Team Name" = data$name, Rating = data$rating,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
}

writeRemainingGames <- function(full_schedule, teams, sheet_name = "Remaining Games",
                               ratings = getLaxNumsRatings()) {

  games <- full_schedule %>%
    filter(is.na(OwnScore), Team %in% teams, Home) %>%
    transmute(Team1 = Team, Team2 = Opponent, Date = Date) %>%
    arrange(Date, Team1) %>%
    left_join(ratings, by = c("Team1" = "Team Name")) %>%
    rename(Rating1 = Rating) %>%
    left_join(ratings, by = c("Team2" = "Team Name")) %>%
    rename(Rating2 = Rating) %>%
    mutate(
      diff = abs(Rating1 - Rating2),
      Pick = case_when(
        Rating1 >= Rating2 ~ 1L,
        TRUE               ~ 2L
      )
    )

  games %>%
    select(Team1, Team2, Date, Pick) %>%
    sheet_write(SHEET_URL, sheet = sheet_name)

  # Apply background colors via a single batchUpdate API call
  ss        <- gs4_get(SHEET_URL)
  sheet_id  <- ss$sheets$id[ss$sheets$name == sheet_name]

  mk_color <- function(r, g, b) list(red = r, green = g, blue = b)
  color_for_diff <- function(d) {
    if (is.na(d))    mk_color(0.85, 0.85, 0.85)  # gray  – ratings unknown
    else if (d > 5)  mk_color(0.20, 0.73, 0.39)  # green
    else if (d >= 2) mk_color(1.00, 0.92, 0.23)  # yellow
    else             mk_color(0.95, 0.28, 0.28)  # red
  }

  requests <- lapply(seq_len(nrow(games)), function(i) {
    list(repeatCell = list(
      range  = list(sheetId = sheet_id,
                    startRowIndex = i, endRowIndex = i + 1L,
                    startColumnIndex = 0L, endColumnIndex = 4L),
      cell   = list(userEnteredFormat = list(
                    backgroundColor = color_for_diff(games$diff[i]))),
      fields = "userEnteredFormat.backgroundColor"
    ))
  })

  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(spreadsheetId = ss$spreadsheet_id, requests = requests)
  )
  request_make(req)

  invisible(NULL)
}

updateRPISheet <- function(completed_schedule, teams, team_info, sheet_name = "RPI"){
  getRanksForSchedule(completed_schedule, teams, team_info) %>%
    sheet_write(SHEET_URL, sheet = sheet_name)
  invisible(NULL)
}
