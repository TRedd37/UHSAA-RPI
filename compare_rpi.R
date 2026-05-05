library(rvest); library(dplyr); library(purrr); library(furrr); library(stringr)
library(lubridate); library(jsonlite); library(googlesheets4)
plan(multisession, workers = 2)
source("functions.R")

getUHSAARPI <- function(year = NULL) {
  if (is.null(year)) year <- lubridate::year(today())
  map_dfr(c("5A", "6A"), function(cls) {
    url  <- sprintf("https://uhsaa.org/rpi/boyslacrosse/%d/%s.php", year, cls)
    tbl  <- read_html(url) %>% html_table() %>% .[[1]]
    tbl %>%
      rename(UHSAA_Rank = Rank, UHSAA_RPI = RPI, UHSAA_WP = WP,
             UHSAA_OWP = OWP, UHSAA_OOWP = OOWP, Record = `W-L-T`) %>%
      mutate(Classification = cls)
  })
}

cat("Loading schedules...\n")
output_list        <- getCompleteGames()
completed_schedule <- output_list[["schedule"]]
team_info          <- output_list[["team_info"]]
teams              <- getTeamList()

teams_56 <- team_info %>%
  filter(Classification %in% c("5A", "6A")) %>%
  pull("Team Name") %>%
  intersect(teams)

completed <- completed_schedule %>%
  filter(!is.na(OwnScore)) %>%
  mutate(Opponent = ifelse(Opponent == "SteamboSprings", "Steamboat Springs", Opponent))

team_totals <- completed %>%
  group_by(Team) %>%
  summarise(total_wins = sum(OwnScore > OpponentScore), total_games = n(), .groups = "drop")

vs_stats <- completed %>%
  group_by(Team, Opponent) %>%
  summarise(wins_vs = sum(OwnScore > OpponentScore), games_vs = n(), .groups = "drop")

adj_wp <- vs_stats %>%
  left_join(team_totals, by = "Team") %>%
  mutate(adj_games = total_games - games_vs,
         adj_wp    = ifelse(adj_games == 0, NA_real_, (total_wins - wins_vs) / adj_games)) %>%
  select(Team, Opponent, adj_wp)

team_opponents <- completed %>% select(Team, Opponent)

owp_all <- team_opponents %>%
  left_join(adj_wp, by = c("Team" = "Opponent", "Opponent" = "Team")) %>%
  group_by(Team) %>%
  summarise(OWP = mean(adj_wp, na.rm = TRUE), .groups = "drop") %>%
  mutate(OWP = ifelse(is.nan(OWP), 0.5, OWP))

oowp_all <- team_opponents %>%
  left_join(owp_all, by = c("Opponent" = "Team")) %>%
  left_join(team_info %>% select("Team Name", UtahNeighbor), by = c("Opponent" = "Team Name")) %>%
  mutate(OWP = ifelse(!is.na(UtahNeighbor) & UtahNeighbor, OWP, 0.5),
         OWP = ifelse(is.na(OWP) | is.nan(OWP), 0.5, OWP)) %>%
  group_by(Team) %>%
  summarise(OOWP = mean(OWP, na.rm = TRUE), .groups = "drop")

our_rpi <- data.frame(Team = teams_56) %>%
  left_join(team_totals %>% mutate(WP = total_wins / total_games) %>% select(Team, WP), by = "Team") %>%
  left_join(owp_all,  by = "Team") %>%
  left_join(oowp_all, by = "Team") %>%
  mutate(WP   = replace_na(WP,   0),
         OWP  = replace_na(OWP,  0.5),
         OOWP = replace_na(OOWP, 0.5),
         RPI  = 0.45 * WP + 0.45 * OWP + 0.1 * OOWP) %>%
  left_join(team_info %>% select("Team Name", "Classification"), by = c("Team" = "Team Name")) %>%
  arrange(desc(Classification), desc(RPI)) %>%
  group_by(Classification) %>%
  mutate(Our_Rank = row_number()) %>%
  ungroup()

cat("Fetching UHSAA RPI...\n")
uhsaa_rpi <- getUHSAARPI()

comparison <- uhsaa_rpi %>%
  left_join(our_rpi, by = c("School" = "Team", "Classification")) %>%
  mutate(
    Rank_Diff = Our_Rank - UHSAA_Rank,
    RPI_Diff  = round(RPI - UHSAA_RPI, 4),
    WP_Diff   = round(WP  - UHSAA_WP,  4),
    OWP_Diff  = round(OWP - UHSAA_OWP, 4),
    OOWP_Diff = round(OOWP - UHSAA_OOWP, 4)
  ) %>%
  arrange(Classification, UHSAA_Rank) %>%
  select(Classification, UHSAA_Rank, Our_Rank, Rank_Diff,
         School, Record,
         UHSAA_RPI, RPI, RPI_Diff,
         UHSAA_WP, WP, WP_Diff,
         UHSAA_OWP, OWP, OWP_Diff,
         UHSAA_OOWP, OOWP, OOWP_Diff)

print(comparison, n = 100)

unmatched <- uhsaa_rpi %>%
  anti_join(our_rpi, by = c("School" = "Team", "Classification"))
if (nrow(unmatched) > 0) {
  cat("\nUHSAA teams not matched in our results:\n")
  print(unmatched %>% select(Classification, UHSAA_Rank, School))
}

