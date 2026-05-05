source("functions.R")

# Assumes completed_schedule, teams, team_info are already loaded

teams_56 <- team_info %>%
  filter(Classification %in% c("5A", "6A")) %>%
  pull("Team Name") %>%
  intersect(teams)

TOLERANCE <- 1e-6

cat("Computing old RPI (pmap)...\n")
old_rpi <- data.frame(team_name = teams_56) %>%
  pmap_dfr(calculateRPI, schedule = completed_schedule, team_info = team_info) %>%
  mutate(Team = teams_56)

cat("Computing new RPI (vectorized)...\n")
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

new_values <- data.frame(Team = teams_56) %>%
  left_join(team_totals %>% mutate(WP = total_wins / total_games) %>% select(Team, WP), by = "Team") %>%
  left_join(owp_all,  by = "Team") %>%
  left_join(oowp_all, by = "Team") %>%
  mutate(WP   = replace_na(WP,   0),
         OWP  = replace_na(OWP,  0.5),
         OOWP = replace_na(OOWP, 0.5),
         RPI  = 0.45 * WP + 0.45 * OWP + 0.1 * OOWP)

# Compare all four values
comparison <- old_rpi %>%
  select(Team, WP_old = WP, OWP_old = OWP, OOWP_old = OOWP, RPI_old = RPI) %>%
  left_join(new_values %>% select(Team, WP_new = WP, OWP_new = OWP,
                                   OOWP_new = OOWP, RPI_new = RPI), by = "Team") %>%
  mutate(
    WP_diff   = abs(WP_old   - WP_new),
    OWP_diff  = abs(OWP_old  - OWP_new),
    OOWP_diff = abs(OOWP_old - OOWP_new),
    RPI_diff  = abs(RPI_old  - RPI_new),
    any_fail  = WP_diff > TOLERANCE | OWP_diff > TOLERANCE |
                OOWP_diff > TOLERANCE | RPI_diff > TOLERANCE
  )

failures <- comparison %>% filter(any_fail)

cat(sprintf("\nTolerance: %g\n", TOLERANCE))
cat(sprintf("Teams checked: %d\n", nrow(comparison)))
cat(sprintf("Teams with differences exceeding tolerance: %d\n\n", nrow(failures)))

if (nrow(failures) > 0) {
  cat("=== FAILURES ===\n")
  print(failures %>% select(Team, WP_diff, OWP_diff, OOWP_diff, RPI_diff))
} else {
  cat("All WP, OWP, OOWP, and RPI values match within tolerance. ✓\n")
}
