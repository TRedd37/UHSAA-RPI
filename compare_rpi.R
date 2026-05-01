library(rvest); library(dplyr); library(purrr); library(furrr); library(stringr)
library(lubridate); library(jsonlite); library(googlesheets4)
plan(multisession)
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

our_rpi <- data.frame(team_name = teams) %>%
  pmap_dfr(calculateRPI, schedule = completed_schedule, team_info = team_info) %>%
  mutate(Team = teams) %>%
  left_join(team_info %>% select("Team Name", "Classification"),
            by = c("Team" = "Team Name")) %>%
  filter(Classification %in% c("5A", "6A")) %>%
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

