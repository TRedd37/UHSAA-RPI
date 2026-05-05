library(dplyr); library(purrr); library(furrr); library(stringr)
library(rvest); library(ggplot2); library(jsonlite); library(lubridate)
source("functions.R")

# Pull every team_info ID we know about (Utah teams + opponents)
output_list <- if (exists("output_list")) output_list else getCompleteGames()
team_info   <- output_list[["team_info"]]

team_ids <- team_info %>%
  filter(!is.na(`LaxNums ID`)) %>%
  pull(`LaxNums ID`) %>%
  unique()

cat(sprintf("Scraping rating math for %d teams...\n", length(team_ids)))

rating_math <- team_ids %>%
  future_map_dfr(getTeamRatingMath)

cat(sprintf("Got %d game-rows total (will dedupe to unique games)\n",
            nrow(rating_math)))

# Each game appears twice (once per team) with opposite-sign +/-.
# Dedupe by sorting (Team, Opponent) alphabetically per game and keeping one row.
unique_games <- rating_math %>%
  mutate(pair_key = pmap_chr(list(Team, Opponent, Date),
                             ~ paste(sort(c(..1, ..2)), ..3, collapse = "|"))) %>%
  distinct(pair_key, .keep_all = TRUE)

cat(sprintf("Unique games: %d\n", nrow(unique_games)))

# --- Distribution of +/- ---
sigma <- sd(unique_games$PlusMinus, na.rm = TRUE)
cat(sprintf("\n+/- distribution:\n  mean = %.3f\n  sd   = %.3f\n",
            mean(unique_games$PlusMinus, na.rm = TRUE), sigma))

# Logistic scale equivalent of a normal residual with std dev sigma:
#   scale_logistic = sigma * sqrt(3) / pi
scale_normal_approx <- sigma * sqrt(3) / pi
cat(sprintf("  Implied prob_scale (normal -> logistic) ≈ %.2f\n",
            scale_normal_approx))

# --- Direct logistic regression: P(home/Team1 win) ~ rating_diff ---
# The +/- is the residual: actual_GD - predicted_GD, where predicted_GD = TeamRating - OppRating.
# So predicted_GD = GD - PlusMinus  (since +/- is added to expected to get observed)
# Actually: PlusMinus = (game-points-earned) - (pre-game rating)
#                     = (GD + Opp) - pre_rating
# Pre-game rating diff = (pre_rating - Opp). We can recover this:
#   rating_diff = GD - PlusMinus
# (This is the team's pre-game rating minus opponent's rating.)
fit_data <- unique_games %>%
  mutate(rating_diff = GD - PlusMinus,
         won         = `W/L/T` == "W") %>%
  filter(!is.na(rating_diff), `W/L/T` %in% c("W", "L"))

cat(sprintf("\nFitting logistic on %d W/L games...\n", nrow(fit_data)))
fit <- glm(won ~ rating_diff, data = fit_data, family = binomial)
print(summary(fit)$coefficients)

beta <- coef(fit)[["rating_diff"]]
fitted_scale <- 1 / beta
cat(sprintf("\nFitted prob_scale = 1 / beta = %.2f\n", fitted_scale))

# --- Plots ---
p_hist <- ggplot(unique_games, aes(PlusMinus)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, lty = 2) +
  labs(title = sprintf("Distribution of +/- (sd = %.2f)", sigma),
       x = "+/-", y = "Games") +
  theme_minimal()

# Logistic curve at the fitted scale, overlaid on binned win-rates
binned <- fit_data %>%
  mutate(bucket = cut(rating_diff, breaks = seq(-25, 25, by = 2))) %>%
  group_by(bucket) %>%
  summarise(rating_diff = mean(rating_diff),
            win_rate    = mean(won),
            n           = n(), .groups = "drop") %>%
  filter(n >= 5)

curve_df <- data.frame(rating_diff = seq(-25, 25, by = 0.1)) %>%
  mutate(prob_fit = win_prob(rating_diff, scale = fitted_scale),
         prob_5   = win_prob(rating_diff, scale = 5),
         prob_7   = win_prob(rating_diff, scale = 7))

p_fit <- ggplot() +
  geom_point(data = binned,
             aes(rating_diff, win_rate, size = n),
             color = "black", alpha = 0.7) +
  geom_line(data = curve_df,
            aes(rating_diff, prob_fit, color = sprintf("fit (%.1f)", fitted_scale)),
            lwd = 1.2) +
  geom_line(data = curve_df, aes(rating_diff, prob_5, color = "scale = 5"),
            lty = 2) +
  geom_line(data = curve_df, aes(rating_diff, prob_7, color = "scale = 7"),
            lty = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Empirical win rate vs logistic curves",
       x = "Pre-game rating diff (Team − Opp)",
       y = "P(Team wins)", color = NULL, size = "games") +
  theme_minimal()

print(p_hist)
print(p_fit)

cat(sprintf("\n==> Recommended prob_scale: %.1f\n", fitted_scale))
