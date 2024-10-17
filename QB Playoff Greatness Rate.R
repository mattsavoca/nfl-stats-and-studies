library(nflreadr)
library(glue)
library(ggplot2)
library(scales)
library(nflplotR)
library(tidyverse)
library(ebbr)
library(ggtext)

# Plot Settings
theme_set(theme_minimal()+
            theme(panel.grid.major = element_blank(), 
                  panel.grid =element_blank()))

# Scrape Data
stats = load_player_stats(1999:most_recent_season())


# Gather All QB Games, Regular and Postseason to use for Bayesian Model
true_epa_sr = stats %>% 
  filter(position == "QB", season_type %in% c("POST", "REG"), attempts > 10) %>%
  mutate(
    passing_epa = replace_na(passing_epa, 0),
    rushing_epa = replace_na(rushing_epa, 0),
    epa_play = (passing_epa+rushing_epa)/(attempts+carries),
    epa_success = if_else(epa_play >= 0.36, 1, 0)
  ) %>% 
  arrange(-season, -week) %>%
  select(player_id, player = player_display_name, position, season, week, epa_play, season_type, epa_success, recent_team) %>% 
  group_by(player_id, player, position) %>% 
  summarize(
    min_year = min(season),
    max_year = max(season),
    recent_team = recent_team[1],
    epa_success = sum(epa_success),
    n = n(),
    season_type = "ALL"
  ) %>%
  filter(epa_success <= n) %>%
  ungroup()


# Gather JUST the Playoff Games, but run adjusted rate for all QB games (Reg and Post)
qb_playoff_great_rate = stats %>%  # "GREAT" will be defined as 1 StdDev better than average QB play in playoff
  filter(position == "QB", season_type %in% c("POST"), attempts > 10) %>%
  mutate(
    passing_epa = replace_na(passing_epa, 0),
    rushing_epa = replace_na(rushing_epa, 0),
    epa_play = (passing_epa+rushing_epa)/(attempts+carries),
    epa_success = if_else(epa_play >= 0.36, 1, 0)
  ) %>% 
  arrange(-season, -week) %>%
  select(player_id, player = player_display_name, position, season, week, epa_play, season_type, epa_success, recent_team) %>%
  group_by(player_id, player, position, season_type) %>% 
  summarize(
    recent_team = recent_team[1],
    epa_success = sum(epa_success, na.rm = T),
    n = n()
  ) %>%
  select(player, position, epa_success, n, season_type, recent_team) %>%
  ungroup() %>%
  rbind(
    true_epa_sr %>% select(-contains("year"))
  ) %>%
  left_join(
    true_epa_sr %>% select(player_id, min_year, max_year) # For Chart labels, we'll want all Years Player was in NFL, not just Playoffs Years
  ) %>%
  mutate(epa_success = replace_na(epa_success, 0)) %>%
  filter(epa_success <= n, n > 0, !is.na(n)) %>% # Using Bayesian Estimation to create adjusted rates
  add_ebb_estimate(epa_success, n ,prior_subset = n >= 5) %>%
  filter(season_type == "POST" , n >= 3) %>%
  arrange(-.fitted) %>%
  mutate(player_label = glue("{player} ({min_year}-{max_year})"), # Create player labels for chart
         player_label = ifelse(max_year == 2023, paste0("<b>", player_label, "</b>"), player_label), # Bold active players 
         player_label = reorder(player_label, .fitted))


# Generate Plot
qb_great_rate_plot = qb_playoff_great_rate %>%
  filter(.raw >= .fitted) %>%
  ggplot() + 
  aes(x = .fitted, y = player_label, fill = recent_team) +
  geom_col()+
  geom_nfl_logos(aes(team_abbr = recent_team, y = player_label, alpha = .9), width = 0.025, x = -.015) +
  geom_col(alpha = .3, aes(x = .raw, y = player_label, fill = recent_team))+
  scale_fill_nfl()+
  scale_x_continuous(labels = percent)+
  labs(
    x = "EPA 'Great' Rate (Faint Color: Observed, Solid Color: Sample-Size Adjusted)",
    y = "",
    title = "These QBs Are a Bucket in the Postseasn",
    subtitle = "Adjusted Postseason 'Great' Rate ('Great': One-Plus Stdv Above Avg. EPA)",
    caption = "Chart: @Draftaholic | Data: @nflreadR"
  ) +
  theme(
    axis.text.y = element_markdown()
  )

# View Plot
qb_great_rate_plot

# Save Plot
ggsave("QB Playoff Great Rate.png", plot = qb_great_rate_plot, width = 1080, height = 1080, units = "px")



# Generate Plot
qb_playoff_great_rate %>%
  filter(.raw < .fitted) %>%
  ggplot() + 
  aes(x = .fitted, y = player_label, fill = recent_team) +
  geom_col()+
  geom_nfl_logos(aes(team_abbr = recent_team, y = player_label, alpha = .9), width = 0.025, x = -.015) +
  geom_col(alpha = .3, aes(x = .raw, y = player_label, fill = recent_team))+
  scale_fill_nfl()+
  scale_x_continuous(labels = percent)+
  labs(
    x = "EPA 'Great' Rate (Faint Color: Observed, Solid Color: Sample-Size Adjusted)",
    y = "",
    title = "These QBs Are a Bucket in the Postseasn",
    subtitle = "Adjusted Postseason 'Great' Rate ('Great': One-Plus Stdv Above Avg. EPA)",
    caption = "Chart: @Draftaholic | Data: @nflreadR"
  ) +
  theme(
    axis.text.y = element_markdown()
  )

