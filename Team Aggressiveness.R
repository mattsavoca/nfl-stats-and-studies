n_drives = pbp %>%
  filter(!is.na(posteam) & posteam != "") %>%
  group_by(season, week, posteam) %>%
  summarize(
    drives = n_distinct(fixed_drive)
  )

n_drives %>%
  pull(drives)
  

n_drives %>% ggplot()+
  aes(
    x = drives
  )+
  geom_histogram()+
  geom_vline(
    aes(xintercept = median(drives)), color = "red", lty = 2
  )
  


n_drives %>% 
  filter(drives < 2)


pbp %>%
  select(
    season, week, drive = fixed_drive, play_id ,posteam, defteam, pass_oe, game_seconds_remaining, time_of_day
  ) %>%
  filter(season >= 2011, !is.na(posteam)) %>%
  write_csv("seconds_per_play")

sec_per_play = read_csv("sec_per_play.csv")

sec_per_play %>%
  filter(!is.na(true_seconds_per_play), true_seconds_per_play <= 500) %>%
  group_by(season, posteam) %>%
  summarize(across(contains("_per_play"), ~mean(.x))) %>%
  filter(season == 2023) %>%
  arrange(true_seconds_per_play) %>%
  view
  


adj_rates = pbp %>% 
  filter(season >= 2006, !is.na(posteam)) %>%
  mutate(
    neutral_script = case_when(
      wp < .025 | wp > .975 ~ 0,
      qtr > 4 ~ 0,
      half_seconds_remaining <= 120 ~ 0,
      T ~ 1
    )
  ) %>%
  group_by(game_id, season, week) %>%
  mutate(
    max_qtr = max(qtr),
    last_play_sec_remaining = min(quarter_seconds_remaining[qtr == max_qtr], na.rm  = T),
    extra_sec = case_when(
      max_qtr == 6 ~  15*60 + (15*60 - max(last_play_sec_remaining,na.rm = T)),
      max_qtr == 5 ~ 15*60 - max(last_play_sec_remaining,na.rm = T),
      T ~ 0
    ),
    total_game_time = 3600 + extra_sec
  ) %>% 
  group_by(posteam, season, week, game_id) %>%
  summarize(
    neutral_plays = n_distinct(play_id[neutral_script == 1]),
    neutral_passes = n_distinct(play_id[neutral_script == 1 & qb_dropback == 1]),
    total_game_time = max(total_game_time),
    adj_neutral_play_rate = (3600 * neutral_plays)/total_game_time,
    adj_neutral_pass_rate = ((3600 * neutral_passes)/total_game_time)/adj_neutral_play_rate
  ) %>%
  group_by(posteam, season) %>%
  summarize(
    adj_neutral_play_rate = mean(adj_neutral_play_rate, na.rm = T),
    adj_neutral_pass_rate = mean(adj_neutral_pass_rate, na.rm = T),
    .groups = "drop"
  )


adj_rates %>%
  filter(season == 2023) %>%
  ggplot(
    aes(x = adj_neutral_play_rate, 
        y = adj_neutral_pass_rate, 
        team_abbr = posteam)
  )+
  nflplotR::geom_nfl_logos(width = 0.035)+
  geom_vline(aes(xintercept = mean(adj_neutral_play_rate)), lty = 2)+
  geom_hline(aes(yintercept = mean(adj_neutral_pass_rate)), lty = 2)+
  scale_y_continuous(label = percent)+
  scale_x_continuous(breaks = seq(0, 100, 2))+
  theme_light()+
  labs(
    title = "Team Aggressiveness in Neutral Scripts",
    subtitle = "2023 Season, All Games",
    x = "Plays/60 Min.",
    y = "Pass Rate"
  )
  



pbp %>% 
  filter(season >= 2022, !is.na(posteam)) %>%
  write_csv("pbp_2223.csv")
