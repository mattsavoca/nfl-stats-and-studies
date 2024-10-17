options(scipen=999)
seasons_raw = nflreadr::load_schedules(seasons = T)

seasons = seasons_raw %>% 
  ungroup() %>%
  mutate(
    ud_spread_odds = case_when(
      spread_line > 0 ~ away_spread_odds,
      spread_line < 0 ~ home_spread_odds,
      T ~ NA_real_
    ),
    ud_ml_odds = case_when(
      spread_line > 0 ~ away_moneyline,
      spread_line < 0 ~ home_moneyline,
      T ~ NA_real_
    ),
    ud_spread_prob = odds.converter::odds.us2prob(ud_spread_odds),
    ud_ml_prob = odds.converter::odds.us2prob(ud_ml_odds),
    ud_spread_dec = odds.converter::odds.us2dec(ud_spread_odds),
    ud_ml_dec = odds.converter::odds.us2dec(ud_ml_odds),
    ud_spread_xroi = (ud_spread_dec*ud_spread_prob)-1,
    ud_ml_xroi = (ud_ml_dec*ud_ml_prob)-1,
    ud_beat_spread = case_when(
      spread_line > 0 & result < spread_line ~ 1,
      spread_line <0 & result > spread_line ~ 1,
      T ~ 0
    ),
    ud_beat_ml = case_when(
      spread_line > 0 & result < 0 ~ 1,
      spread_line < 0 & result > 0 ~ 1,
      T ~ 0
    ),
    ud_spread_roi = ud_spread_dec*ud_beat_spread/1-1,
    ud_ml_roi = ud_ml_dec*ud_beat_ml/1-1,
    ud_ml_win_pct = mean(ud_beat_ml, na.rm = T),
    ud_spread_win_pct = mean(ud_beat_spread, na.rm = T),
    ud_spread_roi_vs_exp = ud_spread_roi - ud_spread_xroi,
    ud_ml_roi_vs_exp = ud_ml_roi - ud_ml_xroi,
    ud_ml_roi_avg = mean(ud_ml_roi, na.rm = T),
    ud_spread_roi_avg = mean(ud_spread_roi, na.rm = T)
  )


seasons_filtered = seasons %>%
  filter(!is.na(spread_line), game_type == "REG", season >= 2011)
  

logit_model =  glm(formula = ud_beat_spread ~ week, family = "binomial", data = seasons_filtered)

summary(logit_model)


seasons_test = seasons_filtered %>% select(season, week, spread_line, result, actual = ud_beat_spread)
logit_model %>% augment(seasons_test) %>% view

seasons %>%
  filter(!is.na(spread_line), game_type == "REG", season >= 2011) %>%
  group_by(season, week) %>%
  summarize(
    upset_pct = sum(ud_beat_spread)/n(),
    total_sample_ud_win_pct = mean(ud_win_pct),
  ) %>%
  ggplot()+
  aes(upset_pct)+
  geom_density(aes(y = after_stat(density)))+
  geom_boxplot()+
  scale_x_continuous(labels = percent)+
  theme_minimal()+
  labs(
    x = "Upset Pct.",
    y = ""
  )



seasons_filtered %>%
  group_by(
    week
  ) %>% 
  summarize(
    ud_wins = sum(ud_beat_spread)/n_distinct(season),
    ovr_ud_win_pct = mean(ud_spread_win_pct)*n()/n_distinct(season),
    total_sample_ud_win_pct = mean(ud_spread_win_pct),
    ud_win_pct_vs_avg = ud_wins - ovr_ud_win_pct,
    .groups = "drop"
  ) %>% 
  ggplot()+
  aes(week, ud_win_pct_vs_avg)+
  geom_col()+
  scale_y_continuous(breaks = seq(-1, 1, .2))+
  scale_x_continuous(breaks = seq(1, 18, 1))+
  labs(
    y = "Upset Pct. Over Average (vs. the Spread)",
    x = "Week of Season",
    title = "Early Season = More Uncertainty",
    subtitle = "Red Line: Underdog wins 50% or more of all contests",
    caption = "Data: @nflreadR"
  )+
  theme_minimal()


seasons_filtered %>%
  group_by(
    week
  ) %>% 
  summarize(
    ud_wins = sum(ud_beat_ml)/n_distinct(season),
    ovr_ud_win_pct = mean(ud_ml_win_pct)*n()/n_distinct(season),
    total_sample_ud_win_pct = mean(ud_ml_win_pct),
    ud_win_pct_vs_avg = ud_wins - ovr_ud_win_pct,
    .groups = "drop"
  ) %>% 
  ggplot()+
  aes(week, ud_win_pct_vs_avg)+
  geom_col()+
  scale_y_continuous(breaks = seq(-1, 1, .2))+
  scale_x_continuous(breaks = seq(1, 18, 1))+
  labs(
    y = "Upset Pct. Over Average (Moneyline)",
    x = "Week of Season",
    title = "Early Season = More Uncertainty",
    subtitle = "Red Line: Underdog wins 50% or more of all contests",
    caption = "Data: @nflreadR"
  )+
  theme_minimal()


seasons_filtered %>%
  group_by(
    week
  ) %>%
  summarize(
    ud_spread_roi = mean(ud_spread_roi, na.rm = T),
    ud_spread_roi_vs_exp = mean(ud_spread_roi_vs_exp, na.rm = T),
    .groups = "drop"
  ) %>%
  ggplot()+
  aes(x = week, y = ud_spread_roi)+
  theme_minimal()+
  geom_col()+
  scale_x_continuous(breaks = seq(1, 18, 1))+
  scale_y_continuous(labels = percent, breaks = seq(-.16, .16, .02))+
  labs(
    x = "Week",
    y = "Underdog ROI vs. Expected (vs. Spread)",
    title = "ROI on Underdog Bets ATS are highest in Early Weeks, Wk 6 & Wk 10"
  )



seasons_filtered %>%
  group_by(
    week
  ) %>%
  summarize(
    ud_ml_roi_vs_exp = mean(ud_ml_roi_vs_exp, na.rm = T),
    .groups = "drop"
  ) %>%
  ggplot()+
  theme_minimal()+
  aes(x = week, y = ud_ml_roi_vs_exp)+
  geom_col()+
  scale_x_continuous(breaks = seq(1, 18, 1))+
  scale_y_continuous(labels = percent, breaks = seq(-.50, .50, .05))+
  labs(
    x = "Week",
    y = "Underdog ROI vs. Expected (Moneyline)",
    title = "No Weekly Trend in ROI ML Bets for Underdogs"
  )
