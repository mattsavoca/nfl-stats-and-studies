#### initial setup -------
library(easypackages)
packages("tidyverse", "tidymodels", "skimr", "remotes", "dplyr", "gt", "paletteer", "janitor", "googlesheets4")
remotes::install_github("nflverse/nflfastR")
future::plan("multisession")

all_seasons = seq(1999, 2020)
theme_set = theme_light()

###### load data --------

PROJECT_DIR = "~/Dropbox/Matt Savoca/Projects/NFL 2021/"
SCRAPE_PATH = glue::glue("{PROJECT_DIR}/scripts/pff_ep_scraping.R")
THEME_PATH = glue::glue("{PROJECT_DIR}/scripts/ajreinhard_plot_func.R")
RESEARCH_SHEET = "https://docs.google.com/spreadsheets/d/1GtvJI9xjoym9I6BMDx7Pzud7EuO2XlrcFit6sUGd4ng/edit?usp=sharing"
player_stats = nflfastR::load_player_stats()
player_rosters = nflfastR::fast_scraper_roster(seasons = all_seasons)

source(SCRAPE_PATH)

# EP Data
qb_ep_df_raw = read_csv(paste0(PROJECT_DIR,"/data/pff_qb_ep.csv"))
skill_ep_df_raw = read_csv(paste0(PROJECT_DIR, "data/pff_skill_ep.csv"))
qb_ep_df = qb_ep_df_raw %>% clean_names() %>% mutate(position = "QB")
skill_ep_df = skill_ep_df_raw %>% clean_names()

# ADP Data
adp_raw = read_csv(paste0(PROJECT_DIR, "/data/ud_rankings.csv"))
adp_df = adp_raw %>% clean_names()


# ADP features
adp = adp_df %>%
  mutate(player = paste(first_name, last_name),
         ud_proj = round(projected_points/16,2),
         adp = as.numeric(adp),
         adp = replace_na(adp, 300)) %>%
  rename(ud_id = id) %>%
  select(
    -lineup_status,
    -bye_week,
    -first_name,
    -last_name)
  
  

## Helper Functions ------

scale_this <- function(x) as.vector(scale(x))

## FLEX Feature Engineering | NFLFastR --------

ff_feature_engineering = function(df, player_rosters){
  df =  df %>%
    left_join(player_rosters %>% select(season, position, name = full_name, headshot_url, player_id = gsis_id, pff_id)) 
 
  df = df  %>%
    mutate(
      player_name = name,
      passing_epa = replace_na(passing_epa, 0),
      rushing_epa = replace_na(rushing_epa, 0),
      receiving_epa = replace_na(receiving_epa, 0),
      epa = passing_epa + rushing_epa + receiving_epa,
      is_flex = if_else(position %in% c('TE', 'WR', 'RB'),1,0),
      is_rec = if_else(position %in% c('TE', 'WR' ),1,0),
      reg_season = case_when(season < 2021 & week <= 17 ~ 1,
                             season >= 2021 & week <= 18 ~ 1,
                             TRUE ~ 0),
      touches = receptions + carries,
      opportunities = targets + carries,
      passing_adot = if_else(attempts == 0 | is.na(attempts), 0, passing_air_yards/attempts),
      receiving_adot = if_else(attempts == 0 | is.na(targets), 0, receiving_air_yards/targets),
      adot = if_else(position == "QB", passing_adot, receiving_adot)
    ) %>%
    filter(reg_season == 1) %>%
    arrange(-season, week, is_flex, -fantasy_points_ppr) %>%
    group_by(season, week, position) %>%
    mutate(pos_rank = min_rank(-fantasy_points_ppr) %>% as.double()) %>%
    group_by(season, week, is_flex) %>%
    mutate(flex_rank = min_rank(-fantasy_points_ppr) %>% as.double()) %>%
    group_by(season, week, is_rec) %>%
    mutate(rec_rank = min_rank(-fantasy_points_ppr) %>% as.double()) %>%
    ungroup() %>%
    mutate(
      flex_rank = if_else(is_flex < 1, 999, flex_rank),
      rec_rank = if_else(is_rec < 1, 999, rec_rank)
    ) %>%
    group_by(season, player_id) %>%
    transmute(
      dakota = mean(dakota, na.rm = T),
      player_name, recent_team, position,
      season_games = n_distinct(week),
      three_target_rt = n_distinct(week[targets >= 3])/season_games,
      six_target_rt = n_distinct(week[targets >= 6])/season_games,
      seven_target_rt = n_distinct(week[targets >= 7])/season_games,
      eight_target_rt = n_distinct(week[targets >= 8])/season_games,
      nine_target_rt = n_distinct(week[targets >= 9])/season_games,
      ten_target_rt = n_distinct(week[targets >= 10])/season_games,
      ten_opp_rt = n_distinct(week[opportunities >= 10])/season_games,
      twelve_opp_rt = n_distinct(week[opportunities >= 12])/season_games,
      fifteen_opp_rt = n_distinct(week[opportunities >= 15])/season_games,
      eighteen_opp_rt = n_distinct(week[opportunities >= 18])/season_games,
      twenty_opp_rt = n_distinct(week[opportunities >= 20])/season_games,
      fifty_ay_rt = n_distinct(week[receiving_air_yards >= 50])/season_games,
      seventyfive_ay_rt = n_distinct(week[receiving_air_yards >= 75])/season_games,
      ninety_ay_rt = n_distinct(week[receiving_air_yards >= 90])/season_games,
      hundo_ay_rt = n_distinct(week[receiving_air_yards >= 100])/season_games,
      threehundo_passing = n_distinct(week[passing_yards >= 300])/season_games,
      threefifty_passing = n_distinct(week[passing_yards >= 350])/season_games,
      pos_top_12_rt = n_distinct(week[pos_rank <= 12])/season_games,
      pos_top_24_rt = n_distinct(week[pos_rank <= 24])/season_games,
      flex_top_12_rt = n_distinct(week[flex_rank <= 12])/season_games,
      flex_top_24_rt = n_distinct(week[flex_rank <= 24])/season_games,
      flex_top_36_rt = n_distinct(week[flex_rank <= 36])/season_games,
      rec_top_12_rt = n_distinct(week[rec_rank <= 12])/season_games,
      rec_top_24_rt = n_distinct(week[rec_rank <= 24])/season_games,
      rec_top_36_rt = n_distinct(week[rec_rank <= 36])/season_games,
      adot_sd = sd(adot),
      adot = mean(adot),
      fp = sum(fantasy_points_ppr),
      epa = sum(epa),
      fp_gm = fp/season_games,
      trg_gm = sum(targets)/season_games,
      trg_sd = sd(targets, na.rm = T),
      ay_gm = sum(receiving_air_yards)/season_games,
      ay_sd = sd(receiving_air_yards, na.rm =T),
      fp_sd = sd(fantasy_points_ppr, na.rm = T),
      fantasy_points_ppr,
      pff_id
      ) %>%
    rowwise() %>%
    mutate(boom_gm = if_else(fantasy_points_ppr > (fp_gm+fp_sd), 1, 0)) %>%
    group_by(season, player_id) %>%
    mutate(boom_gm = sum(boom_gm)) %>%
    ungroup() %>%
    select(-fantasy_points_ppr)
}



## FLEX Feature Engineering | PFF ------
flex_ep_df_preprocess = function(df, player_rosters) {
  df = df %>% left_join(player_rosters %>% rename(player_id = pff_id, year = season))
  
  df = df %>%
    filter((year <= 2020 & week <= 17) | (year > 2020 & week <= 16)) %>%
    mutate(
      expected_yards = expected_rushing_yards + expected_receiving_yards,
      expected_touchdowns = expected_rushing_tds + expected_receiving_tds,
      touchdown_xfp_share = (expected_touchdowns * 6)/expected_ppg,
      touchdown_fp_share = (6*receiving_tds + 6*rushing_tds)/ppg,
      is_flex = if_else(position %in% c("RB", "WR", "TE"), 1, 0)
    ) %>%
    arrange(-ppg) %>%
    group_by(year, week, team) %>%
    mutate(team_flex_ppg = sum(ppg, na.rm = T),
           team_flex_ep = sum(expected_ppg, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      flex_fp_share = ppg/team_flex_ppg,
      flex_ep_share = expected_ppg/team_flex_ep
    ) %>%
    group_by(year, week, position) %>%
    mutate(pos_ep_posrk = min_rank(-expected_ppg)) %>%
    group_by(year, week, is_flex) %>%
    mutate(flex_ep_posrk = min_rank(-expected_ppg)) %>%
    ungroup()
  
  df2 = df %>%
    group_by(year, week, team) %>%
    top_n(3, ppg) %>%
    summarize(
      top_sum_ppg = sum(ppg, na.rm = T),
      top_sum_ep = sum(expected_ppg, na.rm = T),
    ) %>%
    right_join(df)
  
  
  df = df2 %>%
    mutate(flex_ep_posrk = if_else(is_flex < 1, 999, as.double(flex_ep_posrk))) %>%
    group_by(year, player, player_id) %>%
    transmute(
      player,
      position,
      year,
      team,
      ep = round(mean(expected_ppg, na.rm =T)/.5)*.5, 
      ep_sd = round(sd(expected_ppg, na.rm =T)/.5)*.5,
      plus_minus = round(mean(diff, na.rm =T)/.5)*.5, 
      season_games = n_distinct(week),
      plus_ep_rt = n_distinct(week[diff > 0])/season_games,
      touchdown_xfp_share = mean(touchdown_xfp_share, na.rm =T), 
      touchdown_ppg_share = mean(touchdown_fp_share, na.rm =T), 
      pos_ep_top_12 = n_distinct(week[pos_ep_posrk <= 12])/season_games,
      pos_ep_top_24 = n_distinct(week[pos_ep_posrk <= 24])/season_games,
      pos_ep_top_36 = n_distinct(week[pos_ep_posrk <= 36])/season_games,
      flex_ep_top_12 = n_distinct(week[flex_ep_posrk <= 12])/season_games,
      flex_ep_top_24 = n_distinct(week[flex_ep_posrk <= 24])/season_games,
      flex_ep_top_36 = n_distinct(week[flex_ep_posrk <= 36])/season_games,
      team_flex_ppg = round(mean(team_flex_ppg, na.rm = T)/.5)*.5,
      team_flex_ep = round(mean(team_flex_ep, na.rm = T)/.5)*.5,
      flex_fp_share = round(mean(flex_fp_share, na.rm = T)/.001)*.001,
      flex_ep_share = round(mean(flex_ep_share, na.rm = T)/.001)*.001,
      top_sum_ppg = mean(top_sum_ppg, na.rm = T),
      top_sum_ep = mean(top_sum_ep, na.rm = T)
    ) %>%
    ungroup() %>%
    rename(pff_id = player_id, gp = season_games, season = year,
           expected_ppg = ep) %>%
    distinct()
    

    
    
}

## QB Feature Engineering -----
qb_ep_df_mean_preprocess = function(df){
  df %>%
    group_by(year, player, player_id, team) %>%
    mutate(games_played = n_distinct(week)) %>%
    group_by(year, player, player_id, team, games_played) %>%
    summarize_if(is.numeric, mean, na.rm = T) %>%
    select(-week, -games)
}

qb_ep_df_preprocess = function(df, player_rosters){
  df = df %>% left_join(player_rosters %>% rename(player_id = pff_id, year = season))
  df %>%
    filter(
      (year <= 2020 & week <= 17) | (year > 2020 & week <= 16)
    ) %>%
    mutate(
      scramble_expected_ppg = (expected_scramble_yards*.1) + (expected_scramble_tds*6),
      scramble_ppg = (scramble_yards*.1) - (scramble_tds*6),
      scramble_plus_minus = scramble_ppg - scramble_expected_ppg,
      designed_rush_expected_ppg = expected_ppg - scramble_expected_ppg - passing_expected_ppg) %>%
    group_by(year, week)  %>%
    mutate(
      ppg_pos_rk = min_rank(-ppg),
      ep_pos_rk = min_rank(-expected_ppg),
      passing_ep_pos_rk = min_rank(-passing_expected_ppg)
    ) %>%
    group_by(year, player, player_id, team) %>%
    transmute(
      games_played = n_distinct(week),
      ppg = mean(ppg, na.rm = T),
      expected_ppg = mean(expected_ppg, na.rm = T),
      scramble_expected_ppg = mean(scramble_expected_ppg, na.rm = T),
      scramble_plus_minus = mean(scramble_plus_minus, na.rm = T),
      plus_minus = ppg - expected_ppg,
      passing_expected_ppg = mean(passing_expected_ppg, na.rm = T),
      ppg_top_12 = n_distinct(week[ppg_pos_rk <= 12])/games_played,
      ppg_top_24 = n_distinct(week[ppg_pos_rk <= 24])/games_played,
      ep_top_12 = n_distinct(week[ep_pos_rk <= 12])/games_played,
      ep_top_24 = n_distinct(week[ep_pos_rk <= 24])/games_played,
      pass_ep_top_12 = n_distinct(week[passing_ep_pos_rk <= 12])/games_played,
      pass_ep_top_24 = n_distinct(week[passing_ep_pos_rk <= 24])/games_played) %>%
    ungroup() %>%
    distinct()
}

## Full EDA Table | Flex --------
player_ep_full = skill_ep_df %>%
  flex_ep_df_preprocess(player_rosters)

player_ep_filtered = player_ep_full %>%
  filter(season == 2020 | 
           (player %in% c("Courtland Sutton", "Julio Jones", "George Kittle", "Saquon Barkley") & season == 2019),
         position == "WR", gp > 1) %>%
  arrange(-expected_ppg, -plus_ep_rt)

# player_ep_filtered %>%
#   write_sheet(ss = RESEARCH_SHEET, sheet = "EP_Viewer") 


player_stats_full = player_stats %>%
  ff_feature_engineering(player_rosters) %>%
  arrange(-flex_top_24_rt) %>%
  distinct() %>%
  filter(position %in% c("WR","TE","RB")) %>%
  left_join(player_ep_full %>% select(-position), by = c("pff_id", "season"))




# quick check 

team_ep_share = player_stats_full %>%
  filter(
    (season_games >= 3 & season == 2020) |
      (player_name %in% c("Saquon Barkley", "Courtland Sutton, George Kittle") & 
         season == 2019),
    !player_name %in% c("Christian McCaffrey", "Saquon Barkley")
  ) %>%
  arrange(-flex_fp_share) %>%
  select(player, position, recent_team, season, flex_fp_share, flex_ep_share,
         team_flex_ppg, team_flex_ep) %>%
  group_by(recent_team) %>%
  top_n(3, flex_fp_share) %>%
  mutate(top_fp_share = sum(flex_fp_share, na.rm = T)/n()*3,
         top_ep_share = sum(flex_ep_share, na.rm = T)/n()*3,
         top_sum_ppg = round(mean(team_flex_ppg) * top_fp_share/.5)*.5,
         top_sum_ep = round(mean(team_flex_ep) * top_ep_share/.50)*.5)
  


player_stats_filtered = player_stats_full %>%
  left_join(player_ep_full) %>%
  filter(
    (season_games >= 3 & season == 2020) |
    (player_name %in% c("Saquon Barkley", "Courtland Sutton, George Kittle") & 
       season == 2019) 
   ) %>%
  select(-player, -boom_gm, -dakota, -contains("_passing")) %>%
  select(season, player_id, player_name, recent_team, 
         position, season_games, gp, everything()) %>%
  full_join(adp %>% 
              rename(player_name = player, position = slot_name) %>% 
              select(-team_name) %>% filter(!position %in% c("QB"))) %>%
  distinct()

# player_stats_filtered %>%
#   write_sheet(ss = RESEARCH_SHEET, sheet = "EP_Viewer") 



player_stats_rename = player_stats_filtered %>%
  select(player_name, season, position, fp_gm, expected_ppg, ud_proj, adp, ud_id) %>%
  arrange(adp)



  

  






## Full EDA Table | QB --------
qb_ep_full = qb_ep_df %>%
  qb_ep_df_preprocess(player_rosters = player_rosters)

qb_nflfastr_stats = player_stats %>% 
  ff_feature_engineering(player_rosters) %>%
  arrange(-flex_top_24_rt) %>%
  select(-contains("rec_"),-contains("flex"), -contains("target"), -contains("ay_"), -contains("_opp"), -contains("pos_top")) %>%
  distinct() %>%
  filter(position %in% c("QB")) 

qb_stats_full = qb_nflfastr_stats %>%
  left_join(qb_ep_full %>% rename(pff_id = player_id, season = year, pff_name = player, pff_team = team)) %>%
  select(-contains("fp_"), -fp, -boom_gm, -pff_name, -pff_team, -contains("trg_"), -games_played)

qb_stats_filtered = qb_stats_full %>%
  filter(season_games >= 3, season == 2020)


# full data ------
all_stats_filtered = player_stats_filtered %>%
  full_join(qb_stats_filtered)



## RB TabViz -------

player_stats_full %>%
  filter(position == "RB", season == 2020, season_games >=3, !player_name %in% c("D.Johnson", "W.Gallman")) %>%
  mutate(
    recent_team = case_when(
      player_name == "Le'Veon Bell" ~ "NYJ/KC", 
      player_name == "Kalen Ballage" ~ "NYJ/LAC", 
      player_name == "Ty Johnson" ~ "DET/NYJ", 
      TRUE ~ recent_team),
    epa = scale_this(epa)
  ) %>%
  distinct() %>%
  select(
    player_name, recent_team, season_games, fp_gm, trg_gm, everything(),
    -player_id, -pff_id,
    -season, -fp, -adot, 
    -contains("rec_"), -contains("passing"), -contains("ay_rt"),
    -seven_target_rt, 	-eight_target_rt, -nine_target_rt, -ten_target_rt,
    -ten_opp_rt, -twelve_opp_rt,
    -ay_gm, -ay_sd, -adot_sd, -position, -trg_sd, -fp_sd, -boom_gm
  ) %>% 
  distinct() %>%
  arrange(-pos_top_24_rt) %>%
  top_n(65, fp_gm) %>%
  gt(rowname_col = "player_name") %>%
  fmt_number(
    columns = c(ends_with("_sd"), ends_with("_gm"), contains("epa")),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(ends_with("_rt")),
    decimals = 0
  ) %>%
  data_color(
    columns = c(contains("fp_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(6, 35)
    )
  ) %>%
  data_color(
    columns = c(ends_with("_rt")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 1)
    )
  ) %>% 
  data_color(
    columns = c(contains("trg_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 10)
    )
  ) %>%
  data_color(
    columns = c(contains("epa")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-5, 5)
    )
  ) %>%
  cols_label(
    season_games = md("Games Played"),
    recent_team = md("Tm."),
    three_target_rt = md("3+ Targets"),
    six_target_rt = md("6+ Targets"),
    fifteen_opp_rt = md("15+ Opps."),
    eighteen_opp_rt = md("18+ Opps."),
    twenty_opp_rt = md("20+ Opps."),
    pos_top_12_rt = md("RB1 *(12-Team)*"),
    pos_top_24_rt = md("RB2"),
    flex_top_12_rt = md("Top-12 Flex"),
    flex_top_24_rt = md("Top-24 Flex"),
    flex_top_36_rt = md("Top-36 Flex"),
    fp_gm = md("PPR"),
    trg_gm = md("Targets"),
    epa = md("EPA")
  ) %>% tab_header(
    title = "Running Backs",
    subtitle = "2020 Regular Season"
  )


## WR TabViz -------


player_stats_full %>%
  mutate(epa = scale_this(epa)) %>%
  filter(position == "WR", season == 2020, season_games >=3) %>%
  select(player_name, recent_team, season_games,  fp_gm, fp_sd, trg_gm, trg_sd, ay_gm, everything(), -player_id, -pff_id,
         -season, -fp, -adot, -position,-dakota,
         -contains("opp"), -contains("passing"), -ninety_ay_rt,
         -three_target_rt, -seven_target_rt, -nine_target_rt,
         -ay_sd, -adot_sd, -boom_gm
  ) %>% 
  distinct() %>%
  arrange(-pos_top_12_rt) %>%
  top_n(65, fp_gm) %>%
  gt(rowname_col = "player_name") %>%
  fmt_number(
    columns = c(ends_with("_sd"), ends_with("_gm"), contains("epa")),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(ends_with("_rt")),
    decimals = 0
  ) %>%
  data_color(
    columns = c(contains("fp_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 40)
    )
  ) %>%
  data_color(
    columns = c(ends_with("_rt")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 1)
    )
  ) %>% 
  data_color(
    columns = c(contains("trg_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 20)
    )
  ) %>%
  data_color(
    columns = c(contains("ay_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(10, 150)
    )) %>%
  data_color(
    columns = c(contains("epa")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-5,5)
    )) %>%
  data_color(
    columns = c(contains("trg_sd")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0,8)
    )) %>%
  data_color(
    columns = c(contains("fp_sd")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(3, 15)
    )) %>%
  data_color(
    columns = c(contains("fp_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 30)
    )) %>%
  cols_label(
    season_games = md("Games Played"),
    fp_gm = md("PPR"),
    ay_gm = md("Air Yds."),
    trg_gm = md("Targets"),
    recent_team = md("Tm."),
    six_target_rt = md("6+ Targets"),
    eight_target_rt = md("8+ Targets"),
    ten_target_rt = md("10+ Targets"),
    fifty_ay_rt = md("50+ Air Yards"),
    seventyfive_ay_rt = md("75+ Air Yards"),
    hundo_ay_rt = md("100+ Air Yards"),
    pos_top_12_rt = md("WR1 *(12-Team)*"),
    pos_top_24_rt = md("WR2"),
    flex_top_12_rt = md("Top-12 Flex"),
    flex_top_24_rt = md("Top-24 Flex"),
    flex_top_36_rt = md("Top-36 Flex"),
    rec_top_12_rt = md("Top-12 WR/TE"),
    rec_top_24_rt = md("Top-24 WR/TE"),
    rec_top_36_rt = md("Top-36 WR/TE"),
    trg_sd = md("Trg ±"),
    fp_sd = md("FP ±"),
    epa = md("EPA")) %>%
  tab_header(
    title = "Wide Receivers",
    subtitle = "2020 Regular Season"
  )





## TE TabViz -----

player_stats_full %>%
  mutate(epa = scale_this(epa)) %>%
  filter(position == "TE", season == 2020, season_games >=3) %>%
  select(player_name, recent_team, season_games,  fp_gm, fp_sd, trg_gm, trg_sd, ay_gm, everything(), -player_id, -pff_id,
         -season, -fp, -adot, -position,-dakota,
         -contains("opp"), -contains("passing"), -ninety_ay_rt,
         -three_target_rt, -seven_target_rt, -nine_target_rt,
         -ay_sd, -adot_sd, -boom_gm, -pos_top_24_rt
  ) %>% 
  distinct() %>%
  arrange(-flex_top_24_rt) %>%
  top_n(35, fp_gm) %>%
  gt(rowname_col = "player_name") %>%
  fmt_number(
    columns = c(ends_with("_sd"), ends_with("_gm"), contains("epa")),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(ends_with("_rt")),
    decimals = 0
  ) %>%
  data_color(
    columns = c(contains("fp_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 40)
    )
  ) %>%
  data_color(
    columns = c(ends_with("_rt")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 1)
    )
  ) %>% 
  data_color(
    columns = c(contains("trg_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 20)
    )
  ) %>%
  data_color(
    columns = c(contains("ay_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(10, 150)
    )) %>%
  data_color(
    columns = c(contains("epa")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-5,5)
    )) %>%
  data_color(
    columns = c(contains("trg_sd")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0,8)
    )) %>%
  data_color(
    columns = c(contains("fp_sd")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(3, 15)
    )) %>%
  data_color(
    columns = c(contains("fp_gm")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 30)
    )) %>%
  cols_label(
    season_games = md("Games Played"),
    fp_gm = md("PPR"),
    ay_gm = md("Air Yds."),
    trg_gm = md("Targets"),
    recent_team = md("Tm."),
    six_target_rt = md("6+ Targets"),
    eight_target_rt = md("8+ Targets"),
    ten_target_rt = md("10+ Targets"),
    fifty_ay_rt = md("50+ Air Yards"),
    seventyfive_ay_rt = md("75+ Air Yards"),
    hundo_ay_rt = md("100+ Air Yards"),
    pos_top_12_rt = md("TE1 *(12-Team)*"),
    flex_top_12_rt = md("Top-12 Flex"),
    flex_top_24_rt = md("Top-24 Flex"),
    flex_top_36_rt = md("Top-36 Flex"),
    rec_top_12_rt = md("Top-12 WR/TE"),
    rec_top_24_rt = md("Top-24 WR/TE"),
    rec_top_36_rt = md("Top-36 WR/TE"),
    trg_sd = md("Trg ±"),
    fp_sd = md("FP ±"),
    epa = md("EPA")) %>%
  tab_header(
    title = "Tight Ends",
    subtitle = "2020 Regular Season"
  )
# QB TabViz -----
qb_stats_tab_df = qb_stats_full %>% 
  filter(season_games >= 3) %>%
  mutate(epa = scale_this(epa),
         dakota = scale_this(dakota)) %>%
  filter(season == 2020) %>%
  select(-season, -player_id, -pff_id, -epa, -adot_sd) %>%
  select(player_name, recent_team, position, season_games, dakota, 
         ppg, expected_ppg, plus_minus,
         scramble_expected_ppg, scramble_plus_minus,
         passing_expected_ppg, adot,
         everything()) 

qb_stats_tab_df %>%
  top_n(40, ppg)  %>%
  arrange(-expected_ppg) %>% 
  gt(rowname_col = "player_name") %>%
  fmt_number(
    columns = c(contains("ppg"), contains("plus_"), contains("adot"), contains("epa"), contains("dakota")),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(contains("top_"), contains("_passing")),
    decimals = 0
  ) %>%
  cols_label(
    dakota = md("Eff."),
    player_name = md("Quarterback"),
    recent_team = md("Tm"),
    season_games = md("Games"),
    ppg = md("PPG"),
    expected_ppg = md("Exp. PPG"),
    scramble_expected_ppg = md("Scramble Exp. PPG"),
    scramble_plus_minus = md("Scramble ±"),
    plus_minus = md("PPG ±"),
    passing_expected_ppg = md("Pass Exp. PPG"),
    adot = md("ADOT"),
    threehundo_passing = md("300+ Pass Yds."),
    threefifty_passing = md("350+ Pass Yds."),
    ppg_top_12 = md("QB1 (*12 Tm*)"),
    ppg_top_24 = md("QB2"),
    ep_top_12 = md("Exp. Pts QB1"),
    ep_top_24 = md("Exp. Pts QB2"),
    pass_ep_top_12 = md("Pass Exp. Pts QB1"),
    pass_ep_top_24 =  md("Pass Exp. Pts QB2")
  ) %>%
  data_color(
    columns =  c("ppg", "expected_ppg"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 30)
    )) %>%
  data_color(
    columns =  c("plus_minus"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-6, 6)
    )) %>%
  data_color(
    columns =  c("scramble_plus_minus"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-3, 3)
    )) %>%
  data_color(
    columns =  c("scramble_expected_ppg"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 5)
    )) %>%
  data_color(
    columns =  c("passing_expected_ppg"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 25)
    )) %>%
  data_color(
    columns =  c(contains("top"), contains("three")),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(0, 1)
    )) %>%
  data_color(
    columns =  c("adot"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(3, 12)
    )) %>%
  data_color(
    columns =  c("dakota"),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(-3, 3)
    )) 




### comparison viz - QB ------

qb_stats_long = qb_stats_full %>%
  pivot_longer(
    cols = c(-player_name, -recent_team, -position, -season, -player_id, -pff_id),
    names_to = "metric",
    values_to = "value"
  )



qb_stats_chart_df = qb_stats_long %>%
  filter(
         season == 2020,
         !(metric %in% c("adot_sd", "epa", "season_games"))) %>%
  mutate(fake_name = case_when(
    player_name == "Justin Herbert" ~ "QB B", 
    player_name == "Russell Wilson" ~ "QB C", 
    TRUE ~ "QB A"),
         metric = case_when(
           metric == "dakota" ~ "Adv. Efficiency", 
           metric == "ep_top_12" ~ "Top 12 Exp. FP (Among QBs) Rate",
           metric == "ep_top_24" ~ "Top 24 Exp. FP (Among QBs) Rate",
           metric == "passing_expected_ppg" ~ "Exp. FP on Passes",
           metric == "scramble_expected_ppg" ~ "Exp. FP  on Scrambles",
           metric == "ppg" ~ "FP/Gm",
           metric == "ppg_top_12" ~ "Top-12 FP (Among QBs) Rate",
           metric == "ppg_top_24" ~ "Top-24 FP (Among QBs) Rate",
           metric == "pass_ep_top_12" ~ "Passing Top 12 Exp. FP (Among QBs) Rate",
           metric == "pass_ep_top_24" ~ "Passing Top 24 Exp. FP (Among QBs) Rate",
           metric == "plus_minus" ~ "FP vs. Expected",
           metric == "scramble_plus_minus" ~ "FP vs. Expected on Scrambles",
           metric == "threehundo_passing" ~ "300+ Yds Passing",
           metric == "threefifty_passing" ~ "350+ Yds Passing",
           metric == "expected_ppg" ~ "Expected FP/Gm",
           TRUE ~ metric
           ))

#write_csv(qb_stats_chart_df, "./ep_quickcheck/www/2020_qb_stats.csv")

qb_stats_chart_df %>%
  ggplot()+
  aes(x = value, y = player_name)+
  geom_col(aes(fill = player_name))+
  geom_text(aes(label = round(value,2)), hjust = "inward", size = 3)+
  facet_wrap(~ metric, scales = "free") +
  theme_light()+
  labs(fill = "", y = "", x = "")+
  theme(
    strip.text.x = element_text(
      size = 12, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 12, color = "black", face = "bold"
    ),
    strip.background = element_rect(
      color="black", fill="#ffffff", size=1.5, linetype="solid"
    )
  )
### comparison viz - FLEX -----

player_stats_long = player_stats_full %>%
  pivot_longer(
    cols = c(-player_name, -player, -recent_team, -position, -season, -player_id, -pff_id),
    names_to = "metric",
    values_to = "value"
  )



player_stats_chart_df = player_stats_long %>%
  filter(
    (season == 2020 & !(player_name %in% c("Saquon Barkley", "Courtland Sutton, George Kittle"))) |
      (player_name %in% c("Saquon Barkley", "Courtland Sutton, George Kittle") & 
              season == 2019)) %>%
  filter(
    !(metric %in% c(
      "adot",
      "adot_sd",
      "dakota",
      "rec_top_12_rt",
      "rec_top_24_rt",
      "rec_top_36_rt",
      "threehundo_passing",
      "threefifty_passing",
      "season_games"
    )
  )) %>%
  mutate(value =
           case_when(
             metric == "touchdown_xfp_share" ~ (1-value),
             metric == "touchdown_ppg_share" ~ (1-value),
             TRUE ~ value)
           ) %>%
  mutate(
    #metric category
    metric_cat = case_when(
      metric == "ay_gm" ~ "Opportunity",
      metric == "ay_sd" ~ "Opportunity",
      metric == "boom_gm" ~ "Production",
      metric == "eight_target_rt" ~ "Opportunity",
      metric == "eighteen_opp_rt" ~ "Opportunity",
      metric == "ep_sd" ~ "Opportunity",
      metric == "epa" ~ "Efficiency",
      metric == "expected_ppg" ~ "Opportunity",
      metric == "fifteen_opp_rt" ~ "Opportunity",
      metric == "fifty_ay_rt" ~ "Opportunity",
      metric == "flex_ep_top_12" ~ "Opportunity",
      metric == "flex_ep_top_24" ~ "Opportunity",
      metric == "flex_ep_top_36" ~ "Opportunity",
      metric == "flex_top_12_rt" ~ "Production",
      metric == "flex_top_24_rt" ~ "Production",
      metric == "flex_top_36_rt" ~ "Production",
      metric == "fp" ~ "Production",
      metric == "fp_gm" ~ "Production",
      metric == "fp_sd" ~ "Production",
      metric == "gp" ~ "Opportunity",
      metric == "hundo_ay_rt" ~ "Opportunity",
      metric == "nine_target_rt" ~ "Opportunity",
      metric == "ninety_ay_rt" ~ "Opportunity",
      metric == "plus_ep_rt" ~ "Efficiency",
      metric == "plus_minus" ~ "Efficiency",
      metric == "pos_ep_top_12" ~ "Opportunity",
      metric == "pos_ep_top_24" ~ "Opportunity",
      metric == "pos_ep_top_36" ~ "Opportunity",
      metric == "pos_top_12_rt" ~ "Production",
      metric == "pos_top_24_rt" ~ "Production",
      metric == "pos_top_36_rt" ~ "Production",
      metric == "seven_target_rt" ~ "Opportunity",
      metric == "seventyfive_ay_rt" ~ "Opportunity",
      metric == "six_target_rt" ~ "Opportunity",
      metric == "ten_opp_rt" ~ "Opportunity",
      metric == "ten_target_rt" ~ "Opportunity",
      metric == "three_target_rt" ~ "Opportunity",
      metric == "touchdown_ppg_share" ~ "Efficiency",
      metric == "touchdown_xfp_share" ~ "Efficiency",
      metric == "trg_gm" ~ "Opportunity",
      metric == "trg_sd" ~ "Opportunity",
      metric == "twelve_opp_rt" ~ "Opportunity",
      metric == "twenty_opp_rt" ~ "Opportunity",
      TRUE ~ "NA"),
  #metrics
    metric = case_when(
      metric == "ay_gm" ~ "AY/Gm",
      metric == "ay_sd" ~ "AY Std. Dev.",
      metric == "boom_gm" ~ "Games 1 SD Over EP",
      metric == "eight_target_rt" ~ "Targets - 08+",
      metric == "eighteen_opp_rt" ~ "Opps - 18+",
      metric == "ep_sd" ~ "Expected FP ±",
      metric == "epa" ~ "EPA",
      metric == "expected_ppg" ~ "Exp. FP/Gm",
      metric == "fifteen_opp_rt" ~ "Opps - 15+",
      metric == "fifty_ay_rt" ~ "AY - 050+",
      metric == "flex_ep_top_12" ~ "Exp. Pts. FLEX Top-12 Rate",
      metric == "flex_ep_top_24" ~ "Exp. Pts. FLEX Top-24 Rate",
      metric == "flex_ep_top_36" ~ "Exp. Pts. FLEX Top-36 Rate",
      metric == "flex_top_12_rt" ~ "FP FLEX Top-12 Rate",
      metric == "flex_top_24_rt" ~ "FP FLEX Top-24 Rate",
      metric == "flex_top_36_rt" ~ "FP FLEX Top-36 Rate",
      metric == "fp" ~ "FP - Total",
      metric == "fp_gm" ~ "FP/Gm",
      metric == "fp_sd" ~ "FP ±",
      metric == "gp" ~ "Gms.",
      metric == "hundo_ay_rt" ~ "AY - 100+",
      metric == "nine_target_rt" ~ "Targets - 09+",
      metric == "ninety_ay_rt" ~ "AY - 090+",
      metric == "plus_ep_rt" ~ "Positive FPVE Rate",
      metric == "plus_minus" ~ "FP vs. Exp.",
      metric == "pos_ep_top_12" ~ "Exp. Pts. Pos. Top-12 Rate",
      metric == "pos_ep_top_24" ~ "Exp. Pts. Pos. Top-24 Rate",
      metric == "pos_ep_top_36" ~ "Exp. Pts. Pos. Top-36 Rate",
      metric == "pos_top_12_rt" ~ "FP Pos. Top-12 Rate",
      metric == "pos_top_24_rt" ~ "FP Pos. Top-24 Rate",
      metric == "pos_top_36_rt" ~ "FP Pos. Top-36 Rate",
      metric == "seven_target_rt" ~ "Targets - 07+",
      metric == "seventyfive_ay_rt" ~ "AY - 075+",
      metric == "six_target_rt" ~ "Targets - 06+",
      metric == "ten_opp_rt" ~ "Opps - 10+",
      metric == "ten_target_rt" ~ "Targets - 10+",
      metric == "three_target_rt" ~ "Targets - 03+",
      metric == "touchdown_ppg_share" ~ "Non-TD Share of FP",
      metric == "touchdown_xfp_share" ~ "Non-xTD Share of xFP",
      metric == "trg_gm" ~ "Targets/Gm",
      metric == "trg_sd" ~ "Targets/Gm SD",
      metric == "twelve_opp_rt" ~ "Opps - 12+",
      metric == "twenty_opp_rt" ~ "Opps - 20+",
      TRUE ~ metric),
  metric = reorder(metric, metric_cat))

#write_csv(player_stats_chart_df, "./ep_quickcheck/www/2020_flex_stats.csv")






player_stats_chart_df %>%
  ggplot()+
  aes(x = value, y = player_name)+
  geom_col(aes(fill = player_name))+
  geom_text(aes(label = round(value,3)), hjust = "inward", size = 3)+
  facet_wrap( ~ metric, scales = "free", nrow = 6) +
  labs(x = "", y = "", fill = "")+
  theme_light()+
  theme(
    strip.text.x = element_text(
      size = 6, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 6, color = "black", face = "bold"
    ),
    strip.background = element_rect(
      color="black", fill="#ffffff", size=1.5, linetype="solid"
    )
  )