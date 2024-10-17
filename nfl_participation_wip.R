# Install Packages and Dependencies ------
source("./dependencies_and_themes.R")
source("./helpers.R")

# Extract Data -------
SEASONS = 2017:2022
nfl_partic_raw = nflreadr::load_participation(seasons = SEASONS)
nfl_pbp_raw = nflreadr::load_pbp(seasons = SEASONS)
nfl_playcallers_raw_fromURL = nflreadr::csv_from_url("https://raw.githubusercontent.com/samhoppen/NFL_public/main/data/all_playcallers.csv")
nfl_playcallers_raw = nfl_playcallers_raw_fromURL %>% filter(season %in% SEASONS)
nfl_rosters_raw = nflreadr::load_rosters(seasons = SEASONS)



# Transform and Load ------
nfl_partic = nfl_partic_raw %>%
  separate(offense_personnel, into = c("n_rb", "n_te", "n_wr"),
           sep = ", ",
           convert = TRUE) %>% 
  separate(defense_personnel, into = c("n_dl", "n_lb", "n_db"),
           sep = ", ",
           convert = TRUE) %>% 
  mutate(
    offense_formation = tolower(offense_formation),
    n_rb = quick_parse_number(n_rb),
    n_te = quick_parse_number(n_te),
    n_wr = quick_parse_number(n_wr),
    offense_personnel_code = paste0(as.character(n_rb), as.character(n_te)),
    n_dl = quick_parse_number(n_dl),
    n_lb = quick_parse_number(n_lb),
    n_db = quick_parse_number(n_db))



nfl_pbp = nfl_pbp_raw %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, posteam_playcaller = off_play_caller), 
            by = c("season", "posteam" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, defteam_playcaller = def_play_caller), 
            by = c("season", "defteam" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, home_off_play_caller = off_play_caller), 
            by = c("season", "home_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, away_off_play_caller = off_play_caller), 
            by = c("season", "away_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, home_def_play_caller = def_play_caller), 
            by = c("season", "home_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, away_def_play_caller = def_play_caller), 
            by = c("season", "away_team" = "team", "game_id")) %>%
  left_join(nfl_partic %>% select(
    game_id = nflverse_game_id,
    play_id,
    posteam = possession_team,
    posteam_formation = offense_formation,
    posteam_personnel = offense_personnel_code,
    n_rb,
    n_te,
    n_wr,
    n_dl,
    n_lb,
    n_db,
    posteam_players = offense_players,
    posteam_n_players = n_offense,
    defteam_players= defense_players,
    defteam_n_players = n_defense
  )) %>%
  generate_primary_passers() %>%
  left_join(nfl_rosters_raw %>% select(season, home_team = team, home_primary_passer_name = full_name, home_primary_passer_id = gsis_id)) %>%
  left_join(nfl_rosters_raw %>% select(season, away_team = team, away_primary_passer_name = full_name, away_primary_passer_id = gsis_id)) %>%
  mutate(
    posteam_primary_passer_name = case_when(is.na(home_primary_passer_id) ~ as.character(NA), 
                                            home_team == posteam ~ home_primary_passer_name, 
                                            away_team == posteam ~ away_primary_passer_name, 
                                            T ~ as.character(NA)),
    posteam_primary_passer_id = case_when(is.na(home_primary_passer_id) ~ as.character(NA), 
                                          home_team == posteam ~ home_primary_passer_id, 
                                          away_team == posteam ~ away_primary_passer_id, 
                                          T ~ as.character(NA)),
    home_team_players = if_else(posteam == home_team, posteam_players, defteam_players),
    home_team_n_players = if_else(posteam == home_team, posteam_n_players, defteam_n_players),
    away_team_players = if_else(posteam == away_team, posteam_players, defteam_players),
    away_team_n_players = if_else(posteam == away_team, posteam_n_players, defteam_n_players)
  ) 
  
# Data Visualization -------
## Playcaller/QB Combo Top EPA/Play -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name)) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(n >= 100) %>%
  top_n(25, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.025)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Reid-Mahomes Tandem Dominates EPA per Play",
    subtitle = "All Team Plays in Games with Playercaller/QB Combo 2017-2022",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )

## Playcaller/QB Combo Top EPA/Dropback -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), qb_dropback == 1) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(n >= 100) %>%
  top_n(25, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.025)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Dropback",
    subtitle = "All Dropbacks in Games with Playercaller/QB Combo 2017-2022",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )


## Playcaller/QB Combo Top EPA/Drive -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), qb_dropback == 1) %>%
  mutate(
    playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name),
    unique_drive_id = paste0(game_id,"_drive",fixed_drive)) %>%
  group_by(playcaller_qb_combo, posteam, unique_drive_id) %>%
  summarize(
    plays = n(),
    epa = sum(epa, na.rm = T)
  ) %>% 
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    drives = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(drives >= 60, epa > 0) %>%
  top_n(25, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.025)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Drive",
    subtitle = "All Drives in Games with Playercaller/QB Combo 2017-2022",
    y = "Playcaller-QB Combo",
    x = "EPA/Drive"
  )

nfl_pbp %>% glimpse()

## Playcaller/QB Combo Top EPA/Rush -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), 
         play_type == "run" | qb_scramble == 1 
         ) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  left_join(nfl_rosters_raw %>% select(season, posteam = team, rusher_position = position, rusher_name = full_name, )) %>%
  select(game_id, play_id, season, week, home_team, away_team, posteam, epa) %>%
  View()
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(n >= 100) %>%
  top_n(25, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.025)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Rush",
    subtitle = "All Team Rushes and Scrambles in Games with Playercaller/QB Combo 2017-2022",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )

