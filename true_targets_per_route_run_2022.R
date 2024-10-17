library(easypackages)
packages("plotly", "jsonlite","googlesheets4", "tidyverse", "ebbr", prompt = F)
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
work_folder = paste0(projects_folder, "NFL 2022/")
theme_set(theme_FE)

# Data Ingest =======

pff_rec_raw = read_csv("./pff_receiving_stats/pff_rec.csv")

pff_players_raw = read_csv("./pff_players.csv")

player_draft_rds = pff_players_raw %>%
  mutate(round = if_else(is.na(round), 8, round)) %>%
  select(player_id, draft_round = round)
  

# RBs -----


rookie_rb_df = pff_rec_raw %>%
  mutate(rookie_year = draft_season == year) %>%
  filter(position == "HB", rookie_year) %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 0)

  
tprr_rb_df = pff_rec_raw %>% 
  filter(position == "HB") %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 1) %>%
  rbind(rookie_rb_df) %>%
  left_join(player_draft_rds) %>%
  add_ebb_estimate(targets, routes) %>%
  arrange(-.fitted)

ggplotly(
  tprr_rb_df %>%
    filter(career == 0) %>%
    group_by(round) %>%
    summarize_if(is.numeric, mean, na.rm  = T) %>%
    ungroup() %>%
    ggplot()+
    aes(xmin = .low, xmax = .high, x = .fitted, y = round)+
    geom_errorbarh(height = 0)+
    geom_point()+
    geom_vline(aes(xintercept = mean(.fitted)))+
    scale_y_reverse(breaks = seq(1,8))+
    scale_x_continuous(labels = percent_format())+
    labs(
      title = "RB Rookie True TPRR By Round",
      x = "Draft Round",
      y = "True Targets Per Route Run"
    )
  
)

# Wideouts -----

rookie_wr_df = pff_rec_raw %>%
  mutate(rookie_year = draft_season == year) %>%
  filter(position == "WR", rookie_year) %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 0)




tprr_wr_df = pff_rec_raw %>% 
  filter(position == "WR") %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 1) %>%
  rbind(rookie_wr_df) %>%
  left_join(player_draft_rds) %>%
  add_ebb_estimate(targets, routes) %>%
  arrange(-.fitted)

ggplotly(
  tprr_wr_df %>%
    filter(career == 0) %>%
    group_by(round) %>%
    summarize_if(is.numeric, mean, na.rm  = T) %>%
    ungroup() %>%
    ggplot()+
    aes(xmin = .low, xmax = .high, x = .fitted, y = round)+
    geom_errorbarh(height = 0)+
    geom_point()+
    geom_vline(aes(xintercept = mean(.fitted)))+
    scale_y_reverse(breaks = seq(1,8))+
    scale_x_continuous(labels = percent_format())+
    labs(
      title = "WR Rookie True TPRR By Round",
      x = "Draft Round",
      y = "True Targets Per Route Run"
    )
)


# Tight Ends --------


rookie_te_df = pff_rec_raw %>%
  mutate(rookie_year = draft_season == year) %>%
  filter(position == "TE", rookie_year) %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 0)




tprr_te_df = pff_rec_raw %>% 
  filter(position == "TE") %>%
  select(player, player_id, position, targets, routes, pass_plays) %>%
  group_by(player_id, player, position) %>%
  summarize(
    games = n(),
    targets = sum(targets),
    routes = sum(pass_plays)
  ) %>%
  filter(routes >= targets) %>%
  mutate(career = 1) %>%
  rbind(rookie_te_df) %>%
  left_join(player_draft_rds) %>%
  add_ebb_estimate(targets, routes) %>%
  arrange(-.fitted)

ggplotly(

  tprr_te_df %>%
    filter(career == 0) %>%
    group_by(round) %>%
    summarize_if(is.numeric, mean, na.rm  = T) %>%
    ungroup() %>%
    ggplot()+
    aes(xmin = .low, xmax = .high, x = .fitted, y = round)+
    geom_errorbarh(height = 0)+
    geom_point()+
    geom_vline(aes(xintercept = mean(.fitted)))+
    scale_y_reverse(breaks = seq(1,8))+
    scale_x_continuous(labels = percent_format())+
    labs(
      title = "TE Rookie True TPRR By Round",
      x = "Draft Round",
      y = "True Targets Per Route Run"
    )
)
