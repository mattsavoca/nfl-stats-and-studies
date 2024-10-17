# Libs #####
library(easypackages)
packages(prompt = F, "tidyverse", "janitor","skimr", "slider", "tidymodels", "plotly", "glue" ,"here")
theme_set(theme_light())

# paths #####
PROJECT_DIR = "~/Dropbox/Matt Savoca/Projects/NFL 2021/"
SCRAPE_PATH = glue::glue("{PROJECT_DIR}/projects/pff_exp_fpts_scrape/pff_ep_scraping.R")
THEME_PATH = glue::glue("{PROJECT_DIR}/scripts/ajreinhard_plot_func.R")
PROJECTION_PREFIX = "awesemo"

source(SCRAPE_PATH)
# helper funcs
window_func = function(.x, .i, .before, window, .f =  ~mean(.x)){
  z = slide_index(
    .x = .x,
    .i = .i,
    .f = .f,
    .after = window
  ) %>%
    as.numeric()
  return(z)
}

pff_name_fixer = function(x){
  x = gsub("Odell Beckham Jr.", "Odell Beckham", x)
  x = gsub("Allen Robinson II", "Allen Robinson", x)
  x = gsub("Will Fuller V", "Will Fuller", x)
  x = gsub("D.J. Chark Jr.", "D.J. Chark", x)
  x = gsub("Melvin Gordon III", "Melvin Gordon", x)
  x = gsub("Jeff Wilson Jr.", "Jeff Wilson", x)
  x = gsub("Irv Smith Jr.", "Irv Smith", x)
  x = gsub("Anthony McFarland Jr.", "Anthony McFarland", x)
}
# EP Data ####
# Local:
qb_ep_df_raw = read_csv(paste0(PROJECT_DIR,"/data/pff_qb_ep.csv"))
skill_ep_df_raw = read_csv(paste0(PROJECT_DIR, "data/pff_skill_ep.csv"))


# Projections ######
proj_df_raw = read_csv(glue("{here()}/data/{PROJECTION_PREFIX}_proj.csv"))


# Duplicate DF to avoid re-downloading #####
qb_ep_df = qb_ep_df_raw %>% clean_names() %>% mutate(position = "QB")
skill_ep_df = skill_ep_df_raw %>% clean_names()
proj_df = proj_df_raw %>% clean_names()



# Combine QB and Skill EP into one DF #####
ep_df  =full_join(qb_ep_df, skill_ep_df, 
                  c("year", "week", "points", "player_id", "position", 
                    "ppg", "diff", "team_id", 
                    "expected_points", "diff_ppg", "team", 
                    "expected_ppg", "games", "player")) %>%
  select(player, player_id, position, team, team_id, everything())



### recency WINDOW
#recency  = 8
## Adjustments
create_ff_ep_df = function(ep_df, recency){
  ep_adj = ep_df %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           player_id = as.factor(player_id),
           is_flex = if_else(position == "QB", 0, 1),
           team_id = as.factor(team_id),
           td_based_ep = expected_passing_tds * 4 + 6 *
             (expected_designed_rush_tds + expected_rushing_tds + expected_scramble_tds+ expected_receiving_tds),
           td_agnostic_expected_ppg = expected_ppg - td_based_ep,
           scramble_based_ep = (scramble_yards/10 + scramble_tds*6),
           scramle_agnostic_expected_ppg = expected_ppg - scramble_based_ep,
           notd_noscramb_ep = expected_ppg - scramble_yards/10 - td_based_ep
    ) %>%
    arrange(player_id, -year, -week) %>%
    mutate(week = as.factor(week),
           year = as.factor(year),
           player = pff_name_fixer(player)
    ) %>%
    ungroup() %>%
    select(player, player_id, position, team, team_id, everything() )%>%
    group_by(team, year, week) %>%
    mutate(team_ep = sum(expected_ppg, na.rm = T),
           team_ppg = sum(points, na.rm = T),
           team_fpoe = mean(diff, na.rm = T))
  
  
  #dallas filter
  # ep_adj = ep_adj %>%
  #   filter(team != "DAL"| 
  #           as.numeric(year) < 2020  |
  #          (team == "DAL" & as.numeric(year) == 2020 & as.numeric(week) < 5))
  
  
  #means
  ep_mean = ep_adj %>%
    group_by(player_id) %>%
    mutate(game_recency = row_number()) %>%
    mutate(across(c(where(is.numeric), -game_recency), ~window_func(
      .x = .,
      .i = game_recency, 
      window = recency,
      .f =  ~mean(.x,na.rm = T)))) %>%
    filter(game_recency == min(game_recency)) %>%
    select(full_name  = player, pff_id = player_id, position,
           ppg = points, ep = expected_points,
           notd_ep = td_agnostic_expected_ppg,
           noscramb_ep = scramle_agnostic_expected_ppg,
           notd_noscramb_ep,
           team_ep,
           team_ppg,
           team_fpoe, 
           game_recency)
  
  
  
  #minimums
  ep_min = ep_adj %>%
    arrange(player_id, -year, -week) %>%
    group_by(player_id) %>%
    mutate(game_recency = row_number()) %>%
    mutate(across(c(where(is.numeric), -game_recency), ~window_func(
      .x = ., .i = game_recency,
      window = recency, 
      .f =  ~min(.x,na.rm = T)))) %>%
    filter(game_recency == min(game_recency)) %>%
    select(full_name  = player, pff_id = player_id, position,
           min_ppg = points, min_ep = expected_points,
           min_notd_ep = td_agnostic_expected_ppg,
           min_noscramb_ep = scramle_agnostic_expected_ppg,
           min_notd_noscramb_ep = notd_noscramb_ep,
           min_team_ep = team_ep,
           min_team_ppg = team_ppg,
           min_team_fpoe = team_fpoe,
           min_game_recency = game_recency)
  
  
  #maximums
  ep_max = ep_adj %>%
    arrange(player_id, -year, -week) %>%
    group_by(player_id) %>%
    mutate(game_recency = row_number()) %>%
    mutate(across(c(where(is.numeric), -game_recency), 
                  ~window_func(.x = ., .i = game_recency,window = recency, 
                               .f =  ~max(.x,na.rm = T)))) %>%
    filter(game_recency == min(game_recency)) %>%
    select(full_name  = player, pff_id = player_id, position,
           max_ppg = points, max_ep = expected_points,
           max_notd_ep = td_agnostic_expected_ppg,
           max_noscramb_ep = scramle_agnostic_expected_ppg,
           max_notd_noscramb_ep = notd_noscramb_ep,
           max_team_ep = team_ep,
           max_team_ppg = team_ppg,
           max_team_fpoe = team_fpoe,
           max_game_recency = game_recency)
  
  
  # rolling df
  ep_df_rolling = ep_mean %>% 
    left_join(ep_min) %>% 
    left_join(ep_max) %>% 
    distinct()
  
  return(ep_df_rolling)
}




create_ep_proj_df = function(proj_df, ep_df, recency){
  #get player IDs
  ud_pff_id_df = 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1GtvJI9xjoym9I6BMDx7Pzud7EuO2XlrcFit6sUGd4ng/edit#gid=1595921121",
      sheet = "Name Key") %>%
    clean_names() %>%
    filter(!is.na(ud_id)) %>%
    select(ud_id, pff_id)
  
  
  # join on projection df
  proj_adj = proj_df %>% 
    mutate(name = trimws(name)) %>%
    rename(adp = ovr_adp, proj = aw_proj, full_name = name) %>%
    group_by(position) %>%
    mutate(pos_max_proj  = max(proj, na.rm = T)) %>%
    rowwise() %>%
    mutate(proxy_proj = round(proj/pos_max_proj/.005)*.005) %>%
    ungroup() %>%
    left_join(ud_pff_id_df) %>%
    distinct() %>%
    left_join(create_ff_ep_df(ep_df, recency) %>% 
                select(-full_name, -position), by = "pff_id")
  
  #impute missing
  proj_rec = proj_adj %>% recipe(ppg ~ ., data = proj_adj) %>%
    step_naomit(adp) %>%
    step_impute_linear(all_numeric(), -all_outcomes(), impute_with = imp_vars(position, adp, proj))
  
  #final df
  proj_imputed = proj_rec %>% prep() %>% juice()
  
  
  return(proj_imputed)
  
}

test_ep_ff_df = create_ff_ep_df(ep_df, 16)


proj_5 = create_ep_proj_df(proj_df, ep_df, 5)
proj_8 = create_ep_proj_df(proj_df, ep_df, 8)
proj_16 = create_ep_proj_df(proj_df, ep_df, 16)



write_csv(proj_5, "./projects/ep_quickcheck/ep_quickcheck/www/proj_5.csv")
write_csv(proj_8, "./projects/ep_quickcheck/ep_quickcheck/www/proj_8.csv")
write_csv(proj_16, "./projects/ep_quickcheck/ep_quickcheck/www/proj_16.csv")



# proj_8 %>%
#   group_by(team) %>%
#   summarize(ep = mean(team_ep, na.rm = T),
#             ppg = mean(team_ppg, na.rm = T),
#             fpoe = ppg - ep) %>%
#   arrange(-ppg, -ep, -fpoe) %>%
#   View()



# proj_16 %>%
#   group_by(position) %>%
#   mutate(pos_adp = rank(adp)) %>%
#   arrange(position, proxy_proj) %>%
#   ggplot()+
#   aes(pos_adp, proxy_proj, group = position, color = position) %>%
#   geom_smooth(se = F)+
#   scale_y_continuous(labels = scales::percent)+
#   expand_limits(y = .2)+
#   coord_cartesian(xlim = c(0, 40))+
#   labs(x = "Positional ADP",
#        y = "Percentage of Top Positional Projection",
#        color = NULL)
# 









