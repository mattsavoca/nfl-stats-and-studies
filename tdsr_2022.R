current_week = 23
weekly_ss = "https://docs.google.com/spreadsheets/d/1UMvUorrtRTkN2n6l9gd5xUlg0iiaJzCy19pP6QWUnDo/edit?usp=sharing"
slate_filter = c("Main")

# Install Packages and Link Filepaths ----------
library(easypackages)
packages("plotly", "jsonlite","googlesheets4", "odds.converter")
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
week_folder = paste0(projects_folder, "NFL 2021/Week ", 
                     if_else(current_week < 10, "", ""), 
                     as.character(current_week))
downloads_folder = paste0(dropbox, "Matt Savoca/Droploads/")

 
## current  QBs

current_qbs = c(
  "Dak Prescott",
  "Carson Wentz",
  "Daniel Jones",
  "Jalen Hurts",
  "Aaron Rodgers",
  "Jared Goff",
  "Justin Fields",
  "Kirk Cousins",
  "Tom Brady",
  "Matt Ryan",
  "Jameis Winston",
  "Matthew Stafford",
  "Kyler Murray",
  "Drew Lock",
  "Trey Lance",
  "Jimmy Garoppolo",
  "Mac Jones",
  "Zack Wilson",
  "Josh Allen",
  "Tua Tagovailoa",
  "Lamar Jackson",
  "Joe Burrow",
  "Deshaun Watson",
  "Mitchell Trubisky",
  "Davis Mills",
  "Trevor Lawrence",
  "Ryan Tannehill",
  "Patrick Mahomes",
  "Russell Wilson",
  "Derek Carr",
  "Justin Herbert",
  "Sam Ehlinger",
  "Sam Darnold"
)

plan("multisession")



nfl_pbp = nflreadr::load_pbp(
  seasons = 1999:most_recent_season(),
)

nfl_rosters = load_rosters(
  seasons = 1999:most_recent_season()) %>%
  mutate(active_status = if_else(
    season == most_recent_season() & status %in% c("ACT","Active"), 1, 0)) %>%
  select(season, full_name, position, gsis_id)

hof_qbs = c("Peyton Manning", "Drew Brees", "Philip Rivers", "Ben Roethlisberger", "Eli Manning")

nfl_pbp %>% pull(drive_end_transition) %>% unique()

qb_drives = nfl_pbp %>%
  mutate(
    drive_success = case_when(
      drive_end_transition == "TOUCHDOWN" ~ 1,
      (drive_end_transition == "END GAME" & posteam_score > defteam_score) ~ 1,
      T ~ 0
    ),
    game_date = as.Date(game_date, formate = "%Y-%m-%d")
  ) %>% 
  group_by(game_id, drive, passer_id) %>%
  mutate(player_drive_dropbacks = sum(qb_dropback)) %>%
  filter(!is.na(drive), !is.na(passer_id), !is.na(passer_player_name)) %>%
  group_by(game_id, drive) %>%
  mutate(total_drive_dropbacks = sum(qb_dropback)) %>% 
  rowwise() %>%
  mutate(player_dropback_rate = player_drive_dropbacks/total_drive_dropbacks) %>% #glimpse()
  ungroup() %>%
  select(
    game_id, game_date, drive, season,
    gsis_id = passer_id,
    player_drive_dropbacks,
    total_drive_dropbacks,
    player_dropback_rate,
    drive_success) %>%
  filter(!is.nan(player_dropback_rate)) %>%
  distinct() %>%
  left_join(nfl_rosters)



# Per Season TDSR -----

qb_tdsr_season = qb_drives %>%
  filter(position == "QB", player_dropback_rate > .5) %>%
  group_by(full_name, season) %>%
  summarize(
    dropbacks = sum(player_drive_dropbacks),
    drives = n(),
    successes = sum(drive_success)) %>%
  group_by(full_name) %>%
  mutate(latest_season = max(season)) %>%
  ungroup() %>%
  filter(dropbacks > 50) %>%
  add_ebb_estimate(successes, drives) %>%
  arrange(-.fitted)


# %>%
#   arrange(desc(game_date), -drive, passer_player_name) %>%
#   group_by(passer_player_name, passer_id) %>%
#   mutate(
#     drive_recency = cumsum(qb_dropback)
#   ) %>%
#   ungroup()
ggplotly(
  qb_tdsr_season %>%
    arrange(-.fitted) %>%
    filter((latest_season == 2021 
           | full_name %in% hof_qbs
           ),dropbacks > 100
           ) %>%
    mutate(
      season_color = if_else(season == 2021, "red", "black"),
      #full_name = paste0(full_name, " ", season),
      full_name = reorder(full_name, .fitted)) %>%
    group_by(full_name) %>%
    mutate(mean_tdsr = mean(.fitted, na.rm = T),
           low_tdsr = min(.low, na.rm = T),
           high_tdsr = max(.high, na.rm = T)) %>%
    ggplot()+
    aes(xmin = low_tdsr, xmax = high_tdsr, x = .fitted, y = full_name)+
    geom_errorbarh(height = 0, alpha = .3)+
    geom_point(alpha = .9, aes(color = season_color))+
    geom_point(aes(x = mean_tdsr, y = full_name), color = "black", alpha = .7, size = 3)+
    theme_FE+
    scale_color_identity()+
    scale_x_continuous(labels = percent_format())+
    labs(x = "Bayesian Estimated Drive Success Rate",
         y = "",
         title = "How QBs stacked-up against the HOFers in 2021",
         subtitle = "2021 Seasons in Blue")+
    theme(legend.position = 'none')
)

# Per Career TDSR -----

qb_tdsr_career = qb_drives %>%
  filter(position == "QB", player_dropback_rate > .5) %>%
  group_by(full_name) %>%
  summarize(
    dropbacks = sum(player_drive_dropbacks),
    drives = n(),
    successes = sum(drive_success),
    latest_season = max(season)) %>%
  ungroup() %>%
  filter(dropbacks > 50) %>%
  add_ebb_estimate(successes, drives) %>%
  arrange(-.fitted)


# %>%
#   arrange(desc(game_date), -drive, passer_player_name) %>%
#   group_by(passer_player_name, passer_id) %>%
#   mutate(
#     drive_recency = cumsum(qb_dropback)
#   ) %>%
#   ungroup()
ggplotly(
  qb_tdsr_career %>%
    arrange(-.fitted) %>%
    filter((full_name %in% current_qbs
            | full_name %in% "Deshaun Watson"
            | full_name %in% hof_qbs
    ), dropbacks > 100
    ) %>%
    mutate(
      season_color = case_when(
        full_name == "Ben Roethlisberger" ~ "red",
        latest_season != 2021 & full_name != "Deshaun Watson" ~ "red", 
        T ~ "black"),
      #full_name = paste0(full_name, " ", season),
      full_name = reorder(full_name, .fitted)) %>%
    ggplot()+
    aes(xmin = .low, xmax = .high, x = .fitted, y = full_name)+
    geom_errorbarh(aes(color = season_color), height = 0)+
    geom_point()+
    geom_point(aes(x = .raw),  color = "red", size =.7)+
    #geom_point(aes(x = mean_tdsr, y = full_name), color = "black", alpha = .7, size = 3)+
    theme_FE+
    scale_color_identity()+
    scale_x_continuous(labels = percent_format())+
    labs(x = "Bayesian Estimated Drive Success Rate",
         y = "",
         title = "Quarterback Drive Efficiency with HOF QBs Included",
         subtitle = "2021 Seasons in Blue")+
    theme(legend.position = 'none')
)

