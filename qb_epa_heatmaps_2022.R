# Get Week and Weekly Sheet -----------
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



### Passer Comparison =--------


passer_compares = nfl_pbp %>%
  filter(passer_player_name %in% 
           c("M.Ryan" ,"C.Wentz", "P.Mahomes", "J.Allen", "J.Goff",
             "M.Stafford"), !is.na(passer_id)) %>%
  pull(passer_id) %>%
  unique()

full_names = nfl_rosters %>%
  filter(season == 2020 & gsis_id %in% passer_compares) %>%
  rename(passer_id = gsis_id) %>%
  select(-season)
  

passer_compare_df = nfl_pbp %>% 
  left_join(full_names) %>%
  filter(
    season %in% 2019:2020,
    !is.na(full_name),
    passer_id %in% passer_compares,
    !is.na(air_yards))
  
# passer_compare_df %>%
#   ggplot(aes(x = air_yards, fill = full_name))+
#   geom_density(alpha = .4)+
#   scale_y_continuous(labels = percent_format())+
#   labs(
#     fill = ""
#     )+
#   theme_FE



  
create_pass_epa_card = function(df){
  df %>%
    select(full_name, air_yards, epa, pass_location) %>%
    filter(!is.na(air_yards), 
           !is.na(pass_location)
           ) %>%
    mutate(
      depth_tier = case_when(
        air_yards < 1 ~ 1,
        air_yards < 10 ~ 2,
        air_yards < 20 ~ 3,
        T ~ 4
      ),
      pass_depth = case_when(
        air_yards < 1 ~ "< 1",
        air_yards < 10 ~ "1-10",
        air_yards < 20 ~ "10-20",
        T ~ ">20"
      )
    ) %>%
    group_by(full_name, pass_depth, pass_location, depth_tier) %>%
    summarize_if(is.numeric, mean, na.rm =T) %>%
    ungroup() %>%
    arrange(depth_tier)
}



create_pass_depth_epa_card = function(df){
  df %>%
    select(full_name, air_yards, epa) %>%
    filter(!is.na(air_yards), 
    ) %>%
    mutate(
      depth_tier = case_when(
        air_yards < 1 ~ 1,
        air_yards < 10 ~ 2,
        air_yards < 20 ~ 3,
        T ~ 4
      ),
      pass_depth = case_when(
        air_yards < 1 ~ "< 1",
        air_yards < 10 ~ "1-10",
        air_yards < 20 ~ "10-20",
        T ~ ">20"
      )
    ) %>%
    group_by(full_name, pass_depth, depth_tier) %>%
    summarize_if(is.numeric, mean, na.rm =T) %>%
    ungroup() %>%
    arrange(depth_tier)
}




create_league_epa_card = function(df){
  df %>%
    select(air_yards, epa, pass_location) %>%
    filter(!is.na(air_yards), 
           !is.na(pass_location)
    ) %>%
    mutate(
      depth_tier = case_when(
        air_yards < 1 ~ 1,
        air_yards < 10 ~ 2,
        air_yards < 20 ~ 3,
        T ~ 4
      ),
      pass_depth = case_when(
        air_yards < 1 ~ "< 1",
        air_yards < 10 ~ "1-10",
        air_yards < 20 ~ "10-20",
        T ~ ">20"
      )
    ) %>%
    group_by(pass_depth, pass_location, depth_tier) %>%
    summarize_if(is.numeric, mean, na.rm =T) %>%
    ungroup() %>%
    arrange(depth_tier)
}


create_league_depth_epa_card = function(df){
  df %>%
    select(air_yards, epa) %>%
    filter(!is.na(air_yards)) %>%
    mutate(
      depth_tier = case_when(
        air_yards < 1 ~ 1,
        air_yards < 10 ~ 2,
        air_yards < 20 ~ 3,
        T ~ 4
      ),
      pass_depth = case_when(
        air_yards < 1 ~ "< 1",
        air_yards < 10 ~ "1-10",
        air_yards < 20 ~ "10-20",
        T ~ ">20"
      )
    ) %>%
    group_by(pass_depth, depth_tier) %>%
    summarize_if(is.numeric, mean, na.rm =T) %>%
    ungroup() %>%
    rename(league_depth_epa = epa) %>%
    select(-air_yards) %>%
    arrange(depth_tier)
}

nfl_avg_pass_epa_df = nfl_pbp %>%
  filter(season %in% 2009:2020) %>%
  create_league_epa_card() %>%
  rename(
    avg_air_yards = air_yards,
    avg_epa = epa
  )


nfl_avg_depth_epa_df = nfl_pbp %>%
  filter(season %in% 2009:2020) %>%
  create_league_depth_epa_card()


QB1 = "Carson Wentz"
QB2 = "Matt Ryan"

dir_compare_df = passer_compare_df %>%
  filter(!is.na(air_yards), 
         !is.na(pass_location),
         full_name %in% c(QB2, QB1)) %>%
  select(full_name, air_yards,
         pass_location,
         epa) %>%
  create_pass_epa_card() 


depth_compare_df = passer_compare_df %>%
  filter(!is.na(air_yards), 
         !is.na(pass_location),
         full_name %in% c(QB2, QB1)) %>%
  select(full_name, air_yards,
         epa) %>%
  create_pass_depth_epa_card() %>%
  select(-air_yards) %>%
  rename(ovr_epa = epa)


qb_compare_table_df = dir_compare_df %>%
  left_join(depth_compare_df)




qb_compare_table_df %>%
  left_join(nfl_avg_pass_epa_df) %>%
  left_join(nfl_avg_depth_epa_df) %>% 
  select(-air_yards, -avg_air_yards) %>%
  mutate(epa_plusminus = epa-avg_epa,
         ovr_plusminus = ovr_epa - league_depth_epa) %>%
  select(full_name, 
         pass_depth,
         pass_location,
         epa_plusminus,
         ovr_plusminus) %>%
  pivot_wider(
    names_from = c(pass_location),
    values_from = c(epa_plusminu)
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  group_by(full_name) %>%
  gt() %>%
  tab_header(title = "QB Comparison:
             Efficiency vs. League Avg. by Direction & Depth")  %>%
  cols_label(
    pass_depth = "",
    left = "Left",
    middle = "Middle",
    right = "Right")


