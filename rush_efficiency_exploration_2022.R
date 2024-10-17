options(tidyverse.quiet = TRUE)
options(nflreadr.verbose = FALSE)
# Helper Functions, Filepaths & Chart Themes -----
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/NFL 2022/sim_projections_helpers.R"))
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
theme_set(theme_FE)



nfl_pbp_raw = load_pbp(seasons = 1999:2021) 

nfl_pbp_raw %>%
  filter(rush_attempt == 1 & qb_scramble == 0, !is.na(down)) %>%
  mutate(down = as.factor(down)) %>%
  ggplot()+
  aes(
    x = yardline_100,
    y = epa,
    color = down,
    group = down
  ) +
  geom_smooth(se = F) +
  scale_x_reverse(breaks = seq(100, 0, -10))+
  geom_hline(yintercept = 0, lty = 2, size = 2)+
  labs(
    x = "Yards from Opponent End-Zone",
    y = "Expected Points Added",
    title = "League-Wide Rush Efficiency by Down and Distance to GL"
  )



nfl_pbp_raw %>%
  filter(rush_attempt == 1 & qb_scramble == 0, !is.na(down)) %>%
  mutate(down = as.factor(down),
         field_zone = cut(yardline_100, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))) %>%
  group_by(field_zone, down, ydstogo) %>%
  summarize(
    ep = mean(ep, na.rm =T)
  ) %>%
  ggplot()+
  aes(
    x = ydstogo,
    y = ep,
    fill = down,
    group = down
  ) +
  geom_col(position = "dodge")+
  facet_wrap(field_zone ~ ., nrow = 2)+
  coord_cartesian(xlim = c(0, 20))+
  labs(
    x = "Yards from Opponent End-Zone",
    y = "Expected Points",
    title = "Rushing Expected Points by Down and Distance to 1D, Grouped by Field Position"
  )
 


situational_rush_df = nfl_pbp_raw %>%
  filter(rush_attempt == 1 & qb_scramble == 0, !is.na(down), qb_kneel == 0) %>%
  mutate(down = as.factor(down),
         field_zone = cut(yardline_100, 
                          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 100),
                          labels = c(
                            "Inside the 10",
                            "Inside the 20",
                            "21-30",
                            "31-40",
                            "41-50",
                            "51-60",
                            "61-70",
                            "71-99"
                          )),
         ydstogo_zone = case_when(
           ydstogo <= 2 ~ "<3",
           ydstogo < 5 ~ "<5",
           ydstogo < 11 ~ "5-10",
           T ~ "10+",
         ),
         ydstogo_zone = reorder(ydstogo_zone, ydstogo)
  ) %>%
  group_by(down, field_zone, ydstogo_zone) %>%
  summarize(
    epa = mean(epa, na.rm =T),
    n = n()
  )



situational_rush_df %>% 
  filter(n >= 50) %>%
  ggplot()+
  aes(
    x = ydstogo_zone,
    y = epa,
    fill = down,
    group = down
  ) +
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_wrap(field_zone ~ ., nrow = 2)+
  labs(
    x = "Yards from First Down",
    y = "Expected Points Added",
    title = "Rushing Expected Points Added by Down and Distance to 1D, Grouped by Field Position"
  )+
  theme_light()



ggplotly(
  nfl_pbp_raw %>%
    filter(rush_attempt == 1 & qb_scramble == 0, !is.na(down), qb_kneel == 0) %>%
    mutate(down = as.factor(down),
           field_zone = cut(yardline_100, 
                            breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 100),
                            labels = c(
                              "Inside the 10",
                              "Inside the 20",
                              "21-30",
                              "31-40",
                              "41-50",
                              "51-60",
                              "61-70",
                              "71-99"
                            )),
           ydstogo_zone = case_when(
             ydstogo <= 2 ~ "<3",
             ydstogo < 5 ~ "<5",
             ydstogo < 11 ~ "5-10",
             T ~ "10+",
           ),
           ydstogo_zone = reorder(ydstogo_zone, ydstogo)
    ) %>%
    filter(season >= 2019) %>%
    group_by(rusher, rusher_id, field_zone, down, ydstogo_zone) %>%
    summarize(
      att = n(),
      obs_epa = sum(epa)
    ) %>%
    ungroup() %>%
    left_join(
      situational_rush_df
    ) %>%
    rowwise() %>%
    mutate(x_epa = epa * att) %>%
    group_by(rusher, rusher_id) %>%
    summarize(
      att = sum(att),
      x_epa = mean(x_epa),
      obs_epa = sum(obs_epa)) %>%
    arrange(-att) %>%
    ggplot()+
    aes(x = att,y = x_epa) +
    geom_point(aes(label = rusher))+
    geom_point(aes(x = att,y = obs_epa, label = rusher), color = "green")
)




nfl_pbp_raw %>%
  filter(rush_attempt == 1 & qb_scramble == 0, !is.na(down), qb_kneel == 0) %>%
  mutate(down = as.factor(down),
         field_zone = cut(yardline_100, 
                          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 100),
                          labels = c(
                            "Inside the 10",
                            "Inside the 20",
                            "21-30",
                            "31-40",
                            "41-50",
                            "51-60",
                            "61-70",
                            "71-99"
                          )),
         ydstogo_zone = case_when(
           ydstogo <= 2 ~ "<3",
           ydstogo < 5 ~ "<5",
           ydstogo < 11 ~ "5-10",
           T ~ "10+",
         ),
         ydstogo_zone = reorder(ydstogo_zone, ydstogo)
  ) %>%
  filter(season == 2021) %>%
  group_by(rusher, rusher_id) %>%
  count() %>%
  arrange(-n)
