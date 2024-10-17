qb_epa_percentile_df = plays %>%
  generate_primary_passers() %>% #glimpse()
  filter(season >= 2016, play_type %in% c("pass"), season_type == "REG") %>%
  select(game_id, play_id, season, week, posteam, play_type, posteam_primary_passer_id, epa, name, qb_dropback) %>% 
  group_by(posteam_primary_passer_id) %>%
  summarize(
    recent_year = max(season),
    passer_career_epa = mean(epa, na.rm = T),
    dropbacks = sum(qb_dropback, na.rm = T),
    .groups = "drop"
  ) %>%
  left_join(rosters %>% select(player = full_name, posteam_primary_passer_id = gsis_id, recent_year = season)) %>%
  filter(dropbacks > 100) %>%
  select(
    posteam_primary_passer_id,
    primary_qb = player, 
    passer_career_epa
  ) %>%
  mutate(
    passer_career_epa_percentile = round((percent_rank(passer_career_epa)*100)/10)*10
  ) %>%
  arrange(-passer_career_epa) 



personnel = partic %>%  
  select(game_id = nflverse_game_id, play_id, off_formation_code)


formation_plot_df = plays %>%
    generate_primary_passers() %>% 
    mutate(off_pts_scored = posteam_score_post - posteam_score) %>%
    filter(season >= 2016, play_type %in% c("pass"), season_type == "REG", aborted_play == 0) %>%
    left_join(qb_epa_percentile_df %>% select(posteam_primary_passer_id, passer_career_epa_percentile)) %>%
    left_join(personnel) %>%
    select(play_id, off_formation_code, passer_career_epa_percentile, ep, epa, off_pts_scored) %>%
    mutate(passer_career_epa_percentile = cut(passer_career_epa_percentile, breaks = 3, labels = c("Bottom", "Middle", "Top"))) %>% #as.character(seq(10, 100, 10)))) %>%
    filter(!is.na(off_formation_code)) %>%
    group_by(off_formation_code) %>%
    mutate(
      off_formation_code_n = n(),
      off_formation_code_ep = sum(ep, na.rm = T),
      off_formation_code_epa = sum(epa, na.rm = T),
      off_formation_code_pts = sum(off_pts_scored, na.rm = T),
      off_formation_code_ep_rounded = off_formation_code_ep %>% round(0),
      off_formation_code_epa_rounded = off_formation_code_epa %>% round(0)
    ) %>%
    ungroup() %>% #View()
    #mutate(ep_rounded = round(ep)) %>%
    filter(!is.na(passer_career_epa_percentile)) %>% #View()
    filter(off_formation_code_n > 50, !is.na(passer_career_epa_percentile))
    
    
    

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

formation_plot_df %>% #View()
  select(contains(c("off_formation")), off_formation_code_pts) %>%
  distinct() %>%
  filter(off_formation_code_pts > off_formation_code_n) %>% 
  mutate(
    scaled_epa = round(normalize(off_formation_code_epa_rounded) * 100)
  ) %>%
  add_ebb_estimate(off_formation_code_n, off_formation_code_ep_rounded) %>%
  add_ebb_estimate(scaled_epa, off_formation_code_n) %>%
  rename(true_ep = .fitted...10, true_epa = .fitted...16) %>%
  mutate(true_ep = 1/true_ep %>% round(2),
         true_epa = round(true_epa, 3)) %>%
  select(contains("off_formation_code"), true_ep, true_epa) %>%
  glimpse()

p = formation_plot_df %>%
  group_by(off_formation_code, passer_career_epa_percentile) %>%
  summarize(off_formation_code_pts = sum(off_pts_scored, na.rm = T),
            off_formation_code_n = n(),
            off_formation_code_pts_play = off_formation_code_pts/off_formation_code_n) %>%
  group_by(off_formation_code) %>%
  mutate(off_formation_total_n = sum(off_formation_code_n)) %>%
  ungroup() %>%
  mutate(off_formation_code = reorder(off_formation_code, off_formation_code_pts_play)) %>%
  select(passer_career_epa_percentile, off_formation_code, off_formation_code_pts, off_formation_code_n, off_formation_code_pts_play, off_formation_total_n) %>%
  distinct() %>% 
  filter(off_formation_total_n > 50) %>% #View()
  ggplot() +
  aes(x= passer_career_epa_percentile, y = off_formation_code)+
  geom_tile(aes(fill = off_formation_code_pts_play),  linewidth = .33, show.legend = T, linejoin = "round") +
  scale_fill_gradient2(name = "Pts/Play", low= "red",high = "#4aac5a", na.value = "grey") +
  theme_light()+
  labs(y = "Offensive Personnel", x = "Primary QB Career Efficiency Tier (By EPA)",
       title = "With a Mid QB, Play More TE",
       subtitle = "2 & 3 TE sets dominate Pts. Scored Per Play since 2016, Especially for Mid-Tier QBs",
       caption = "Chart: Matt Savoca       @draftaholic       Data:nflreadR")+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "white"), 
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    #axis.text.x = element_text(hjust=-.35),
    axis.text = element_markdown(hjust = 0, size = 11, family = "Franklin Gothic Medium", color = "black"), 
    axis.title = element_text(size = 14, family = "Franklin Gothic Medium", face = "bold"), 
    title = element_text(size = 16, family = "Franklin Gothic Medium"), 
    plot.caption = element_text(color = "grey", size = 11, hjust = 1),
    legend.key.width = unit(.15, units = "inches"),
    legend.title = element_text(size = 9, color = "black"),
    legend.text = element_text(size = 8, color = "darkgrey")
  )

p
  
ggsave("formation_plot.png", p, scale = .5)



  #geom_nfl_headshots(aes(player_gsis = player_id, color = hs_color), x = -.1, height = .03)+
  #geom_point(aes(color = circle_color), shape = 21, size = 8, x = -.1, stroke = 0.5) +
  #geom_nfl_wordmarks(aes(team_abbr = present_team), width = .05, height = 1.5, x = -1.5)+
  #geom_nfl_wordmarks(aes(team_abbr = most_recent_team), width = .05, height = 1.5, x = -2.9)+
  #scale_x_continuous(breaks = seq(1, 18, 1), labels = 1:18, position = "top") +
  #scale_color_identity()+
  #geom_hline(yintercept = FA_line+.5)
