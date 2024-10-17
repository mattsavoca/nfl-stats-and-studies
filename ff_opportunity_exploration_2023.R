#install.packages("nflplotR", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
install.packages("easypackages")
source("helpers.R")
#install.packages("showtext")
#install.packages("sysfonts")
#install.packages("extrafont")
#install.packages("ggtext")
devtools::install_github("dgrtwo/ebbr")
library(ebbr)
library(showtext)
library(sysfonts)
library(extrafont)
library(ggtext)
library(nflplotR)
library(nflreadr)
font_add_google("Source Sans Pro")
font_add_google("Roboto")
#font_import(prompt = F)
loadfonts()

SEASONS = 2009:2022

## Player Opportunities-----

ff_opp = nflreadr::load_ff_opportunity(seasons = SEASONS)


ff_opp %>%
  mutate(season = as.numeric(season)) %>%
  left_join(weekly %>% select(player_id, targets, season, week)) %>%
  filter(season > 2017, position %in% c("WR"),
         (week < 18 | (week == 18 & season > 2020))) %>%
  group_by(player_id) %>%
  summarize(
    full_name = full_name[1],
    targets = sum(targets, na.rm = T),
    receptions = sum(receptions, na.rm = T),
    fp_trg = (sum(rec_fantasy_points, na.rm = T)-.5*receptions)/targets,
  ) %>%
  filter(!is.infinite(fp_trg), targets >= 200) %>%
  arrange(-fp_trg) %>% 
  View() 

xfp_df = ff_opp %>% filter(
  season < 2022,
  position %in% c("QB")
) %>%
  select(
    season, posteam, week, player_id, full_name, position,
    total_fantasy_points_exp, total_fantasy_points, total_fantasy_points_exp_team, total_fantasy_points_team,
  ) %>%
  group_by(
    season, player_id
  ) %>%
  summarize(
    full_name = full_name[1],
    position = position[1],
    games = n(),
    xfp_game = mean(total_fantasy_points_exp, na.rm = T),
    xfp_share = sum(total_fantasy_points_exp, na.rm = T)/sum(total_fantasy_points_exp_team,na.rm = T),
    fp_game = mean(total_fantasy_points, na.rm = T),
    fp_share = sum(total_fantasy_points, na.rm = T)/sum(total_fantasy_points_team,na.rm = T),
    fpoe = (sum(total_fantasy_points) - sum(total_fantasy_points_exp))/games,
    team_fpoe = (sum(total_fantasy_points_team) - sum(total_fantasy_points_exp_team))/games,
    fpoe_share = fpoe/team_fpoe,
    .groups = "drop"
  ) %>%
  mutate(
    season_n1 = as.character(as.numeric(season) + 1)
  )


xfp_n1_df = xfp_df %>% select(season = season_n1, player_id) %>%
  left_join(
    xfp_df %>% select(-season_n1)
  ) %>%
  mutate(season = as.character(as.numeric(season)- 1)) %>%
  rename_with(~paste0(., "_n1"), contains(c("game", "share", "fpoe"))) %>%
  select(-full_name, -position)

xfp_full_df = 
  xfp_n1_df %>% left_join(
    xfp_df %>% select(-season_n1)
  )


xfp_full_df %>%
  ggplot()+
  aes(
    xfp_share,
    xfp_share_n1
  ) + 
  geom_jitter()+
  geom_smooth(method = "glm")



summary(lm(xfp_share_n1 ~ xfp_share, data = xfp_full_df))

#ff_opp %>% glimpse()

## Player Participation-----
partic_raw = load_participation(seasons = 2016:2023)


## All Plays to Weekly Stats -----

plays = load_pbp(seasons = 2009:2023)


## Partic ----
partic = partic_raw %>%
  mutate(
    two_qb = if_else(str_starts(offense_personnel,"2 QB,"),1,0),
    six_ol = if_else(str_detect(offense_personnel,"6 OL,"),1,0),
    n_ol = if_else(six_ol == 1, 6, 5 - (11-n_offense)),
    n_qb = if_else(two_qb == 1, 2, 1),
    n_skill = n_offense - n_qb - n_ol,
    n_rb = str_extract(offense_personnel, "[0-9]+ RB") %>% parse_number(),
    n_wr = str_extract(offense_personnel, "[0-9]+ WR") %>% parse_number(),
    n_te = str_extract(offense_personnel, "[0-9]+ TE") %>% parse_number(),
    multi_te = if_else(n_te > 1, 1, 0), 
    off_formation_code = if_else(is.na(n_qb), NA_character_, paste0(as.character(n_rb), as.character(n_te)))
  )

multi_te_df = partic %>%
  left_join(plays %>% rename(nflverse_game_id = game_id) %>% select(nflverse_game_id, play_id, season, play_type)) %>%
  filter(play_type == "pass") %>%
  group_by(possession_team, season) %>%
  summarize(
    plays = n(),
    multi_te = sum(multi_te, na.rm =T),
    pct_multi_te = multi_te/plays
  ) %>%
  ungroup() %>% 
  filter(season == 2022) %>%
  arrange(-pct_multi_te) %>%
  select(
    Team = possession_team, 
    `2+ TE Snaps` = multi_te,
    `% Snaps` = pct_multi_te
  ) 

multi_te_df %>%
  top_n(n = 10, wt = `2+ TE Snaps`) %>%
  gt() %>%
  gt_nfl_wordmarks(columns = gt::starts_with("team")) %>%
  gt::fmt_percent(columns = `% Snaps`, decimals = 1) %>%
  tab_header(
    title = "2+ TE Set Passing Leaders",
    subtitle = "2022 Season"
  )
  
  

#partic %>% glimpse()

## Player Weekly Stats-----

weekly = nflreadr::load_player_stats(seasons = SEASONS)

quick_fp_to_nflfastr = function(x){
  nm = gsub(pattern = "DJ Chark Jr.", "D.J. Chark", x)
  nm = gsub(pattern = "Richie James Jr.", "Richie James", nm)
  nm = gsub(pattern = "Michael Pittman Jr.", "Michael Pittman", nm)
  nm = gsub(pattern = "Joshua Palmer", "Josh Palmer", nm)
  nm = gsub(pattern = "DJ Moore", "D.J. Moore", nm)
  nm = gsub(pattern = "Allen Robinson II", "Allen Robinson", nm)
  nm = gsub(pattern = "Marvin Jones Jr.", "Marvin Jones", nm)
  return(nm)
}

weekly %>%
  group_by(player_id) %>%
  mutate(
    player_n_seasons = 1+max(season, na.rm = T)-min(season, na.rm = T)
  ) %>%
  filter(
    season == 2022 |
    (player_display_name %in% c("Calvin Ridley", "Odell Beckham Jr.") & season == 2021)) %>%
  select(
    contains(c("player","position")), recent_team, season, week, yac = receiving_yards_after_catch, ay = receiving_air_yards,targets) %>%
  group_by(season, player_id) %>%
  summarize(
    games = n(),
    player = player_display_name[1],
    player_n_seasons = player_n_seasons[1],
    prime_distance = abs(5 - (player_n_seasons + 1)),
    team = recent_team[1],
    position = position[1],
    targets = sum(targets),
    adot = sum(ay)/targets,
    yac = sum(yac),
    targets_game = targets/games,
    yac_target = yac/targets,
    .groups = "drop"
  ) %>%
  left_join(ff_ranks %>% 
              rename(fp_team = team) %>%
              mutate(player = quick_fp_to_nflfastr(player))
            ) %>%
  arrange(-yac_target) %>% 
  filter(
    (targets > 50 | targets_game > 5.5), 
    position == "WR", 
    (adot > 8 | yac_target > 2),
    ecr > 60, fp_team != "FA"
    #prime_distance < 4
    ) %>% 
  top_n(30, yac_target) %>%
  mutate(
    player = paste0(player, " (", fp_team, ")"),
    player = reorder(player, -ecr)) %>%
  ggplot()+
  aes(
    yac_target, player, fill = prime_distance
  ) +
  geom_col()

#weekly %>% glimpse()


## Team Roster Info -----
rosters = load_rosters(seasons = SEASONS)
teams = load_teams()

#rosters %>% glimpse()

## Player Info ----
players = nflreadr::load_players()
# Free Agency Updates ----
fa_changes_raw = fa_changes = read_csv("./data/2023 FA Skill Changes.csv") %>% clean_names() %>%
  mutate(
    pos = if_else(adj_position != pos, adj_position, pos),
    player = if_else(adj_name != player, adj_name, player)
  ) %>%
  select(-adj_name, -adj_position) 



fa_changes_df = fa_changes_raw %>%
  left_join(teams %>% select(x2022_team = team_nick, x22_posteam = team_abbr)) %>%
  left_join(teams %>% select(x2023_team = team_nick, x23_posteam = team_abbr)) %>%
  left_join(players %>% filter(status != "RET") %>% select(player = display_name, pos = position, player_id = gsis_id)) %>%
  filter(!is.na(player_id))

#fa_changes_df



# Current Year ADP
ff_ranks = ffs_latest_rankings()

p = rosters %>%  
  group_by(gsis_id) %>%
  mutate(n_season = n()+1) %>%
  ungroup() %>%
  filter(
    season == 2023 | 
      full_name %in% c("") & season == 2022
    ) %>%
  right_join(ff_ranks, by = "sportradar_id") %>% 
  select(
    player, pos, ecr, team = team.y, sportradar_id, gsis_id, birth_date, n_season
  ) %>%
  filter(
    pos %in% c("RB", "WR") & ecr < 100 | pos %in% c("QB", "TE") & ecr < 45
  ) %>% 
  mutate(
    age =  as.numeric(interval(birth_date, today()) / years(1)),
    n_season = replace_na(n_season, 1), #lol bruteforce
    est_age = replace_na(age, 22),
    prime_distance = case_when(
      pos == "RB" ~ abs(2-n_season),
      pos == "WR" ~ abs(5-n_season),
      T ~ abs(27-age)
    ) 
  ) %>%
  ggplot()+
  aes(
    x = ecr, y = prime_distance, label = player, fill = pos
  ) +
  #geom_nfl_logos(aes(team_abbr = team))+
  geom_jitter()+
  scale_y_log10()+
  coord_cartesian(ylim = c(-1,15))

ggplotly(p)

# off_play_types = c("pass", "run", "qb_spike", "qb_kneel")
# 
# plays %>% 
#   filter(play_type %in% off_play_types) %>%
#   group_by(game_id, team = posteam) %>%
#   summarize(
#     team_plays = n(),
#     week = week[1]) %>%
#   ungroup() %>%
#   glimpse()


# True Opp Share, 2022 --------

ebbr_rename <- function(data, prefix = "stat") {
  new_names <- map(names(data), ~if (startsWith(.x, ".")) str_replace(.x, "\\.", paste0(prefix, ".")) else .x)
  names(data) <- unlist(new_names)
  return(data)
}


# ff_opp %>%
#   select(player_id, game_id, season, week, posteam, position,
#          contains(c("_attempt", "_first_down", "touchdown")),
#          -ends_with("_exp")) %>%
#   mutate() %>%
#   add_ebb_estimate(opps, team_opps) %>%
#   ebbr_rename("opps") %>%
#   glimpse()




# FP/RR -------


p_df = weekly %>%
  left_join(ff_opp %>% mutate(season = as.integer(season)) %>% select(-position), by = c("player_id", "season", "week")) %>% #glimpse()
  filter(position %in% c("WR"), !is.na(player_display_name)) %>%
  group_by(player_id, season) %>%
  summarize(
    player = player_display_name[1],
    position = position[1],
    games = n(),
    targets = sum(targets),
    target_share = mean(target_share, na.rm = T),
    xfp_trg = sum(total_fantasy_points_exp)/sum(targets),
    fp_trg = sum(total_fantasy_points)/sum(targets)) %>%
  mutate(xfp_trg = if_else(xfp_trg == Inf, 0, xfp_trg)) %>%
  ungroup() %>% 
  arrange(-xfp_trg) %>%
  mutate(last_season = if_else(season == 2022, "red", "grey"),
         last_season = if_else(player == "Cooper Kupp", "yellow", last_season)
  ) %>%
  filter(target_share > .15, targets > 25) %>% 
  ebbr::add_ebb_estimate(games, targets) #%>% 
  # mutate(
  #   fitted = 1/.fitted,
  #   raw = 1/.raw,
  #   high = 1/.high,
  #   low = 1/.low
  # )

p = p_df %>%
  ggplot()+
    aes()+
    aes(targets, fp_trg, color = last_season, label = paste0(player, ", ", season))+
    scale_color_identity()+
    geom_vline(aes(xintercept = mean(targets))) + 
    geom_hline(aes(yintercept = mean(xfp_trg, na.rm = T))) +
    geom_jitter()+
    geom_smooth(color = "blue", method = "glm", se = F)+
    labs(title = "FP_Trg")


ggplotly(p)

p_df %>%
  pull(fp_trg) %>%
  quantile(probs = c(0.005, .995), na.rm = T)


p1 = weekly %>%
  left_join(ff_opp %>% mutate(season = as.integer(season)) %>% select(-position), by = c("player_id", "season", "week")) %>%
  filter(position %in% c("WR"), !is.na(player_display_name)) %>%
  group_by(player_id, season) %>%
  summarize(
    player = player_display_name[1],
    position = position[1],
    games = n(),
    targets = sum(targets),
    target_share = mean(target_share, na.rm = T),
    xfp_trg = sum(total_fantasy_points_exp)/sum(targets),
    fp_trg = sum(total_fantasy_points)/sum(targets)) %>%
  mutate(xfp_trg = if_else(xfp_trg == Inf, 0, xfp_trg)) %>%
  ungroup() %>% 
  arrange(-xfp_trg) %>%
  mutate(last_season = if_else(season == 2022, "red", "grey"),
         last_season = if_else(player == "Cooper Kupp", "yellow", last_season)
  ) %>%
  filter(target_share > .15, targets > 25) %>% 
  ggplot() +
  aes(xfp_trg)+
  geom_density(aes(y = ..scaled..))+
  scale_y_continuous(labels = percent)

# ggplotly(p1)

# XFP Share Chart ------
all_teams = ff_opp %>% pull(posteam) %>% unique()

xfp_share_chart = function(df, roster_df, team_df, focus_team, fa_changes, pos = c("RB", "WR", "TE", "FB"), season_filter = "2022", baseline_filter = 0.0, suffix = "_test"){
  #if(is.na(team_df)) {team_df = nflreadR::load_teams()}
  suppressMessages({
  
    summary_df = df %>%
      select(
        full_name, player_id, game_id, season, week, posteam, position,
        contains("fantasy_points")
      ) %>%
      filter(!is.na(player_id), week <= 18) %>%
      group_by(season, week, posteam) %>%
      mutate(total_fantasy_points_exp_team = sum(total_fantasy_points_exp)) %>%
      ungroup() %>%
      mutate(
        xfp_share = total_fantasy_points_exp/total_fantasy_points_exp_team
      ) %>%
      group_by(player_id, season) %>% 
      summarize(
        position = position[1],
        max_week = max(week),
        full_name = full_name[1],
        posteam = posteam[1],
        season_xfp = mean(total_fantasy_points_exp, na.rm = T),
        early_season_xfp = mean(total_fantasy_points_exp[week <= 4], na.rm = F),
        late_season_xfp = mean(total_fantasy_points_exp[week >= 12], na.rm = F),
        season_xfp_share = mean(xfp_share, na.rm = T),
        sum_xfp = sum(total_fantasy_points_exp, na.rm = T) %>% round() %>% as.integer(),
        ) %>% 
      ungroup() %>%
      group_by(posteam, season) %>%
      mutate(sum_xfp_team = sum(sum_xfp, na.rm =T) %>% round() %>% as.integer()) %>% 
      ungroup() %>% #View()
      filter(sum_xfp > 0, sum_xfp < sum_xfp_team) %>% #View()
      ebbr::add_ebb_estimate(sum_xfp, sum_xfp_team) %>% 
      #select(-max_week) %>%
      arrange(-season_xfp_share) %>%
      mutate(full_name = reorder(full_name, season_xfp_share)) %>%
      filter(season %in% season_filter, position %in% pos)
     
    recent_team_df = df %>%
      filter(!is.na(player_id), week <= 18) %>%
      group_by(player_id, season) %>%
      summarize(
        position = position[1],
        max_week = max(week),
        most_recent_team = posteam[week==max_week],
        ) %>%
      ungroup() %>%
      select(-max_week)  %>%
      filter(season %in% season_filter, position %in% pos)
    
    changes_df = fa_changes %>% 
      select(player_id, most_recent_team = x22_posteam, present_team = x23_posteam) %>%
      mutate(
        most_recent_team = replace_na(most_recent_team, "FA"),
        present_team = replace_na(present_team, "FA"),
      )
    
      
    summary_df = summary_df %>%
      left_join(recent_team_df) %>%
      left_join(
       changes_df
      ) %>%
      mutate(present_team = if_else(is.na(present_team), most_recent_team, present_team)) %>%
      filter(
        posteam %in% focus_team | most_recent_team %in% focus_team | present_team %in% focus_team
      )
    
    
    weekly_df = df %>% 
      filter(season %in% season_filter) %>%
      mutate(xfp_share = total_fantasy_points_exp/total_fantasy_points_exp_team) %>%
      select(player_id,season, week, posteam, xfp = total_fantasy_points_exp, xfp_share) %>%
      distinct()

    xfp_midpoint = weekly_df %>% pull(xfp) %>% mean(na.rm = T)
    share_midpoint = weekly_df %>% pull(xfp_share) %>% mean(na.rm = T)
    print(paste0("The midpoint for XFP in the sample was ", round(xfp_midpoint,1), " XFP/Game"))
    print(paste0("The midpoint for % Tm. XFP in the sample was ", round(share_midpoint,2)*100, "%"))
    
    crossing_df = crossing(player_id = summary_df$player_id, week = 1:18, season = season_filter)
    
    plot_df = crossing_df %>% 
      left_join(weekly_df) %>% 
      left_join(summary_df %>% select(-posteam)) %>%# View()
      mutate(late_season_xfp = replace_na(late_season_xfp, 0)) %>%
      filter(.fitted >= baseline_filter) %>%
      mutate(
        most_recent_team = if_else(most_recent_team == "FA", NA_character_, most_recent_team),
        present_team = if_else(present_team == "FA", NA_character_, present_team),
        adj_fitted = case_when(
          !most_recent_team %in% focus_team & present_team %in% focus_team  ~ baseline_filter,
          T ~ .fitted),
        hs_color = case_when(
          length(focus_team) > 1 ~ NA,
          present_team != focus_team | is.na(present_team) ~ "b/w", 
          T ~ NA),
        tile_color = case_when(
          length(focus_team) > 1 ~ "white",
          posteam != focus_team ~  "darkgrey", 
          T ~ "white"),
        circle_color = case_when(
          length(focus_team) > 1 ~ NA,
          present_team != focus_team | is.na(present_team) ~ "darkgrey", 
          T ~ "black"),
        ) %>%
      arrange(-adj_fitted) %>%
      mutate(full_name = reorder(full_name, adj_fitted))
      
    
    print(paste0("Plotting ", nrow(plot_df)/18, " players.."))
    
    plot_df %>% View()
    
    
    # Line stuff
    FA_line = plot_df %>% filter(most_recent_team != focus_team & present_team == focus_team) %>% pull(player_id) %>% unique() %>% length()
    
    print(paste0("Adding ", FA_line, " Free Agents..."))
    
    
    # Title Stuff
    title_df = data.frame(team = focus_team) %>%
      left_join(team_df %>% select(team = team_abbr, team_name))
    team_title = if(length(focus_team)>1){""}else{title_df$team_name[1]}
    #print(team_title)
    
    bl_caption = if_else(
      length(focus_team) > 1, "",
      paste0("Grey Outline: Game Played for team other than ", team_title,
             "\n", "All-Grey Square: Player did not play at all.")
    )
    
    # Plot
    suppressWarnings({
      label_y = nrow(plot_df)/18
      
      print(label_y)
      
      plot = plot_df %>%
        ggplot() +
        aes(x= week, y = full_name)+
        geom_tile(aes(fill = xfp_share, color = tile_color),  linewidth = .33, show.legend = T, linejoin = "round") +
        geom_nfl_headshots(aes(player_gsis = player_id, color = hs_color), x = -.1, height = .03)+
        geom_point(aes(color = circle_color), shape = 21, size = 8, x = -.1, stroke = 0.5) +
        geom_nfl_wordmarks(aes(team_abbr = present_team), width = .05, height = 1.5, x = -1.5)+
        geom_nfl_wordmarks(aes(team_abbr = most_recent_team), width = .05, height = 1.5, x = -2.9)+
        scale_fill_gradient(name = "", low= "white", high = "#4aac5a", na.value = "grey", breaks = seq(0, .4, .1), labels = percent) +
        scale_x_continuous(breaks = seq(1, 18, 1), labels = 1:18, position = "top") +
        theme_light()+
        scale_color_identity()+
        geom_hline(yintercept = FA_line+.5)+
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "white"), 
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(hjust=-.35),
          axis.text.y = element_markdown(hjust = 0, face = "bold", size = 8, family = "Franklin Gothic Medium", color = "black"), 
          axis.title.x = element_text(size = 10, family = "Franklin Gothic Medium"), 
          title = element_text(size = 16, family = "Franklin Gothic Medium"), 
          plot.caption = element_text(color = "grey", size = 8, hjust=c(1, 0), vjust = c(0, 0)),
          legend.key.width = unit(.15, units = "inches"),
          legend.title = element_text(size = 9, color = "darkgrey"),
          legend.text = element_text(size = 8, color = "darkgrey")
        ) +
        labs(y = "",
             caption = c("Chart by Matt Savoca    @draftaholic   Data: nflreadR", bl_caption),
             title = paste0("Fantasy Utilization Rate, ", if_else(is.na(team_title), "", team_title), " ", paste(season_filter, collapse = ", "), " Season,"),
             subtitle = paste0("Players with at least ", round_half_up(baseline_filter*100,0), "% of Tm. XFP")
        )+
        coord_cartesian(xlim = c(-3, 17.5))+
        annotate("text", x = -2.9, y = label_y + 0.5, label = "2022 Final Team", size = 2, face = "bold", family = "Franklin Gothic Medium")+
        annotate("text", x = -1.5, y = label_y + 0.5, label = "2023 Team", size = 2, face = "bold", family = "Franklin Gothic Medium")
      
      
      ggsave(paste0("xfp_share_plot",suffix,".png"), plot, width = 12, height = 7)
    })
  })
}





xfp_share_chart(
  ff_opp, #nflreadR::load_ff_opportunity()
  rosters, #load_rosters()
  teams, #load_teams()
  fa_changes = fa_changes_df, 
  focus_team = "NYG", 
  baseline_filter = .005, 
  pos = c("RB", "WR", "TE"), 
  suffix = "NYG22")

xfp_share_chart(ff_opp, rosters, teams, focus_team = "PHI", baseline_filter = .005, pos = c("RB", "WR", "TE"), suffix = "PHI22")
xfp_share_chart(ff_opp, rosters, teams, focus_team = "DAL", baseline_filter = .005, pos = c("RB", "WR", "TE"), suffix = "DAL22")


xfp_share_chart(
  ff_opp, #nflreadR::load_ff_opportunity()
  rosters, #load_rosters()
  teams, #load_teams()
  fa_changes = fa_changes_df, 
  focus_team = all_teams, 
  baseline_filter = .15, 
  pos = c("RB", "WR", "TE"), 
  suffix = "ALL22")

two_teams = all_teams[1:2]

 for (team in all_teams) {
  xfp_share_chart(
    ff_opp, #nflreadR::load_ff_opportunity()
    rosters, #load_rosters()
    teams, #load_teams()
    fa_changes = fa_changes_df, 
    focus_team = team, 
    baseline_filter = .005, 
    pos = c("RB", "WR", "TE"), 
    suffix = paste0(team, "22"))
 }

latest_adp_raw  = ffs_latest_rankings()

latest_adp_raw %>% 
  left_join(
    rosters %>% filter(season >= 2022) %>%
      select(player_id = gsis_id, sportradar_id)
  ) %>%
  filter(pos %in% c("RB", "WR", "TE", "QB")) %>%
  glimpse()


fantasypro  