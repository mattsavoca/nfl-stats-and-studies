library(tidyverse)
library(nflreadr)

ff_opp_raw <- load_ff_opportunity(2011:2024)

ff_opp <- ff_opp_raw %>%
    group_by(player = full_name, pos = position, player_id) %>%
    arrange(-as.integer(season), -week) %>%
    mutate(game_recency = row_number()) %>%
    # filter(game_recency <= 3) %>%
    summarize(
        recent_team = posteam[1],
        recent_season = max(season),
        g = n(),
        fpoe_5 = sum(total_fantasy_points_diff >= 5),
        fpoe_10 = sum(total_fantasy_points_diff >= 10),
        fpoe_15 = sum(total_fantasy_points_diff >= 15),
        fpoe_20 = sum(total_fantasy_points_diff >= 20),
        fpoe_25 = sum(total_fantasy_points_diff >= 25),
        across(where(is.numeric), mean),
        .groups = "drop"
    )


ff_opp %>%
    # mutate(across(contains("fpoe"), ~ . * g)) %>%
    filter(pos == "RB", recent_season >= 2015, g > 4) %>%
    select(player,
        pos, g,
        fpoe = total_fantasy_points_diff, xfp = total_fantasy_points_exp, contains("fpoe")
    ) %>%
    arrange(-fpoe) %>%
    view()
