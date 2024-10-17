# NFL 2023 Helpers -------
## Packages -------
#install.packages("easypackages")
library(easypackages)
packages(
  prompt = F,
  "shinydashboard",
  "DT",
  "googledrive",
  "googlesheets4",
  "ffscrapr",
  "markdown",
  "finetune",
  "tidytext",
  "htmlwidgets",
  "ggrepel",
  "shinycssloaders",
  "profvis",
  "tidyverse",
  "googlesheets4",
  "tidymodels",
  "noncompliance",
  "future",
  "data.table",
  "doParallel",
  "tictoc",
  "plotly",
  "janitor",
  "gt",
  "nflplotR",
  "nflreadr",
  "ffsimulator",
  "tidytext",
  "tidymodels",
  "usemodels",
  "ggridges",
  "RColorBrewer"
  #"odds.converter",
  #"ebbr"
)


# Best Ball SZN Name Changes -----
## Latest FP Rankings to sheets-----

### Scrape Latest FPros ADP ------
# fantasypros_rankings = ffs_latest_rankings() 

### Write FPros ADP to Google Sheet-----
#fantasypros_rankings %>% write_sheet(ss = player_name_key_url, sheet = "fp_latest_rankings")


## Underdog ----
convert_ud_adp_to_ffsimulator <- function(
    adp_df, #UD ADP
    team_names_key, #df with `fp_team` (FantasyPros Team ABR) and various team names/abbr. Requires `ud_team_name` variable.
    player_names_key, #df with `pos`, `fp_player` and various player name variants. Requires `ud_player` variable
    fp_latest_rankings #df created with ffsimulator::ffs_latest_rankings()
) {
  adp_df %>% 
    clean_names() %>%
    mutate(player = paste0(first_name, " ", last_name),
           adp = suppressWarnings(as.numeric(adp)),
           pos = slot_name) %>%
    select(-slot_name) %>%
    mutate(
      ovr_adp = if_else(is.na(adp), map_dbl(adp, ~sample(300:400,1)), adp),
      bye_week = if_else(is.na(bye_week), 0, as.numeric(bye_week))
    ) %>%
    arrange(ovr_adp) %>%
    mutate(ovr_rank = row_number()) %>%
    group_by(pos) %>%
    mutate(pos_adp = min_rank(ovr_adp)) %>%
    ungroup() %>%
    mutate(
      ud_id = id,
      scrape_date = lubridate::today()
    ) %>%
    # Prep for FPros ADP Doc
    select(
      player,
      pos,
      team_name,
      ovr_adp,
      ecr = pos_adp,
      ud_id,
      scrape_date
    ) %>%
    # Join with Team/Player Name Keys
    left_join(team_names_key %>% rename(team_name = ud_team, team = fp_team)) %>%
    left_join(player_names_key %>% select(player = ud_player, fp_player)) %>%
    mutate(player = if_else(!is.na(fp_player), fp_player, player)) %>%
    select(-fp_player) %>%
    filter(ecr != "-") %>%
    # Join with FPros Rankings
    left_join(fp_latest_rankings %>% select(player, pos, team, bye, sd)) %>%
    # Final Imputations
    mutate(
      impute_sd =  mean(sd[ovr_adp>=275], na.rm = T),
      sd = if_else(is.na(sd), impute_sd, sd),
      fantasypros_id = ud_id,
      team = if_else(is.na(team), "FA", team),
      bye = if_else(is.na(bye), 0, bye)
      ) %>%
    select(-impute_sd)
}

prepare_sims_for_ud_export = function(
    proj_df, #a data frame generated from  `generate_projection_summary()`
    rank_cutoff = 300, # number of players to include in the exported projection (defaults to 300),
    ud_adp # raw Underdog ADP with janitor::clean_names() run on it 
){
  proj_df %>%
    mutate(my_proj = mid_range * x_games) %>%
    mutate_if(is.numeric, round, 1) %>%
    filter(my_rank <= rank_cutoff) %>% 
    select(my_rank, id = contains("_id")[1], 
           ovr_adp, sim_ppg = mid_range, sim_lo = lo_range , sim_hi = hi_range, 
           usable_weeks, spike_weeks, my_proj, x_games) %>%
    left_join(ud_adp) %>%
    mutate(
      ud_ppg = projected_points/17,
      sim_ppg_diff = sim_ppg - ud_ppg,
      adp_rank = min_rank(ovr_adp),
      sim_rank_diff = my_rank - adp_rank) %>%
    select(-x_games)
  
}

## Drafters ----

convert_drafters_adp_to_ffsimulator = function(
    adp_df, #drafters ADP doc
    player_names_key, #df with `pos`, `fp_player` and various player name variants. Requires `drafters_player` variable within this function.
    latest_rankings #df created with ffsimulator::ffs_latest_rankings()
){
  adp_df %>% clean_names() %>%
    # Change Drafters Names to FP Names
    left_join(player_names_key %>% select(position = pos, name = drafters_player, fp_player)) %>%
    mutate(name = if_else(is.na(fp_player), name, fp_player),
           ovr_rank = row_number(),
           adp = if_else(adp == 0, 300, adp)) %>%
    select(-fp_player, -x5) %>%
    group_by(position) %>%
    # Create Positional ADP
    mutate(
      pos_adp = row_number(),
      scrape_date = lubridate::today()) %>%
    ungroup() %>%
    # Prepare DF for FFSimulator
    rename(
      player = name,
      ecr = pos_adp,
      pos = position,
      ovr_adp = adp,
      fantasypros_id = id #Necessary step for ffsimulator::ffs_generate_projections()
    ) %>%
    # Join with FPRos Ranks
    left_join(
      latest_rankings %>% select(-ecr, -fantasypros_id, -scrape_date)
    ) %>%
    # Imputations for Team , SD, and Byes
    mutate(
      impute_sd = mean(sd[ovr_adp>=175], na.rm = T),
      sd = if_else(is.na(sd), impute_sd, sd),
      team = if_else(is.na(team), "FA", team),
      bye = if_else(is.na(bye), 0, bye),
    ) %>%
    select(-impute_sd)
}


## DraftKings -----
convert_dk_adp_to_ffsimulator = function(
    adp_df, #DK ADP doc
    player_names_key, #df with `pos`, `fp_player` and various player name variants. Requires `dk_player` variable within this function.
    team_names_key, #df  `fp_team` and various team name variants. Requires `dk_team` variable within this function.
    latest_rankings #df created with ffsimulator::ffs_latest_rankings()
){
  adp_df %>% 
    clean_names() %>%
    select(-x6, -instructions) %>%
    # Change Drafters Names to FP Names
    left_join(player_names_key %>% select(position = pos, name = dk_player, fp_player)) %>%
    # Get FP Team Names
    left_join(team_names_key %>% select(team = dk_team, fp_team)) %>%
    rename(dk_team = team) %>%
    mutate(name = if_else(is.na(fp_player), name, fp_player),
           ovr_adp = if_else(is.na(adp), map_dbl(adp, ~sample(300:400,1)), adp)) %>%
    arrange(ovr_adp) %>%
    mutate(ovr_rank = row_number(),
           scrape_date = lubridate::today(),
           dk_id = id) %>%
    group_by(pos = position) %>%
    mutate(pos_adp = row_number()) %>%
    ungroup() %>%
    select(
      fantasypros_id = id, #necessary step for ffsimulator::ffs_generate_projections
      player = name,
      pos,
      dk_id,
      ovr_adp,
      ovr_rank,
      team = fp_team,
      ecr = pos_adp,
      scrae_date
    ) %>%
    left_join(
      latest_rankings %>% select(-ecr, -fantasypros_id, -scrape_date, -team)
    ) %>%
    mutate(
      impute_sd = mean(sd[ovr_adp>=175], na.rm = T),
      sd = if_else(is.na(sd), impute_sd, sd),
      team = if_else(is.na(team), "FA", team),
      bye = if_else(is.na(bye), 0, bye),
    ) %>%
    select(-impute_sd)
}


prepare_sims_for_dk_export = function(
    proj_df,
    dk_adp,
    rank_cutoff = 300
){
  proj_df %>%
    mutate(my_proj = mid_range * x_games) %>%
    mutate_if(is.numeric, round, 1) %>%
    filter(my_rank <= rank_cutoff) %>% 
    select(my_rank, id = contains("_id")[1], player, ovr_adp, 
           sim_ppg = mid_range, sim_lo = lo_range , sim_hi = hi_range, 
           usable_weeks, spike_weeks, my_proj, x_games) %>%
    left_join(
      dk_adp %>% select(-x6, -instructions) %>% clean_names()
    ) %>%
    mutate(
      adp_rank = min_rank(ovr_adp),
      sim_rank_diff = my_rank - adp_rank) %>% 
    select(-x_games) %>%
    arrange(my_rank)
}

## Final Export ----
ffsim_export = function(
    prepped_summarized_sim_df, # df prepped with prepare_sims_for_[]_export()
    path = "./projections/", #filepath
    sport = "NFL", # sport the projections are for
    league = "hppr", # league scoring delimeter
    date = NA) #Title of scoring or roster setting
{
  date = if_else(is.na(date), lubridate::today(), as.date(date))
  prepped_summarized_sim_df %>%
    write_csv(paste0(path,"_",sport,"_",league,"_",date,".csv"))
}


# FFSimulator Extensions -----
generate_projection_summary = function(sims, 
                                       ranks, # a converted underdog default ranks file (using `convert_[]_proj_to_ffsimulator()`)
                                       qb_baseline = 12, # Number of Starter QBs
                                       flex_baseline = 84, # Number of Starter FLEX
                                       te_boost = .33, #Positional Value boost to TE (As % of their FLEX baseline)
                                       spike_weight = .33, #Value to weigh players spike weeks vs. usable weeks
                                       sim_weight = .667, # Weight of Sim vs. ADP,
                                       TE_elite_rank = round(qb_baseline/2), #Rank of an elite TE week
                                       sig_digits = 1, # for rounding
                                       team_boosts = c("")
){
  sims %>% 
    mutate(flex = if_else(pos == "QB", "QB", "FLEX"),
           flex_baseline = if_else(flex == "QB", qb_baseline, flex_baseline)) %>%
    group_by(pos, season, week) %>% 
    mutate(weekly_pos_rank = min_rank(-projected_score),
           te_baseline_score = if_else(pos == "TE" & weekly_pos_rank <= TE_elite_rank, 1, 0)) %>%
    group_by(flex, season, week) %>% 
    mutate(weekly_flex_rank = min_rank(-projected_score),
           weekly_max_flex_score = max(projected_score),
           good_score_baseline =  if_else(flex == "FLEX", quantile(projected_score,.8), quantile(projected_score,.95)), # Is player within 20% of top FLEX/QB score?
           great_score_baseline =  if_else(flex == "FLEX", quantile(projected_score,.98), quantile(projected_score,.975)) # Is player within 10% of top FLEX/QB score?
    ) %>%
    ungroup() %>%
    mutate(
      baseline_score = if_else(weekly_flex_rank <= flex_baseline, 1, 0),
      good_score = if_else(projected_score >= good_score_baseline, 1,0),
      great_score = if_else(projected_score >= great_score_baseline, 1,0)
    ) %>%
    group_by(fantasypros_id) %>%
    summarize(
      player = player[1],
      pos = pos[1],
      flex = flex[1],
      ecr = ecr[1],
      sd = sd[1],
      rank = rank[1],
      good_score = sum(good_score)/n(),
      great_score= sum(great_score)/n(),
      lo_range = quantile(projected_score, 0.25),
      mid_range = quantile(projected_score, 0.5),
      hi_range = quantile(projected_score, 0.75),
      weekly_flex_rank = quantile(weekly_flex_rank, .5),
      weekly_pos_rank = quantile(weekly_pos_rank, .5),
      baseline_score = mean(baseline_score),
      injury_rate = mean(gp_model),
      x_games = injury_rate * 16,
      te_baseline_score = mean(te_baseline_score, na.rm =T),
      adj_baseline_score = if_else(pos == "TE", baseline_score * (1+te_baseline_score * te_boost), baseline_score),
      usable_weeks = adj_baseline_score * x_games,
      spike_weeks = great_score * x_games,
      .groups = "drop"
    ) %>%
    left_join(ranks %>% select(fantasypros_id, team, ovr_adp)) %>%
    mutate(
      usable_weeks = if_else(team %in% team_boosts & usable_weeks > 0, usable_weeks + 0.5, usable_weeks),
      spike_weeks = if_else(team %in% team_boosts & spike_weeks > 0, spike_weeks + 0.5, spike_weeks),
      usable_rank = min_rank(-usable_weeks),
      spike_rank = min_rank(-spike_weeks),
      sim_score = spike_rank * spike_weight + usable_rank * (1-spike_weight),
      sim_rank = min_rank(sim_score),
      edp = sim_weight * sim_rank + (1-sim_weight) * ovr_adp,
      my_rank = min_rank(edp)
    ) %>%
    rename(player_id = fantasypros_id) %>%
    select(my_rank, player, pos, team, player_id, everything(), -usable_rank, -spike_rank) %>%
    mutate_if(
      is.numeric, round, sig_digits
    ) %>%
    arrange(my_rank) %>%
    group_by(pos) %>%
    mutate(sim_pos_rank = row_number()) %>%
    ungroup()
}

prepare_sims_for_ud_export = function(
    proj_df, #a data frame generated from  `generate_projection_summary()`
    rank_cutoff = 300, # number of players to include in the exported projection (defaults to 300),
    ud_adp # raw Underdog ADP with janitor::clean_names() run on it 
){
  proj_df %>%
    mutate(my_proj = mid_range * x_games) %>%
    mutate_if(is.numeric, round, 1) %>%
    filter(my_rank <= rank_cutoff) %>% 
    select(my_rank, id = contains("_id")[1], ovr_adp, 
           sim_ppg = mid_range, 
           sim_lo = lo_range , 
           sim_hi = hi_range, 
           usable_weeks, 
           spike_weeks, 
           sim_pos_rank,
           my_proj, 
           x_games) %>%
    left_join(ud_adp) %>%
    mutate(
      ud_ppg = projected_points/17,
      sim_ppg_diff = sim_ppg - ud_ppg,
      adp_rank = min_rank(ovr_adp),
      sim_rank_diff = my_rank - adp_rank) %>%
    arrange(adp_rank) %>%
    group_by(slot_name) %>%
    mutate(adp_pos_rank = row_number()) %>%
    ungroup() %>% 
    arrange(my_rank) %>%
    mutate(
      pos_rank_diff = sim_pos_rank - adp_pos_rank
    ) %>%
    select(-x_games)
  
}


# Generic Functions ------

# Generate Draft Rounds from Fantsy Drafts with `team_num` and `pick_num` columns
generate_draft_rounds <- function(df) {
  max_team_num <- max(df$team_num)
  df <- df %>%
    mutate(draft_round = ceiling(pick_num / max_team_num))
  return(df)
}

# Parse Text into a Numeric column, replcaing NA with 0
quick_parse_number = function(col, na.replace = 0){
  res = if_else(is.na(col), na.replace, parse_number(col))
  return(res)
}

# Get Last Name from a full name
get_last_name <- function(full_name, include_suffix = TRUE) {
  # Extract last name using regular expression
  last_name <- sub(".*[[:space:]]+(([A-Za-z]+[-']?)+)(?:(?:[[:space:]]+(?:Jr\\.?|Sr\\.?|III|IV))|$)", "\\1", full_name)
  
  # Remove any suffixes from last name (e.g. Jr., Sr., III, IV)
  if (!include_suffix) {
    last_name <- gsub("(Jr\\.?|Sr\\.?|III|IV)$", "", last_name)
  }
  
  return(last_name)
}



# EDA Functions ----
# get the primary passers from nflreadr::load_pbp() data frame ----

generate_primary_passers = function(pbp){
  passers = pbp %>% 
    filter(!is.na(score_differential),
           season_type == "REG",
           !is.na(posteam) & posteam != "",
           #play_type %in% c("run", "pass")
           qb_dropback == 1
    ) %>%
    group_by(game_id, home_team, away_team, posteam, season, passer_player_name, passer_player_id) %>%
    summarize(
      dropbacks = n()
    ) %>% 
    group_by(game_id, posteam, season) %>%
    mutate(dropbacks_ct_rank = rank(-dropbacks, ties.method = "first")) %>%
    filter(dropbacks_ct_rank == 1) %>%
    top_n(1, dropbacks) %>% 
    rename(posteam_primary_passer = passer_player_name, posteam_passer_id = passer_player_id, posteam_primary_passer_dropbacks = dropbacks) %>% 
    group_by(game_id, season) %>%
    mutate(
      home_primary_passer_id = posteam_passer_id[posteam == home_team],
      away_primary_passer_id = posteam_passer_id[posteam == away_team],
      home_primary_passer_dropbacks = posteam_primary_passer_dropbacks[posteam == home_team] %>% as.numeric(),
      away_primary_passer_dropbacks = posteam_primary_passer_dropbacks[posteam == away_team] %>% as.numeric(),
    ) %>%
    ungroup() %>%
    select(-contains(c("posteam")), -dropbacks_ct_rank) %>%
    distinct()
  
  pbp %>%
    left_join(passers) %>%
    mutate(
      posteam_primary_passer_id = case_when(
        posteam == home_team ~ home_primary_passer_id,
        posteam == away_team ~ away_primary_passer_id,
        T ~ as.character(NA)
      ),
      posteam_primary_passer_dropbacks = case_when(
        posteam == home_team ~ home_primary_passer_dropbacks,
        posteam == away_team ~ away_primary_passer_dropbacks,
        T ~ as.numeric(NA)
      ),
    )
}



# Best Ball Tournament Sims ----------
simulate_draft_pool <- function(player_pool, num_teams, num_rounds, n_drafts, stack_weight_mean = 0.5, stack_weight_sd = .33) {
  
  simulate_draft <- function(player_pool, num_teams, num_rounds) {
    
    simulate_pick <- function(available_players, drafted_players, stack_weight_mean, stack_weight_sd) {
      position_counts <- table(drafted_players$pos)
      max_qb <- 5
      max_te <- 5
      
      if ("QB" %in% names(position_counts) && position_counts["QB"] >= max_qb) {
        available_players <- available_players[available_players$pos != "QB",]
      }
      if ("TE" %in% names(position_counts) && position_counts["TE"] >= max_te) {
        available_players <- available_players[available_players$pos != "TE",]
      }
      
      n_drafted = length(drafted_players)
      drafted_teams <- if (n_drafted < 1) {
        data.table(player = NA, team = NA)
      } else {
        drafted_players[team != "FA", .N, by = team][order(-N)]
      }
      primary_team <- if (n_drafted < 1) "" else drafted_teams$team[1]
      secondary_team <- if (n_drafted < 2) "" else drafted_teams$team[2]
      
      available_players[, stack_importance := rnorm(n = 1, mean = stack_weight_mean, sd = stack_weight_sd)
      ][, same_team_penalty := fifelse(team %in% primary_team, stack_importance[1] * fifelse(n_drafted > 15, 15, 1), 
                                       fifelse(team %in% secondary_team, stack_importance[1] * fifelse(n_drafted > 15, 2, .9),
                                               fifelse(n_drafted > 14, 15 * -stack_importance[1],
                                                       fifelse(n_drafted > 5, 2 * -stack_importance[1], 0))))
      ][, rand_std_dev := fifelse(team %in% drafted_teams, 0, sd*1.5)
      ][, user_ranking := ovr_adp - same_team_penalty
      ][, rand_ranking := rnorm(.N, mean = user_ranking, sd = rand_std_dev) - same_team_penalty
      ][, custom_ranking := rand_ranking - (rand_ranking * same_team_penalty)]
      
      selected_player <- available_players[order(custom_ranking)][1,]
      return(selected_player)
    }
    
    draft_order <- c()
    for (round in 1:num_rounds) {
      if (round %% 2 == 1) {
        draft_order <- c(draft_order, 1:num_teams)
      } else {
        draft_order <- c(draft_order, num_teams:1)
      }
    }
    
    available_players <- data.table(player_pool)
    draft_results <- data.table()
    
    draft_results <- future_map_dfr(draft_order,
                                    ~ {
                                      drafted_players <- draft_results[draft_results$team_num == .,]
                                      pick <- simulate_pick(available_players, drafted_players, stack_weight_mean, stack_weight_sd)
                                      available_players <<- available_players[available_players$player_id != pick$player_id,]
                                      pick
                                    }) %>%
      mutate(team_num = draft_order,
             pick_num = row_number()) %>%
      select(pick_num, team_num, player, pos, everything()) %>%
      arrange(pick_num)
    
    return(draft_results)
  }
  
  draft_pool <- future_map_dfr(1:n_drafts, 
                               ~simulate_draft(player_pool, num_teams, num_rounds) %>% 
                                 mutate(season = .x, 
                                        team_id = paste0("draft", 
                                                         if_else(season < 10, "0", ""), season,
                                                         "_team",
                                                         if_else(team_num < 10, "0", ""), team_num
                                        )
                                 ) %>%
                                 select(
                                   player, player_id, pos, flex, team, draft = season, 
                                   pick_num, team_num, sim_ppg = projected_score, team_id
                                 ) %>%
                                 arrange(draft, pick_num)
  )
  
  return(draft_pool)
}




create_draft_pools = function(n_pools, player_pool, num_teams, num_rounds, n_drafts, stack_weight_mean = .5, stack_weight_sd = .7, prefix = "", suffix = ""){
  draft_pools = 1:n_pools
  suppressWarnings(
    for (pool in draft_pools){
      print(paste0("Drafting pool ", pool, " of ", max(draft_pools),". Each Pool contains ", n_drafts, " drafts."))
      draft_pool = simulate_draft_pool(player_pool, num_teams, num_rounds, n_drafts, stack_weight_mean, stack_weight_sd)
      write_rds(draft_pool, paste0(prefix,"draft_pool_",pool, suffix, ".rds"))
      rm(draft_pool)
      toc()
      tic()
    }
  )
}

read_batch_drafts <- function(prefix) {
  combined_data <- data.frame()
  n <- 1
  
  while (TRUE) {
    file_name <- paste0(prefix, "_draft_pool_", n, ".rds")
    
    if (file.exists(file_name)) {
      data <- readRDS(file_name)
      data$pool <- n
      combined_data <- bind_rows(combined_data, data)
      n <- n + 1
    } else {
      break
    }
  }
  
  max_drafts = combined_data %>% 
    arrange(pool, draft) %>%
    group_by(pool) %>%
    summarize(
      max_draft = max(draft),
      .groups = "drop"
    ) %>%
    mutate(
      prev_draft_sum = cumsum(max_draft)
    ) %>%
    mutate(
      pool = pool + 1
    ) %>%
    select(-max_draft) 
  
  adjusted_data = max_drafts %>%
    full_join(combined_data) %>%
    mutate(
      prev_draft_sum = replace_na(prev_draft_sum, 0),
      new_draft_num = prev_draft_sum + draft,
      team_id = paste0("draft", if_else(new_draft_num < 10, "0",""), new_draft_num, "_team", if_else(team_num < 10, "0",""), team_num),
      draft = new_draft_num
    ) %>%
    select(-new_draft_num) %>%
    arrange(pool, draft)
  
  
  
  return(adjusted_data)
}

combine_draft_pool_with_sims <- function(
    df, sims, n_seasons, weeks = 1:16, season_seed = as.numeric(NA)) {
  
  setDT(df) # Convert the data frame to a data.table object
  setDT(sims)
  
  sims[, player_id := fantasypros_id] # Rename the column
  
  # Set the season value for the entire df
  season_set <- ifelse(is.na(season_seed), sample(1:n_seasons, 1), season_seed)
  
  draft_df <- df[, .(player, player_id, pos, flex, draft, team_num, sim_ppg)]
  
  sim_df <- sims[season == season_set, .(player_id, season, week, projected_score)]
  
  # Create a data.table with all combinations of weeks and unique player_ids
  weeks_df <- expand.grid.DT(unique(draft_df$player_id), weeks, season_set, 
                             col.names = c("player_id", "week", "season"))
  
  season_df <- weeks_df[sim_df, on = .(player_id, season, week)]
  
  draft_season_df <- season_df[draft_df, on = .(player_id), allow.cartesian=T][
    , `:=` (team_id = paste0("draft", ifelse(draft < 10, "0", ""), as.character(draft), 
                             "_team", ifelse(team_num < 10, "0", ""), as.character(team_num)))
  ]
  
  return(draft_season_df)
}





calculate_starters <- function(df,
                               n_qb = 1,
                               n_rb = 2,
                               n_wr = 2,
                               n_te = 1,
                               n_flex = 6,
                               n_sflex =7) {
  
  
  dt <- as.data.table(df %>% filter(!is.na(season)))
  
  # Create the unique_id and arrange the data by unique_id and week
  dt[, unique_id := paste0("draft", if_else(draft<10,"0",""), draft, "_", "team", if_else(team_num<10,"0",""), team_num, "_", "season", if_else(season<10,"0",""), season)]
  setorder(dt, unique_id, week, -projected_score)
  
  # Calculate the starter column
  result = dt[, starter := case_when(
    pos == "QB" & rowid(unique_id, week, pos) == n_qb ~ TRUE,
    pos == "RB" & rowid(unique_id, week, pos) <= n_rb ~ TRUE,
    pos == "WR" & rowid(unique_id, week, pos) <= n_wr ~ TRUE,
    pos == "TE" & rowid(unique_id, week, pos) == n_te ~ TRUE,
    flex == "FLEX" & rowid(unique_id, week, flex) <= n_flex-1 ~ TRUE,
    rowid(unique_id, week) <= n_sflex ~ TRUE,
    TRUE ~ FALSE
  )]
  
  return(result)
}




rd_n_grouping <- function(data, top_n_next_rd, grp_size = 12, next_round = 2) {
  #set.seed(42) # For reproducibility when randomizing groups
  
  # Generate column names based on next_round
  advance_to_rd_n_col <- paste0("advance_to_rd", next_round)
  rd_n_grp_col <- paste0("rd", next_round, "_grp")
  rd_n_minus_1_score_rk_col <- paste0("rd", next_round - 1, "_score_rk")
  rd_n_minus_1_grp_col <- paste0("rd", next_round - 1, "_grp")
  
  # Convert data frame to data.table
  setDT(data)
  
  # Perform the calculations
  data[, (advance_to_rd_n_col) := ifelse(get(rd_n_minus_1_score_rk_col) <= top_n_next_rd & get(rd_n_minus_1_grp_col) != 0, 1, 0), by = "season"]
  data[, (rd_n_grp_col) := ifelse(get(advance_to_rd_n_col) == 1,
                                  as.integer(gl(.N %/% grp_size, grp_size, length = .N)),
                                  0L), by = list(season, get(advance_to_rd_n_col))]
  
  return(data)
}



calculate_tournament <- function(
    df,
    top_n_rd2 = 2,
    rd2_grp_size = 12,
    top_n_rd3 = 2,
    rd3_grp_size = 12,
    top_n_rd4 = 2,
    rd4_grp_size = 260) {
  
  
  df %>%
    group_by(unique_id, week) %>%
    summarize(
      season = season[1],
      draft = draft[1],
      rd1_grp = draft,
      team_id = team_id[1],
      starter_pts = sum(projected_score[starter = T]),
    ) %>%
    group_by(unique_id) %>%
    summarize(
      season = season[1],
      draft = draft[1],
      rd1_grp = draft[1],
      team_id = team_id[1],
      rd1_pts = sum(starter_pts[week < 14]),
      rd2_pts = sum(starter_pts[week == 14]),
      rd3_pts = sum(starter_pts[week == 15]),
      rd4_pts = sum(starter_pts[week == 16]),
      full_season_pts = sum(starter_pts)
    ) %>%
    group_by(draft) %>%
    arrange(-rd1_pts) %>%
    mutate(
      rd1_score_rk = row_number(),
      advance_to_rd2 = if_else(rd1_score_rk < 3, 1, 0)
    ) %>%
    ungroup() %>%
    arrange(rd1_pts) %>%
    mutate(
      rd1_ovr_rk = row_number()
    ) %>%
    # Determine who advances to Rd 2 and place into new groups
    rd_n_grouping(top_n_next_rd = top_n_rd2, next_round = 2, grp_size = rd2_grp_size) %>%
    mutate(
      rd1_grp = draft,
      elim_score = case_when(
        advance_to_rd2 == 1 | rd2_grp == 0 ~ 0,
        T ~ rd1_pts),
      rd1_elim_rk = rank(-elim_score, ties.method = "first"),
      rd2_n = n_distinct(unique_id[advance_to_rd2 > 0]),
    ) %>%
    # Calculate best Rd2 Scores within Group and Season
    group_by(rd2_grp) %>%
    arrange(-advance_to_rd2, -rd2_pts) %>%
    mutate(rd2_score_rk = row_number()) %>%
    ungroup() %>%
    mutate( # teams that advanced to round
    ) %>%
    group_by(rd2_grp) %>%
    # Determine who advances to Rd 3 and place into new groups
    rd_n_grouping(top_n_next_rd = top_n_rd3, next_round = 3, grp_size = rd3_grp_size) %>%
    # Calculate best Rd3 Scores within Group and Season
    group_by(rd3_grp) %>%
    arrange(-advance_to_rd3, -rd3_pts) %>%
    mutate(rd3_score_rk = row_number()) %>%
    # Calculate best Rd3 Scores within Group and Season
    ungroup() %>%
    mutate(
      elim_score = case_when(
        advance_to_rd3 == 1 | rd2_grp == 0 ~ 0,
        T ~ rd2_pts),
      rd2_elim_rank = rank(-elim_score, ties.method = "first"),
      rd3_n = n_distinct(unique_id[advance_to_rd3 > 0])
    ) %>%
    select(-elim_score) %>%
    group_by(rd3_grp) %>%
    # Assign Finals Group
    rd_n_grouping(top_n_next_rd = top_n_rd4, next_round = 4, grp_size = rd4_grp_size) %>%
    group_by(rd4_grp) %>%
    arrange(-advance_to_rd4, -rd4_pts) %>% #View()
    mutate(rd4_score_rk = row_number()) %>%
    ungroup() %>%
    mutate(
      elim_score = case_when(
        advance_to_rd4 == 1 | rd3_grp == 0 | rd2_grp == 0 ~ 0,
        T ~ rd3_pts),
      rd3_elim_rank = rank(-elim_score, ties.method = "first"),
      rd4_n = n_distinct(unique_id[advance_to_rd4 > 0])) %>%
    group_by(rd4_grp) %>%
    rd_n_grouping(top_n_next_rd = 1, next_round = 5, grp_size = 1) %>%
    rename(tourney_winner = advance_to_rd5) %>%
    select(-rd5_grp, -elim_score) %>%
    ungroup() %>%
    mutate(
      reg_rank = rd1_ovr_rk,
      final_rank = case_when(
        rd4_grp > 0 ~ as.numeric(rd4_score_rk),
        rd3_grp > 0 ~ max(as.numeric(rd4_score_rk[rd4_grp>0])) + as.numeric(rd3_elim_rank),
        rd2_grp > 0 ~ max(as.numeric(rd4_score_rk[rd4_grp>0])) +
          max(as.numeric(rd3_score_rk[rd3_grp>0])) +
          as.numeric(rd2_elim_rank),
        T ~ max(as.numeric(rd4_score_rk[rd4_grp>0])) +
          max(as.numeric(rd3_score_rk[rd3_grp>0])) +
          max(as.numeric(rd2_score_rk[rd2_grp>0])) +
          as.numeric(rd1_elim_rk))
    ) %>%
    arrange(season, final_rank)
}







simulate_tournament <- function(df, sims, payouts, n_seasons, weeks, n_qb, n_rb, n_wr, n_te, n_flex, n_sflex, 
                                top_n_rd2, rd2_grp_size, top_n_rd3, rd3_grp_size, top_n_rd4, rd4_grp_size, season_seed = NA) {
  tic()
  print("Reading Projections...")
  df_combined <- combine_draft_pool_with_sims(df, sims, n_seasons = n_seasons, weeks = weeks, season_seed = season_seed)
  print("Optimizing Lineups...")
  df_starters <- calculate_starters(df_combined, n_qb, n_rb, n_wr, n_te, n_flex, n_sflex)
  print("Simulating Tournament...")
  df_tournament <- calculate_tournament(df_starters, top_n_rd2, rd2_grp_size, 
                                        top_n_rd3, rd3_grp_size, top_n_rd4, rd4_grp_size)
  print("Calculating Payouts...")
  df_payouts <- add_ud_payouts(df_tournament, payouts)
  toc()
  return(df_payouts)
}


simulate_tournaments <- function(df, sims, payouts, n_seasons, weeks, n_qb, n_rb, n_wr, n_te, n_flex, n_sflex, 
                                 top_n_rd2, rd2_grp_size, top_n_rd3, rd3_grp_size, top_n_rd4, rd4_grp_size,
                                 explicit_seasons = FALSE) {
  suppressWarnings({
    
    max_proj_seasons <- max(sims$season)
    
    if (!explicit_seasons) {
      season_sample <- sample(1:max_proj_seasons, n_seasons, replace = TRUE)
    } else {
      season_sample <- 1:n_seasons
    }
    
    combined_df <- map_dfr(season_sample, function(season) {
      df_combined <- combine_draft_pool_with_sims(df, sims, season_seed = season, weeks = weeks)
      df_starters <- calculate_starters(df_combined, n_qb, n_rb, n_wr, n_te, n_flex, n_sflex)
      df_tournament <- calculate_tournament(df_starters, top_n_rd2, rd2_grp_size, 
                                            top_n_rd3, rd3_grp_size, top_n_rd4, rd4_grp_size)
      df_payouts <- add_ud_payouts(df_tournament, payouts)
      df_final <- df_payouts[, total_payout := ifelse(is.na(total_payout), 0, total_payout)]
      df_selected <- df_final[, c("season", "team_id", grep("advance_", names(df_final), value = TRUE), "final_rank", grep("payout", names(df_final), value = TRUE)), with=FALSE]
      # print(paste0("Simulating Season: ", season+1))
      return(df_selected)
    })
    
    return(combined_df)
  })
}


create_payouts_df <- function(rd1_payout_pairs, rd2_payout_pairs, rd3_payout_pairs, final_payout_pairs) {
  
  # Create a helper function to process a single pair
  process_pair <- function(pair, payout_column) {
    sequence <- pair[-length(pair)]
    payout_value <- pair[length(pair)]
    temp_df <- tibble(rank = sequence, payout = rep(payout_value, length(sequence)))
    
    # Rename the payout column
    colnames(temp_df)[2] <- payout_column
    return(temp_df)
  }
  
  # Process rd1_payout_pairs and pairs_list2
  rd1_payouts <- map_df(rd1_payout_pairs, ~process_pair(.x, "rd1_payout"))
  rd2_payouts <- map_df(rd2_payout_pairs, ~process_pair(.x, "rd2_payout"))
  rd3_payouts <- map_df(rd3_payout_pairs, ~process_pair(.x, "rd3_payout"))
  rd4_payouts <- map_df(final_payout_pairs, ~process_pair(.x, "final_payout"))
  
  
  # Combine the two data frames based on final_rank column
  payout_df <- full_join(rd1_payouts, rd2_payouts, by = "rank") %>%
    full_join(rd3_payouts) %>% full_join(rd4_payouts)
  
  
  return(payout_df %>% mutate_all(replace_na,0))
}

add_ud_payouts <- function(df, payouts_df){
  df %>% left_join(payouts_df %>% select(rd1_ovr_rk = rank, rd1_payout)) %>% 
    left_join(payouts_df %>% select(rd2_score_rk = rank, rd2_payout)) %>%
    left_join(payouts_df %>% select(rd3_score_rk = rank, rd3_payout)) %>%
    left_join(payouts_df %>% select(final_rank = rank, final_payout)) %>%
    mutate(across(contains("_payout"), ~ replace_na(.x, 0))) %>%
    mutate(total_payout = rd1_payout + rd2_payout + rd3_payout + final_payout) %>%
    arrange(-total_payout)
}


# Tourney Sims (Deprecated) ----------


# simulate_draft_pool <- function(player_pool, num_teams, num_rounds, n_drafts) {
#   # Initialize an empty list to store the results of each draft
#   draft_results_list <- list()
# 
#   simulate_draft <<- function(player_pool, num_teams, num_rounds) {
#     
#     available_players <- player_pool
#     
#     simulate_pick <<- function(available_players, drafted_players) {
#       # Filter remaining players based on position constraints
#       position_counts <- table(drafted_players$pos)
#       max_qb <- 4
#       max_te <- 4
#       
#       if ("QB" %in% names(position_counts) && position_counts["QB"] >= max_qb) {
#         available_players <- available_players[available_players$pos != "QB",]
#       }
#       if ("TE" %in% names(position_counts) && position_counts["TE"] >= max_te) {
#         available_players <- available_players[available_players$pos != "TE",]
#       }
#       
#       # Calculate rand_std_dev for each player as a random percentage of their ovr_adp
#       available_players <- available_players %>%
#         dplyr::mutate(rand_std_dev = available_players$sd)
#       
#       # Generate custom_ranking using rnorm() with ovr_adp as the mean and rand_std_dev as the standard deviation
#       available_players <- available_players %>%
#         dplyr::mutate(custom_ranking = rnorm(n(), mean = ovr_adp, sd = rand_std_dev))
#       
#       # Sort remaining players by custom rankings
#       available_players <- available_players[order(available_players$custom_ranking),]
#       
#       # Select the top player
#       selected_player <- available_players[1,]
#       return(selected_player)
#     }
#     
#     draft_results <- data.frame()
#     
#     for (round in 1:num_rounds) {
#       for (team in 1:num_teams) {
#         if (round %% 2 == 0) { # Even round (Snake format)
#           draft_position <- num_teams - team + 1
#         } else { # Odd round
#           draft_position <- team
#         }
#         
#         if (nrow(draft_results) > 0) {
#           drafted_players <- filter(draft_results, team_num == draft_position)
#         } else {
#           drafted_players <- data.frame()
#         }
#         
#         pick <- simulate_pick(available_players, drafted_players) # Set adp_weight as needed
#         
#         # Add columns for team_num and pick_num
#         pick$team_num <- draft_position
#         pick$pick_num <- (round - 1) * num_teams + draft_position
#         
#         # Append the pick to the draft_results data frame
#         draft_results <- bind_rows(draft_results, pick)
#         
#         # Remove the selected player based on player_id
#         available_players <- available_players[available_players$player_id != pick$player_id,]
#       }
#     }
#     
#     draft_results <- draft_results %>% select(-custom_ranking) %>% 
#       select(pick_num, team_num, player, pos, everything()) %>%
#       arrange(pick_num)
#     
#     return(draft_results)
#   }
#   # Run simulate_draft() n_drafts times
#   for (i in 1:n_drafts) {
#     draft_results <- simulate_draft(player_pool, num_teams, num_rounds)
#     draft_results$season <- i
#     draft_results_list[[i]] <- draft_results
#   }
#   
#   # Combine the individual draft results into a single tibble
#   draft_pool <- dplyr::bind_rows(draft_results_list)
#   
#   return(draft_pool)
# }

# combine_draft_pool_with_sims = function(
    #     df, sims, n_seasons, weeks = 1:16, season_seed = as.numeric(NA)){
#   season_set = ifelse(is.na(season_seed), sample(1:n_seasons, 1), season_seed)
#   
#   draft_df = df %>%
#     select(player, player_id, pos, flex, draft, pick_num, team_num, sim_ppg)
#   
#   
#   sim_df = sims %>% 
#     filter(season == season_set) %>%
#     rename(player_id = fantasypros_id) %>%
#     select(player_id, season, week, projected_score)
#   
#   draft_df %>%
#     mutate(
#       season = replace_na(season_seed, sample(1:n_seasons, 1)),
#       team_id = paste0("draft", if_else(draft<10,"0","")
#                        ,as.character(draft),"_team"
#                        ,if_else(team_num<10,"0",""), as.character(team_num))
#     ) %>%
#     crossing(week = weeks) %>%
#     left_join(sim_df, by = c("player_id","season", "week")
#     ) %>%
#     mutate(projected_score = replace_na(projected_score, 0))
# }

# rd_n_grouping <- function(data, top_n_next_rd, grp_size = 12, next_round = 2) {
#   #set.seed(42) # For reproducibility when randomizing groups
#   
#   # Generate column names based on next_round
#   advance_to_rd_n_col <- paste0("advance_to_rd", next_round)
#   rd_n_grp_col <- paste0("rd", next_round, "_grp")
#   rd_n_minus_1_score_rk_col <- paste0("rd", next_round - 1, "_score_rk")
#   rd_n_minus_1_grp_col <- paste0("rd", next_round - 1, "_grp")
#   
#   data <- data %>%
#     mutate(!!advance_to_rd_n_col := ifelse(!!sym(rd_n_minus_1_score_rk_col) <= top_n_next_rd & !!sym(rd_n_minus_1_grp_col) != 0, 1, 0)) %>%
#     group_by(season, !!sym(advance_to_rd_n_col)) %>%
#     mutate(!!rd_n_grp_col := if_else(!!sym(advance_to_rd_n_col) == 1,
#                                      as.integer(gl(n() %/% grp_size, grp_size, length = n())), 
#                                      0L)) %>%
#     ungroup()
#   
#   return(data)
# }


# rd_n_grouping <- function(data, top_n_next_rd, grp_size = 12, next_round = 2) {
#   #set.seed(42) # For reproducibility when randomizing groups
#   
#   # Generate column names based on next_round
#   advance_to_rd_n_col <- paste0("advance_to_rd", next_round)
#   rd_n_grp_col <- paste0("rd", next_round, "_grp")
#   rd_n_minus_1_score_rk_col <- paste0("rd", next_round - 1, "_score_rk")
#   rd_n_minus_1_grp_col <- paste0("rd", next_round - 1, "_grp")
#   
#   data <- data %>%
#     mutate(!!advance_to_rd_n_col := ifelse(!!sym(rd_n_minus_1_score_rk_col) <= top_n_next_rd & !!sym(rd_n_minus_1_grp_col) != 0, 1, 0)) %>%
#     group_by(season, !!sym(advance_to_rd_n_col)) %>%
#     mutate(!!rd_n_grp_col := if_else(!!sym(advance_to_rd_n_col) == 1,
#                                      as.integer(gl(n() %/% grp_size, grp_size, length = n())), 
#                                      0L)) %>%
#     ungroup()
#   
#   return(data)
# }

# calculate_tournament <- function(
    #     df, 
#     top_n_rd2 = 2,
#     rd2_grp_size = 12,
#     top_n_rd3 = 2, 
#     rd3_grp_size = 12,
#     top_n_rd4 = 2,
#     rd4_grp_size = 260) {
#   
#   # Convert data frame to data.table
#   setDT(df)
#   
#   df <- df[, .(season = season[1],
#                draft = draft[1],
#                team_id = team_id[1],
#                starter_pts = sum(projected_score[starter]),
#                week = week),
#            by = .(unique_id, week)][
#              , .(season = season[1],
#                  draft = draft[1],
#                  team_id = team_id[1],
#                  rd1_pts = sum(starter_pts[week < 14]),
#                  rd2_pts = sum(starter_pts[week == 14]),
#                  rd3_pts = sum(starter_pts[week == 15]),
#                  rd4_pts = sum(starter_pts[week == 16]),
#                  full_season_pts = sum(starter_pts)),
#              by = .(unique_id)][
#                order(-rd1_pts), rd1_score_rk := .I, by = .(draft)][
#                  , advance_to_rd2 := ifelse(rd1_score_rk < 3, 1, 0)][
#                    , rd1_grp := draft]
#   
#   # Determine who advances to Rd 2 and place into new groups
#   df <- rd_n_grouping(df, top_n_next_rd = top_n_rd2, next_round = 2, grp_size = rd2_grp_size)
#   
#   # Calculate best Rd2 Scores within Group and Season
#   df[, rd2_score_rk := .I[order(-advance_to_rd2, -rd2_pts)], by = rd2_grp]
#   
#   # Calculate rd2_n
#   rd2_n <- uniqueN(df[advance_to_rd2 > 0, unique_id])
#   
#   # Determine who advances to Rd 3 and place into new groups
#   df <- rd_n_grouping(df, top_n_next_rd = top_n_rd3, next_round = 3, grp_size = rd3_grp_size)
#   
#   # Calculate best Rd3 Scores within Group and Season
#   df[, rd3_score_rk := .I[order(-advance_to_rd3, -rd3_pts)], by = rd3_grp]
#   
#   # Calculate elim_score, rd2_elim_rank, and rd3_n
#   df[, `:=` (rd2_elim_score = fifelse(advance_to_rd3 == 1 | rd2_grp == 0, 0, rd2_pts),
#              rd3_n = uniqueN(unique_id[advance_to_rd3 > 0]))]
#   
#   # Update rd2_elim_rank separately
#   df[, rd2_elim_rank := frank(-.SD$rd2_elim_score, ties.method = "first")]
#   
#   
#   # Assign Finals Group
#   df <- rd_n_grouping(df, top_n_next_rd = top_n_rd4, next_round = 4, grp_size = rd4_grp_size)
#   
#   # Calculate best Rd4 Scores within Group and Season
#   df[, rd4_score_rk := .I[order(-advance_to_rd4, -rd4_pts)], by = rd4_grp]
#   
#   # Calculate elim_score, rd3_elim_rank, and rd4_n using data.table
#   df[, `:=` (
#     rd3_elim_score = ifelse(advance_to_rd4 == 1 | rd3_grp == 0 | rd2_grp == 0, 0, rd3_pts),
#     rd4_n = uniqueN(unique_id[advance_to_rd4 > 0]))
#   ]
#   
#   # Update rd3_elim_rank separately
#   df[, rd3_elim_rank := frank(-.SD$rd3_elim_score, ties.method = "first")]
#   
#   
#   # Determine the Tournament Winner using data.table
#   df <- rd_n_grouping(df, top_n_next_rd = 1, next_round = 5, grp_size = 1)[
#     , `:=` (
#       tourney_winner = advance_to_rd5,
#       final_rank = fifelse(
#         rd4_grp > 0, as.numeric(rd4_score_rk),
#         fifelse(
#           rd3_grp > 0, max(as.numeric(rd4_score_rk[rd4_grp > 0])) + as.numeric(rd3_elim_rank),
#           fifelse(
#             rd2_grp > 0, max(as.numeric(rd4_score_rk[rd4_grp > 0])) + max(as.numeric(rd3_score_rk[rd3_grp > 0])) + as.numeric(rd2_elim_rank),
#             999999
#           )
#         )
#       )
#     )
#   ][, c("rd5_grp") := NULL]
#   
#   # Arrange by season and final_rank
#   setorder(df, season, final_rank)
#   
#   return(df)
# }
# add_payouts <- function(df, payouts) {
#   # Convert data frames to data tables
#   df <- as.data.table(df)
#   payouts <- as.data.table(payouts)
#   
#   # Perform the left join and replace NA values with 0
#   result <- payouts[df, on = .(final_rank), nomatch = 0][is.na(payout), payout := 0]
#   
#   return(result)
# }


# create_payout_df <- function(pairs_list) {
#   # Initialize an empty data frame
#   payout_df <- tibble(final_rank = numeric(), payout = numeric())
#   
#   # Iterate through the list of pairs
#   for (pair in pairs_list) {
#     # Separate the sequence and the payout value from the pair
#     sequence <- pair[-length(pair)]
#     payout_value <- pair[length(pair)]
#     
#     # Create a temporary data frame with the rank and payout values
#     temp_df <- tibble(final_rank = sequence, payout = rep(payout_value, length(sequence)))
#     
#     # Add the temporary data frame to the main data frame using bind_rows()
#     payout_df <- bind_rows(payout_df, temp_df)
#   }
#   
#   return(payout_df)
# }
# 
# 
# add_payouts <- function(df, payouts){
#   df %>%
#     left_join(payouts)
# }




