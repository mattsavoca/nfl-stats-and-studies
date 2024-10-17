options(scipen = 999)
# Load the lpSolve package
library(lpSolve)
# install.packages("ompr")
# install.packages("ompr.roi")
# install.packages("ROI.plugin.glpk")
test_team = "Team 1"
test_round = 3
sim_results_raw = read_csv("./data/test_sim_results.csv")


library(tidyverse)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(coach)


determine_draft_info <- function(n_teams, n_rounds) {
  
  picks <- seq_len(n_teams * n_rounds)
  
  order <- c(1:n_teams, n_teams:1)
  
  teams <- paste0("Team ", order) # Create character team names
  
  df = data.frame(
    Pick = picks,
    Team = rep(teams, n_rounds)[picks] # Use character team names
  ) %>%
    group_by(Team) %>%
    mutate(
      Round = row_number()
    )
  
  return(df)
}

simulate_draft <- function(df) {
  df %>%
    group_by(pos, Round, Pick) %>%
    mutate(is_available = runif(n()) < p_avail,
           is_best = sim_ppg == max(sim_ppg[is_available])) %>%
    ungroup() 
}


generate_draft_values = function(draft_sheet, 
                                 n_sims = 50,
                                 n_teams = 12, 
                                 n_rounds = 9, 
                                 user_team = "Team 1"
){
  test_draft = determine_draft_info(n_teams, n_rounds)
  
  test_draft_picks = test_draft[test_draft$Team == user_team, ]
  
  test_sims = draft_sheet %>%
    mutate(
      player = paste0(first_name, " ",last_name),
      adp_sd = (sim_hi - sim_lo)/2
    ) %>%
    rename(
      pos = slot_name
    ) %>%
    select(
      player, pos, everything()
    )
  
  test_sim_df = crossing(
    test_draft_picks, id = test_sims$id
  ) %>%
    left_join(
      test_sims %>% select(id, player, pos, ovr_adp, adp_sd, sim_ppg), by = "id"
    ) %>%
    mutate(
      p_avail = 
        case_when(
          user_team == "Team 1" & Round == 1 ~ 1,
          T ~ 1- pnorm(Pick, ovr_adp, adp_sd)
        )
    ) %>% 
    arrange(Round, -sim_ppg)
  
  test_draft_values = test_sim_df %>% 
    group_by(pos, Round) %>%
    mutate(
      max_sim_ppg = max(sim_ppg),
    )
  
  sim_results <- map_dfr(1:n_sims, ~simulate_draft(test_sim_df))
  
  prob_best <- sim_results %>%
    group_by(pos, Round, Pick, player) %>%  # Replace 'player' with the name of your player identifier column
    mutate(
      prob_best = mean(is_best),
      e_player = sim_ppg * prob_best) %>%
    ungroup() %>%
    select(-is_available, -is_best) %>%
    distinct()
  
  draft_values = prob_best %>% 
    group_by(pos, Round) %>%
    top_n(1, e_player) %>%
    select(Round, pos, e_player, sim_ppg, id) %>%
    distinct() %>%
    ungroup()
  
  return(draft_values)
  
}

draft_look_ahead = function(draft_values, 
                            draft_sheet,
                            n_rounds = 9,
                            min_rb = 2,
                            min_qb = 2,
                            max_qb = 5,
                            min_wr = 2,
                            min_te = 1,
                            max_te = 4){
  
  model <- MIPModel() %>%
    # decision variable x[i], for each player i
    add_variable(x[i], i = 1:nrow(draft_values), type = "binary") %>%
    # Objective: maximize sum of e_player
    set_objective(sum_expr(draft_values$e_player[i] * x[i], i = 1:nrow(draft_values)), "max") %>%
    # Position constraints
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "RB")) >= 2) %>%
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "WR")) >= 2) %>%
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "QB")) >= 2) %>%
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "QB")) <= 5) %>%
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "TE")) >= 1) %>%
    add_constraint(sum_expr(x[i], i = which(draft_values$pos == "TE")) <= 4) %>%
    # "RB/WR/TE" constraint
    add_constraint(sum_expr(x[i], i = which(draft_values$pos %in% c("RB", "WR", "TE"))) >= 6) %>%
    # Round constraint
    add_constraint(sum_expr(x[i], i = which(draft_values$Round == round)) == 1, round = 1:n_rounds) %>%
    # Id constraint
    add_constraint(sum_expr(x[i], i = which(draft_values$id == id)) <= 1, id = unique(draft_values$id)) %>%
    # Total players constraint
    add_constraint(sum_expr(x[i], i = 1:nrow(draft_values)) == n_rounds)
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = F))
  
  solution <- result$solution
  
  selected_players = 
    draft_values[solution == 1, ] %>%
    left_join(draft_sheet) %>%
    group_by(Round, pos) %>%
    arrange(ovr_adp) %>%
    top_n(1) %>%
    arrange(Round) %>%
    ungroup()
  
  return(selected_players)
  
}



draft_values = generate_draft_values(sim_results_raw)
optimal_draft = draft_look_ahead(draft_values, sim_results_raw)

optimal_draft %>% glimpse()
draft_values %>% glimpse()







# Define the number of simulations

# Define a function to simulate one draft


# Simulate many drafts

# Calculate the proportion of simulations in which each player was the best available









## Solver ------




#write_csv(test_draft_values, "test_draft_values.csv")



# Solve the model

# Extract the solution

