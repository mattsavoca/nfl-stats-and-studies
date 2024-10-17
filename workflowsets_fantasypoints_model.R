library(tidymodels)
library(workflowsets)
library(ranger)
library(xgboost)
library(glmnet)
library(hardhat)
library(lightgbm)
library(catboost)
library(bonsai)
library(rlang)

# dataset -----
nfl_rosters_raw = load_rosters(seasons = 2011:2023)
nfl_rosters = nfl_rosters_raw %>%
  select(player_id = gsis_id, pos2 = position, season) %>%
  distinct()

ff_opp_augmented = ff_opp %>%
  # Transformations
  left_join(nfl_rosters) %>%
  mutate(pos = if_else(is.na(pos), pos2, pos)) %>%
  rename(fp = total_fantasy_points)

fp_n1_df = ff_opp_augmented %>% select(season_n1 = season, fp_n1 = fp, player_id) %>% distinct()
  

fp_etl_df = ff_opp_augmented %>%
  filter(pos %in% c("WR", "TE", "RB"), fp > 5, fpoe > -8, games > 1) %>%
  select(xfp_share, tm_fp, fp, season) %>%
  mutate(season_n1 = season + 1) %>%
  left_join(
    fp_n1_df
  ) %>%
  distinct()


fp_predict_df = fp_etl_df %>%
  filter(season == 2023)

fp_model_df = fp_etl_df %>%
  filter(season < 2023 & !is.na(fp_n1)) %>%
  distinct()





# Define the function --------
run_regression_models <- function(df, gridsize  = 30, var1 = "tm_fp", var2 = "xfp_share") {
  # Split the data
  set.seed(123)
  data_split <- initial_split(df, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  formula_string <- paste("fp_n1 ~", {{var1}}, "+", {{var2}})
  dynamic_formula <- as.formula(formula_string)
  
  # Recipe for pre-processing
  recipe <- recipe(dynamic_formula, data = train_data) %>%
    step_normalize(all_predictors())
  
  # Model specifications
  xgb_spec <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  rf_spec <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  glmnet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  lgbm_spec <- boost_tree(
    trees = tune(), tree_depth = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()) %>%
    set_mode("regression") %>%
    set_engine("lightgbm")
  
  # Workflow set
  workflows <- workflow_set(
    preproc = list(recipe = recipe),
    models = list(xgb = xgb_spec, rf = rf_spec, glmnet = glmnet_spec, lgbm = lgbm_spec),
    cross = TRUE
  )
  
  # Resampling and grid tuning
  folds <- vfold_cv(train_data, v = 5, repeats = 1)
  
  # Tune the models using workflow_map
  tuned_results <- workflows %>%
    workflow_map(
      "tune_grid",
      resamples = folds,
      grid = gridsize,
      metrics = metric_set(mae, rsq),
      control = control_grid(save_pred = TRUE, save_workflow = TRUE)
    )
  
  # Collect results and find the best model based on MAE
  collected_metrics <- collect_metrics(tuned_results)
  # best_mae <- results %>%
  #   filter(.metric == "mae") %>%
  #   arrange(mean) %>%
  #   slice(1)
  # cat("Best MAE:", best_mae$mean, "\n")
  
  # Return the tuned models
  list(splits = data_split, models = tuned_results, collected_metrics = collected_metrics, workflows = workflows)
}
  


# How does FP: influence FP_N1 
run_basic_models <- function(df, gridsize = 30) {
  # Split the data
  set.seed(123)
  data_split <- initial_split(df, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Recipe for pre-processing
  recipe <- recipe(fp_n1 ~ fp, data = train_data)
  
  # Model specifications
  xgb_spec <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  rf_spec <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  glmnet_spec <- linear_reg(
    ) %>%
    set_mode("regression") %>%
    set_engine("lm")


  # Workflow set
  workflows <- workflow_set(
    preproc = list(recipe = recipe),
    models = list(
      xgb = xgb_spec
      , rf = rf_spec
      , glmnet = glmnet_spec
      ),
    cross = TRUE
  )
  
  # Resampling and grid tuning
  folds <- vfold_cv(train_data, v = 5, repeats = 1)
  
  # Tune the models using workflow_map
  tuned_results <- workflows %>%
    workflow_map(
      "tune_grid",
      resamples = folds,
      grid = gridsize,
      metrics = metric_set(mae, rsq),
      control = control_grid(save_pred = TRUE, save_workflow = TRUE)
    )
  
  # Collect results and find the best model based on MAE
  results <- collect_metrics(tuned_results)
  # best_mae <- results %>%
  #   filter(.metric == "mae") %>%
  #   arrange(mean) %>%
  #   slice(1)
  # cat("Best MAE:", best_mae$mean, "\n")
  
  # Return the tuned models
  list(models = tuned_results)
}

# model results -----
doParallel::registerDoParallel()
tic()
model_results <- run_regression_models(fp_model_df, gridsize = 50)
toc()



# explore results -----

autoplot(model_results$models, select_best = T)

# indiv models
autoplot(model_results$models, metric = "mae", id = "recipe_glmnet")
autoplot(model_results$models, metric = "mae", id = "recipe_xgb")
autoplot(model_results$models, metric = "mae", id = "recipe_rf")
autoplot(model_results$models, metric = "mae", id = "recipe_lgbm")


# ollect metrics
collect_metrics(model_results$models, summarize = T)


# extractions ------
model_final_fit = model_results$models %>% 
  fit_best(metric = "rsq", 
           training(model_results$splits))

tidy(model_final_fit$fit$fit$spec)





# smaller models ------


tic()
tiny_model_results <- run_regression_models(fp_model_df, gridsize = 2)
toc()

tic()
basic_model_results = run_basic_models(fp_model_df, 25)
toc()

autoplot(basic_model_results$models)


# rsq
workflowsets::collect_metrics(model_results$models) %>% arrange(-mean) %>% filter(.metric == "rsq") %>% dplyr::slice(1)
workflowsets::collect_metrics(basic_model_results$models) %>% arrange(-mean) %>% filter(.metric == "rsq") %>% dplyr::slice(1)


# mae
workflowsets::collect_metrics(model_results$models) %>% arrange(mean) %>% filter(.metric == "mae") %>% dplyr::slice(1)
workflowsets::collect_metrics(basic_model_results$models) %>% arrange(mean) %>% filter(.metric == "mae") %>% dplyr::slice(1)


# predict best model -----

# Function to extract the best model and make predictions
library(tidymodels)
library(workflowsets)
library(dplyr)
library(broom)  # for augment

# Function to process model results and apply the best model to new data
process_best_model <- function(model_results, new_data, metric = "mae") {
  # Extract the results table from the model_results list
  results <- model_results$models %>%
    collect_metrics() %>%
    filter(.metric == metric) %>%
    arrange(mean) %>%
    dplyr::slice(1)
  
  # Extract the best workflow based on the specified metric
  best_workflow <- model_results$models %>%
    extract_workflow(results$.row)
  
  # Use last_fit to fit the best model on the full training set
  final_fit <- last_fit(best_workflow, split = model_results$split)
  
  # Use augment on the final fitted model to make predictions on new data
  predictions <- final_fit %>%
    pluck("fit") %>%
    augment(new_data = new_data)
  
  # Return the predictions
  predictions
}

process_best_model(model_results, fp_predict_df)

