# Mailroom ----------------------------------------------------------------

library(tidymodels)
library(DALEXtra)
source("01_Data_Manip.R")
source("08_ggplot_imp_func_and_extra_code.R")

# Modeling ----------------------------------------------------------------

slots_predict_break_z <- slots_predict_break |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    receiverAlignment = as.factor(receiverAlignment)
  )

set.seed(11042004)
# set.seed(06062006)
slots_predict_break_split <- initial_split(slots_predict_break_z,
  prop = 0.8,
  strata = start_break_play
)
slots_predict_break_train <- training(slots_predict_break_split)
slots_predict_break_test <- testing(slots_predict_break_split)

slots_predict_break_recipe <- recipe(start_break_play ~ ., data = slots_predict_break_train) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors())


break_folds <- vfold_cv(slots_predict_break_train, v = 5, repeats = 3)

break_xgb_spec <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")

break_xgb_grid <- grid_space_filling(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), slots_predict_break_train),
  learn_rate(),
  size = 75
)

break_xgb_wf <- workflow() |>
  add_recipe(slots_predict_break_recipe) |>
  add_model(break_xgb_spec)

doParallel::registerDoParallel(cores = 5)

break_xgb_res <-
  break_xgb_wf |>
  tune_grid(
    resamples = break_folds,
    grid = break_xgb_grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE, save_workflow = TRUE),
  )

break_best <- break_xgb_res |>
  select_best(metric = "rmse")

break_final_mod <- break_xgb_res |>
  extract_workflow() |>
  finalize_workflow(break_best) |>
  last_fit(split = slots_predict_break_split)

collect_metrics(break_final_mod)

beepr::beep("fanfare")

break_model <- break_xgb_res |>
  extract_workflow() |>
  finalize_workflow(break_best)

fitted_break_model <- break_model |>
  fit(slots_predict_break_z)

reg_metrics <- metric_set(rmse, rsq)

slots_predict_break_final_test |>
  bind_cols(.pred = predict(fitted_break_model, new_data = slots_predict_break_final_test)) |>
  reg_metrics(start_break_play, .pred)

slots_predict_break_1386 <- (slots_3 |> filter(gameId == 2022100206 & playId == 1386)) |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    receiverAlignment = as.factor(receiverAlignment)
  ) |>
  bind_cols(
    predict(
      fitted_break_model,
      new_data = (slots_3 |> filter(gameId == 2022100206 & playId == 1386))
    )
  )

# Break VIP ---------------------------------------------------------------

break_vip_features <- c(
  "club", "yardsToGo", "absoluteYardlineNumber",
  "offenseFormation", "receiverAlignment",
  "dist_football", "dist_outside", "cb_align_x", "cb_align_y"
)

break_vip_train <-
  slots_predict_break_z |>
  select(all_of(break_vip_features))

break_xgb_base_spec <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression")

break_xgb_base_wf <-
  workflow() |>
  add_recipe(slots_predict_break_recipe) |>
  add_model(break_xgb_base_spec)

break_xgb_fit <- break_xgb_base_wf |>
  fit(data = slots_predict_break_z)

break_explainer_xgb <-
  explain_tidymodels(
    break_xgb_fit,
    data = break_vip_train,
    y = slots_predict_break_z$start_break_play,
    label = "xgb (Break)",
    verbose = FALSE
  ) |> model_parts()

break_explainer_no_model <-
  explain_tidymodels(
    break_xgb_fit,
    data = break_vip_train,
    y = slots_predict_break_z$start_break_play,
    label = "xgb (Break)",
    verbose = FALSE
  )

ggplot_imp(break_explainer_xgb)


# Break SHAP --------------------------------------------------------------
set.seed(11042004)
Carter <- slots_3 |>
  filter(gameId == 2022100206 & playId == 1386) |>
  filter(displayName == "DeAndre Carter")

Everett <- slots_3 |>
  filter(gameId == 2022100206 & playId == 1386) |>
  filter(displayName == "Gerald Everett")


Carter_breakdown <-
  predict_parts(
    explainer = break_explainer_no_model,
    new_observation = Carter, type = "shap", B = 20
  )
Everett_breakdown <-
  predict_parts(
    explainer = break_explainer_no_model,
    new_observation = Everett, type = "shap", B = 20
  )


Everett_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(
    data = ~ distinct(., variable, mean_val),
    aes(mean_val, variable),
    alpha = 0.5
  ) +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  labs(y = NULL) +
  scale_x_continuous(limits = c(-2, 2)) +
  ggtitle("Gerald Everett Route Break SHAP Values (20 Bootstrapped Runs)") +
  theme(axis.text.y = element_text(size = 13), plot.title = element_text(size = 16))


Carter_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(
    data = ~ distinct(., variable, mean_val),
    aes(mean_val, variable),
    alpha = 0.5
  ) +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  labs(y = NULL) +
  scale_x_continuous(limits = c(-2, 2)) +
  ggtitle("DeAndre Carter Route Break SHAP Values (20 Bootstrapped Runs)") +
  theme(axis.text.y = element_text(size = 13), plot.title = element_text(size = 16))
