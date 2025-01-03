# Mailroom ----------------------------------------------------------------

library(tidymodels)
library(DALEXtra)
source("01_Data_Manip.R")
source("08_ggplot_imp_func_and_extra_code.R")


# Modeling In/Out ----------------------------------------------------------------

slots_predict_z <- slots_predict |>
  filter(direction_break == "inside" | direction_break == "outside") |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    direction_break = as.factor(direction_break),
    receiverAlignment = as.factor(receiverAlignment)
  )

set.seed(11042004)
# set.seed(06062006)
slots_predict_split <- initial_split(slots_predict_z, prop = 0.8, strata = direction_break)
slots_predict_train <- training(slots_predict_split)
slots_predict_test <- testing(slots_predict_split)

slots_predict_recipe <- recipe(direction_break ~ ., data = slots_predict_train) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors())


folds <- vfold_cv(slots_predict_train, v = 5, repeats = 3)

xgb_spec <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_grid <- grid_space_filling(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), slots_predict_train),
  learn_rate(),
  size = 75
)

xgb_wf <- workflow() |>
  add_recipe(slots_predict_recipe) |>
  add_model(xgb_spec)

doParallel::registerDoParallel(cores = 5)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE),
  metrics = metric_set(roc_auc)
)

best <- xgb_res |>
  select_best(metric = "roc_auc")

final_mod <- xgb_res |>
  extract_workflow() |>
  finalize_workflow(best) |>
  last_fit(split = slots_predict_split)

collect_metrics(final_mod)

beepr::beep("fanfare")

model <- xgb_res |>
  extract_workflow() |>
  finalize_workflow(best)

fitted_model <- model |>
  fit(slots_predict_z)

reg_metrics <- metric_set(roc_auc)

slots_predict_final_test_q <- slots_predict_final_test |>
  filter(direction_break == "inside" | direction_break == "outside") |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    direction_break = as.factor(direction_break),
    receiverAlignment = as.factor(receiverAlignment)
  )

slots_predict_final_test_q |>
  bind_cols(predict(fitted_model, new_data = slots_predict_final_test_q, type = "prob")) |>
  reg_metrics(direction_break, .pred_inside)

slots_3 |>
  filter(gameId == 2022100206 & playId == 1386) |>
  bind_cols(
    predict(
      fitted_model,
      new_data = (slots_3 |> filter(gameId == 2022100206 & playId == 1386)), type = "prob"
    )
  ) |>
  View()

# Modeling all 4 ----------------------------------------------------------

slots_predict_zz <- slots_predict |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    direction_break = as.factor(direction_break),
    receiverAlignment = as.factor(receiverAlignment)
  )

slots_predict_final_test_y <- slots_predict_final_test |>
  mutate(
    club = as.factor(club), offenseFormation = as.factor(offenseFormation),
    direction_break = as.factor(direction_break),
    receiverAlignment = as.factor(receiverAlignment)
  )

set.seed(11042004)
# set.seed(06062006)
slots_predict_split_zz <- initial_split(slots_predict_zz, prop = 0.8, strata = direction_break)
slots_predict_train_zz <- training(slots_predict_split_zz)
slots_predict_test_zz <- testing(slots_predict_split_zz)

slots_predict_recipe_zz <- recipe(direction_break ~ ., data = slots_predict_train_zz) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors())


folds_zz <- vfold_cv(slots_predict_train_zz, v = 5, repeats = 3)

xgb_spec_zz <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(), mtry = tune(), learn_rate = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_grid_zz <- grid_space_filling(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), slots_predict_train_zz),
  learn_rate(),
  size = 75
)

xgb_wf_zz <- workflow() |>
  add_recipe(slots_predict_recipe_zz) |>
  add_model(xgb_spec_zz)

doParallel::registerDoParallel(cores = 5)
xgb_res_zz <- tune_grid(
  xgb_wf_zz,
  resamples = folds_zz,
  grid = xgb_grid_zz,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE),
  metrics = metric_set(roc_auc)
)

best_zz <- xgb_res_zz |>
  select_best(metric = "roc_auc")

final_mod_zz <- xgb_res_zz |>
  extract_workflow() |>
  finalize_workflow(best_zz) |>
  last_fit(split = slots_predict_split_zz)

collect_metrics(final_mod_zz)

beepr::beep("fanfare")

model_zz <- xgb_res_zz |>
  extract_workflow() |>
  finalize_workflow(best_zz)

fitted_model_zz <- model_zz |>
  fit(slots_predict_zz)

reg_metrics_zz <- metric_set(roc_auc)



# DALEX -------------------------------------------------------------------

vip_features <- c(
  "club", "yardsToGo", "absoluteYardlineNumber",
  "offenseFormation", "receiverAlignment",
  "dist_football", "dist_outside", "cb_align_x", "cb_align_y"
)

vip_train <-
  slots_predict_zz |>
  select(all_of(vip_features))

xgb_base_spec <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_base_wf <-
  workflow() |>
  add_recipe(slots_predict_recipe_zz) |>
  add_model(xgb_base_spec)

xgb_fit <- xgb_base_wf |>
  fit(data = slots_predict_zz)

explainer_xgb <-
  explain_tidymodels(
    xgb_fit,
    data = vip_train,
    y = slots_predict_zz$direction_break,
    label = "xgb",
    verbose = FALSE
  ) |> model_parts()

ggplot_imp(explainer_xgb)
