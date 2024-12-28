# Mailroom ----------------------------------------------------------------
library(tidyverse)
library(nflverse)  
library(arrow)
library(NISTunits)
library(tidymodels)
source("09_makeVizFunc.R")
# Parquet Write -------------------------------------------------------------

# tracking_train <- read_csv("data/tracking_week_1.csv") |>
#   bind_rows(read_csv("data/tracking_week_2.csv")) |>
#   bind_rows(read_csv("data/tracking_week_3.csv")) |>
#   bind_rows(read_csv("data/tracking_week_4.csv")) |>
#   bind_rows(read_csv("data/tracking_week_5.csv")) |>
#   bind_rows(read_csv("data/tracking_week_6.csv")) |>
#   bind_rows(read_csv("data/tracking_week_7.csv"))
# 
# write_parquet(tracking_train, "data/tracking_train.parquet")
# 
# 
# tracking_test <- read_csv("data/tracking_week_8.csv") |> 
#   bind_rows(read_csv("data/tracking_week_9.csv"))
# 
# write_parquet(tracking_test, "data/tracking_test.parquet")

# Mod and Read ------------------------------------------------------------

tracking <- read_parquet("data/tracking_train.parquet") |> 
  mutate(
    new_y = ifelse(playDirection == "left", 120 - x, x),
    new_x = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  ) |> 
  mutate(y = new_y, x = new_x) |> 
  select(-c(new_x, new_y, time)) |> 
  group_by(gameId, playId, nflId) |> 
  mutate(lag_half_sec_x = lag(x, n = 5) , 
         lag_half_sec_y = lag(y, n = 5),
         lag_half_sec_s = lag(s, n = 5)) |> 
  ungroup()

games <- read_csv("data/games.csv")

plays <- read_csv("data/plays.csv") |> 
  select(-starts_with("pff_runConcept"), -qbKneel, -qbSneak, - rushLocationType)

players <- read_csv("data/players.csv")

player_play <- read_csv("data/player_play.csv", 
                        col_types = list(penaltyNames = col_character(),
                                         blockedPlayerNFLId3 = col_double())) |> 
  select(gameId, playId, nflId, inMotionAtBallSnap, shiftSinceLineset,
         motionSinceLineset, wasRunningRoute, routeRan, 
         pff_defensiveCoverageAssignment, pff_primaryDefensiveCoverageMatchupNflId,
         pff_secondaryDefensiveCoverageMatchupNflId)

# Ftn and nflverse --------------------------------------------------------


ftn <- load_ftn_charting(season = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass, starting_hash, 
         qb_location, is_screen_pass, is_motion, 
         is_trick_play,is_qb_out_of_pocket)



pbp <- load_pbp(2022) |> 
  select(play_id, old_game_id, game_id)

ftn_added <- left_join(ftn, pbp, by = join_by(nflverse_game_id == game_id, 
                                              nflverse_play_id == play_id)) |> 
  mutate(old_game_id = as.double(old_game_id))

plays_added <- left_join(plays, ftn_added, by = join_by(gameId == old_game_id, 
                                                        playId ==nflverse_play_id))


plays_filtering <- plays_added |> 
  filter(isDropback & pff_manZone == "Man" & (!is_screen_pass) & (!is_trick_play) & 
         (!qbSpike)) |> 
  select(-c(isDropback, is_screen_pass, is_trick_play, qbSpike, preSnapHomeScore, 
            preSnapVisitorScore, playNullifiedByPenalty, preSnapHomeTeamWinProbability,
            preSnapVisitorTeamWinProbability, timeToSack, passTippedAtLine, unblockedPressure,
            penaltyYards, yardsGained, homeTeamWinProbabilityAdded, visitorTeamWinProbilityAdded,
            expectedPointsAdded, expectedPoints, pff_runPassOption, is_qb_out_of_pocket))


joined <- inner_join(tracking, plays_filtering, by = join_by(gameId, playId)) |> 
  left_join(player_play, by = join_by(gameId, playId, nflId))
# Sample Play -------------------------------------------------------------

plays_824 <- plays_filtering |> 
  filter(gameId == 2022091113 & playId == 824)
tracking_824 <- joined |> 
  filter(gameId == 2022091113 & playId == 824)

plays_857 <- plays_filtering |> 
  filter(gameId == 2022100201 & playId == 1271)
tracking_857 <- joined |> 
  filter(gameId == 2022100201 & playId == 1271)

plays_2876 <- plays_filtering |> 
  filter(gameId == 2022100905 & playId == 2876)
tracking_2876 <- joined |> 
  filter(gameId == 2022100905 & playId == 2876)

tracking_2353 <- joined |> 
  filter(gameId == 2022091108 & playId == 2353)

plays_2353 <- plays_filtering |> 
  filter(gameId == 2022091108 & playId == 2353)


# Viz ---------------------------------------------------------------------

makeViz(tracking_2876, plays_2876, "NE", "DET", "#002244", "#0076B6", 2022, 5,
         yardhigh = 65)

makeViz(tracking_857, plays_857, "ATL", "CLE", "#a71930", "#FF3C00", 2022, 4,
        yardlow = 25, yardhigh = 120-35)

makeViz(tracking_824, plays_824, "TB", "DAL", "#a71930", "#0076B6", 2022, 5,
        yardlow = 70)

makeViz(tracking_2353, plays_2353, "TEN", "NYG", "#0076B6", "#a71930", 2022, 3)

# Slots -------------------------------------------------------------------


slots <- joined |> 
  filter(club == possessionTeam|club == "football") |> 
  filter(event == "ball_snap") |> 
  group_by(gameId, playId)|> 
  mutate(left = min(x)) |> 
  mutate(right = max(x)) |> 
  mutate(is_fb = case_when(
    club == "football" ~ x,
    .default = NA
  )) |> 
  mutate(fb_loc = max(is_fb, na.rm = TRUE)) |> 
  select(-is_fb) |> 
  ungroup() |> 
  filter(left < x & x < right & wasRunningRoute & (x < fb_loc - 5 | x > fb_loc + 5)) |> 
  mutate(is_slot = TRUE) |> 
  mutate(dist_football = abs(x - fb_loc)) |> 
  mutate(dist_outside = pmin(abs(x - left), abs(x - right)))


joined_2 <- joined |> 
  left_join(slots |> select(gameId, playId, nflId, is_slot), 
            by = join_by(gameId, playId, nflId)) |> 
  group_by(gameId, playId, nflId) |> 
  mutate(snap_frame = case_when(
    frameType == "SNAP" ~ frameId
  )) |> 
  mutate(snap_frame = max(snap_frame, na.rm = TRUE)) |> 
  ungroup()

slot_mvt <- joined_2 |> 
  filter(is_slot) |> 
  filter(frameType == "AFTER_SNAP" & 
         frameId < snap_frame + ceiling(timeInTackleBox * 10)) |> 
  mutate(delta_x = x - lag_half_sec_x,
         delta_y = y - lag_half_sec_y) |> 
  mutate(ang_rad = atan2(delta_y, delta_x)) |> 
  mutate(ang_deg = NISTradianTOdeg(ang_rad)) |> 
  mutate(turn = case_when(
    ang_deg < 45 & ang_deg > -45 & lag_half_sec_s > 1 ~ "right",
    ang_deg > 135 | ang_deg < -135 & lag_half_sec_s > 1 ~ "left",
    .default = "vert"
  ))|>
  group_by(gameId, playId, nflId) |>
  mutate(direction_break = case_when(
    any(turn == "left") & any(turn == "right") ~ "both",
    any(turn == "left") & (!any(turn == "right")) ~ "left",
    any(turn == "right") & (!any(turn == "left")) ~ "right",
    all(turn == "vert") ~ "vert",
  )) |>
  slice_head(n = 1) |>
  ungroup()
  # |>
  # mutate(new_abs_yardline = ifelse(playDirection == "left",
  #                                  120 - absoluteYardlineNumber,
  #                                  absoluteYardlineNumber)) |>
  # mutate(start_break = case_when(
  #   turn == direction_break & direction_break != "vert" &
  #     playDirection == "right" ~ y - new_abs_yardline,
  #   turn == direction_break & direction_break != "vert" &
  #     playDirection == "left" ~ y - new_abs_yardline
  # )) 



slots_2 <- slots |> 
  left_join(slot_mvt |> select(gameId, playId, nflId, direction_break),
            by = join_by(gameId, playId, nflId)) |> 
  filter(!is.na(direction_break))


slots_predict <- slots_2 |> 
  select(direction_break, club, down, yardsToGo, absoluteYardlineNumber, 
         offenseFormation, receiverAlignment, starting_hash, qb_location,
         is_motion, dist_football, dist_outside)

set.seed(11042004)
slots_predict_split <- initial_split(slots_predict, prop = 0.8, strata = direction_break)
slots_predict_train <- training(slots_predict_split)
slots_predict_test <- testing(slots_predict_split)

slots_predict_recipe <- recipe(direction_break ~ ., data = slots_predict_train) |> 
  step_bin2factor(all_logical_predictors()) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())
  
folds <- vfold_cv(slots_predict_train, v = 5, repeats = 3)

xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()) |> 
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
  size = 50
)
xgb_wf <-
  workflow() |> 
  add_recipe(slots_predict_recipe) |> 
  add_model(xgb_spec)

doParallel::registerDoParallel(cores = 5)
xgb_res <-
  tune_grid(
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
