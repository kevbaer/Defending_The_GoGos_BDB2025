# Mailroom ----------------------------------------------------------------
library(tidyverse)
library(nflverse)
library(arrow)
library(NISTunits)
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

fn_first <- function(x, na_rm = FALSE) {
  if (all(is.na(x))) NA else first(x, na_rm = na_rm)
}


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
  mutate(
    lag_half_sec_x = lag(x, n = 5),
    lag_half_sec_y = lag(y, n = 5),
    lag_half_sec_s = lag(s, n = 5)
  ) |>
  ungroup()

games <- read_csv("data/games.csv")

plays <- read_csv("data/plays.csv") |>
  select(-starts_with("pff_runConcept"), -qbKneel, -qbSneak, -rushLocationType)

players <- read_csv("data/players.csv")

player_play <- read_csv("data/player_play.csv",
  col_types = list(
    penaltyNames = col_character(),
    blockedPlayerNFLId3 = col_double()
  )
) |>
  select(
    gameId, playId, nflId, inMotionAtBallSnap, shiftSinceLineset,
    motionSinceLineset, wasRunningRoute, routeRan,
    pff_defensiveCoverageAssignment, pff_primaryDefensiveCoverageMatchupNflId,
    pff_secondaryDefensiveCoverageMatchupNflId
  )

# Ftn and nflverse --------------------------------------------------------
ftn <- load_ftn_charting(season = 2022) |>
  select(
    nflverse_game_id, nflverse_play_id, is_screen_pass, starting_hash,
    qb_location, is_screen_pass, is_motion,
    is_trick_play, is_qb_out_of_pocket
  )

pbp <- load_pbp(2022) |>
  select(play_id, old_game_id, game_id)

ftn_added <- left_join(ftn, pbp, by = join_by(
  nflverse_game_id == game_id,
  nflverse_play_id == play_id
)) |>
  mutate(old_game_id = as.double(old_game_id))

plays_added <- left_join(plays, ftn_added, by = join_by(
  gameId == old_game_id,
  playId == nflverse_play_id
))

plays_filtering <- plays_added |>
  filter(isDropback & pff_manZone == "Man" & (!is_screen_pass) & (!is_trick_play) &
    (!qbSpike)) |>
  select(-c(
    isDropback, is_screen_pass, is_trick_play, qbSpike, preSnapHomeScore,
    preSnapVisitorScore, playNullifiedByPenalty, preSnapHomeTeamWinProbability,
    preSnapVisitorTeamWinProbability, timeToSack, passTippedAtLine, unblockedPressure,
    penaltyYards, yardsGained, homeTeamWinProbabilityAdded, visitorTeamWinProbilityAdded,
    expectedPointsAdded, expectedPoints, pff_runPassOption, is_qb_out_of_pocket
  ))

joined <- inner_join(tracking, plays_filtering, by = join_by(gameId, playId)) |>
  left_join(player_play, by = join_by(gameId, playId, nflId)) |>
  mutate(new_abs_yardline = ifelse(playDirection == "left",
    120 - absoluteYardlineNumber,
    absoluteYardlineNumber
  ))
# Slots -------------------------------------------------------------------
slots <- joined |>
  filter(club == possessionTeam | club == "football") |>
  filter(event == "ball_snap") |>
  group_by(gameId, playId) |>
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
  filter(!(new_abs_yardline - y > 3)) |>
  mutate(is_slot = TRUE) |>
  mutate(dist_football = abs(x - fb_loc)) |>
  mutate(dist_outside = pmin(abs(x - left), abs(x - right)))


joined_2 <- joined |>
  left_join(slots |> select(gameId, playId, nflId, is_slot),
    by = join_by(gameId, playId, nflId)
  ) |>
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
  mutate(
    delta_x = x - lag_half_sec_x,
    delta_y = y - lag_half_sec_y
  ) |>
  mutate(ang_rad = atan2(delta_y, delta_x)) |>
  mutate(ang_deg = NISTradianTOdeg(ang_rad)) |>
  mutate(turn = case_when(
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(26 - x) < abs(26 - lag_half_sec_x)) ~ "inside",
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(157 / 6 - x) > abs(157 / 6 - lag_half_sec_x)) ~ "outside",
    .default = "vert"
  )) |>
  group_by(gameId, playId, nflId) |>
  mutate(direction_break = case_when(
    any(turn == "inside") & any(turn == "outside") ~ "both",
    any(turn == "inside") & (!any(turn == "outside")) ~ "inside",
    any(turn == "outside") & (!any(turn == "inside")) ~ "outside",
    all(turn == "vert") ~ "vert",
  )) |>
  ungroup() |>
  mutate(start_break = case_when(
    (turn == direction_break & direction_break != "vert") &
      (playDirection == "right") ~ y - new_abs_yardline,
    (turn == direction_break & direction_break != "vert") &
      (playDirection == "left") ~ y - new_abs_yardline
  )) |>
  group_by(gameId, playId, nflId) |>
  mutate(start_break_play = fn_first(start_break, na_rm = TRUE)) |>
  slice_head(n = 1) |>
  ungroup()


slots_2 <- slots |>
  left_join(slot_mvt |> select(gameId, playId, nflId, direction_break, start_break_play),
    by = join_by(gameId, playId, nflId)
  ) |>
  filter(!is.na(direction_break))

# cbs ---------------------------------------------------------------------

cbs <- joined |>
  filter(!is.na(pff_primaryDefensiveCoverageMatchupNflId)) |>
  filter(frameType == "SNAP", pff_defensiveCoverageAssignment == "MAN") |>
  select(gameId, playId, cb_x = x, cb_y = y, primary_cover = pff_primaryDefensiveCoverageMatchupNflId)

slots_3 <- left_join(slots_2, cbs,
  by = join_by(gameId, playId, nflId == primary_cover), multiple = "first"
) |>
  mutate(cb_align_x = ifelse(cb_x > 157 / 6, cb_x - x, x - cb_x), cb_align_y = cb_y - y)



slots_predict <- slots_3 |>
  select(
    direction_break, club, yardsToGo, absoluteYardlineNumber,
    offenseFormation, receiverAlignment, dist_football, dist_outside, cb_align_x,
    cb_align_y
  )

slots_predict_break <- slots_3 |>
  select(
    start_break_play, club, yardsToGo, absoluteYardlineNumber,
    offenseFormation, receiverAlignment, dist_football, dist_outside, cb_align_x,
    cb_align_y
  ) |>
  filter(!is.na(start_break_play))

# Testing Data ------------------------------------------------------------

tracking_test <- read_parquet("data/tracking_test.parquet") |>
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
  mutate(
    lag_half_sec_x = lag(x, n = 5),
    lag_half_sec_y = lag(y, n = 5),
    lag_half_sec_s = lag(s, n = 5)
  ) |>
  ungroup()

joined_test <- inner_join(tracking_test, plays_filtering, by = join_by(gameId, playId)) |>
  left_join(player_play, by = join_by(gameId, playId, nflId)) |>
  mutate(new_abs_yardline = ifelse(playDirection == "left",
    120 - absoluteYardlineNumber,
    absoluteYardlineNumber
  ))

slots_test <- joined_test |>
  filter(club == possessionTeam | club == "football") |>
  filter(event == "ball_snap") |>
  group_by(gameId, playId) |>
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
  filter(!(new_abs_yardline - y > 3)) |>
  mutate(is_slot = TRUE) |>
  mutate(dist_football = abs(x - fb_loc)) |>
  mutate(dist_outside = pmin(abs(x - left), abs(x - right)))


joined_2_test <- joined_test |>
  left_join(slots_test |> select(gameId, playId, nflId, is_slot),
    by = join_by(gameId, playId, nflId)
  ) |>
  group_by(gameId, playId, nflId) |>
  mutate(snap_frame = case_when(
    frameType == "SNAP" ~ frameId
  )) |>
  mutate(snap_frame = max(snap_frame, na.rm = TRUE)) |>
  ungroup()

slot_mvt_test <- joined_2_test |>
  filter(is_slot) |>
  filter(frameType == "AFTER_SNAP" &
    frameId < snap_frame + ceiling(timeInTackleBox * 10)) |>
  mutate(
    delta_x = x - lag_half_sec_x,
    delta_y = y - lag_half_sec_y
  ) |>
  mutate(ang_rad = atan2(delta_y, delta_x)) |>
  mutate(ang_deg = NISTradianTOdeg(ang_rad)) |>
  mutate(turn = case_when(
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(26 - x) < abs(26 - lag_half_sec_x)) ~ "inside",
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(157 / 6 - x) > abs(157 / 6 - lag_half_sec_x)) ~ "outside",
    .default = "vert"
  )) |>
  group_by(gameId, playId, nflId) |>
  mutate(direction_break = case_when(
    any(turn == "inside") & any(turn == "outside") ~ "both",
    any(turn == "inside") & (!any(turn == "outside")) ~ "inside",
    any(turn == "outside") & (!any(turn == "inside")) ~ "outside",
    all(turn == "vert") ~ "vert",
  )) |>
  ungroup() |>
  mutate(start_break = case_when(
    (turn == direction_break & direction_break != "vert") &
      (playDirection == "right") ~ y - new_abs_yardline,
    (turn == direction_break & direction_break != "vert") &
      (playDirection == "left") ~ y - new_abs_yardline
  )) |>
  group_by(gameId, playId, nflId) |>
  mutate(start_break_play = fn_first(start_break, na_rm = TRUE)) |>
  slice_head(n = 1) |>
  ungroup()


slots_2_test <- slots_test |>
  left_join(slot_mvt_test |> select(gameId, playId, nflId, direction_break, start_break_play),
    by = join_by(gameId, playId, nflId)
  ) |>
  filter(!is.na(direction_break))

# cbs ---------------------------------------------------------------------

cbs_test <- joined_test |>
  filter(!is.na(pff_primaryDefensiveCoverageMatchupNflId)) |>
  filter(frameType == "SNAP", pff_defensiveCoverageAssignment == "MAN") |>
  select(gameId, playId, cb_x = x, cb_y = y, primary_cover = pff_primaryDefensiveCoverageMatchupNflId)

slots_3_test <- left_join(slots_2_test, cbs_test,
  by = join_by(gameId, playId, nflId == primary_cover), multiple = "first"
) |>
  mutate(cb_align_x = ifelse(cb_x > 157 / 6, cb_x - x, x - cb_x), cb_align_y = cb_y - y)



slots_predict_final_test <- slots_3_test |>
  select(
    direction_break, club, yardsToGo, absoluteYardlineNumber,
    offenseFormation, receiverAlignment, dist_football, dist_outside, cb_align_x,
    cb_align_y
  )

slots_predict_break_final_test <- slots_3_test |>
  select(
    start_break_play, club, yardsToGo, absoluteYardlineNumber,
    offenseFormation, receiverAlignment, dist_football, dist_outside, cb_align_x,
    cb_align_y
  ) |>
  filter(!is.na(start_break_play))
