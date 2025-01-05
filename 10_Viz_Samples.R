source("01_Data_Manip.R")
source("09_makeVizFunc.R")
# Sample Play -------------------------------------------------------------
#
# plays_824 <- plays_filtering |>
#   filter(gameId == 2022091113 & playId == 824)
# tracking_824 <- joined |>
#   filter(gameId == 2022091113 & playId == 824)

# plays_857 <- plays_filtering |>
#   filter(gameId == 2022100201 & playId == 1271)
# tracking_857 <- joined |>
#   filter(gameId == 2022100201 & playId == 1271)

plays_2876 <- plays_filtering |>
  filter(gameId == 2022100905 & playId == 2876)
tracking_2876 <- joined |>
  filter(gameId == 2022100905 & playId == 2876)
#
# tracking_2353 <- joined |>
#   filter(gameId == 2022091108 & playId == 2353)
#
# plays_2353 <- plays_filtering |>
#   filter(gameId == 2022091108 & playId == 2353)

plays_1386 <- plays_filtering |>
  filter(gameId == 2022100206 & playId == 1386)
tracking_1386 <- joined |>
  filter(gameId == 2022100206 & playId == 1386)


# Viz ---------------------------------------------------------------------



at_snap <- slots_3 |>
  filter(gameId == 2022100206 & playId == 1386) |>
  mutate(.pred = "outside") |>
  mutate(.break = case_when(
    displayName == "DeAndre Carter" ~ 2.36,
    displayName == "Gerald Everett" ~ 4.58,
    .default = NA
  )) |>
  mutate(deets = glue(
    "player: {displayName}
    cb_align_x: {format(cb_align_x, digits = 3)}
    cb_align_y: {format(cb_align_y, digits = 3)}
    dist_to_football: {format(dist_football, digits = 3)}
    dist_to_outside_WR: {format(dist_outside, digits = 3)}

    direction prediction: {.pred}
    depth prediction: {.break}"
  ))

tracking_1386_up <- tracking_1386 |>
  left_join(at_snap)

makeViz(tracking_1386_up, plays_1386, "LAC", "HOU", "#0080C6", "#03202f", 2022, 4,
  frameStart = 1, frameEnd = 93, yardlow = 55, yardhigh = 105, endFreeze = 50, annotate = TRUE,
  toSave = TRUE, saveName = "13_presnap_anim_1386"
)

makeViz(tracking_1386_up, plays_1386, "LAC", "HOU", "#0080C6", "#03202f", 2022, 4,
  frameStart = 83, yardlow = 55, yardhigh = 105, endFreeze = 50
)


# makeViz(tracking_857, plays_857, "ATL", "CLE", "#a71930", "#FF3C00", 2022, 4,
#         yardlow = 25, yardhigh = 120-35)

# makeViz(tracking_824, plays_824, "TB", "DAL", "#a71930", "#0076B6", 2022, 5,
#         yardlow = 70)

# makeViz(tracking_2353, plays_2353, "TEN", "NYG", "#0076B6", "#a71930", 2022, 3)

# Direction ---------------------------------------------------------------

tracking_2876_O <- tracking_2876 |>
  filter((wasRunningRoute == TRUE | club == "football") | displayName == "Bailey Zappe") |>
  mutate(is_slot = if_else(displayName == "Jakobi Meyers", TRUE, NA)) |>
  group_by(gameId, playId, nflId) |>
  mutate(snap_frame = case_when(
    frameType == "SNAP" ~ frameId
  )) |>
  mutate(snap_frame = max(snap_frame, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    delta_x = x - lag_half_sec_x,
    delta_y = y - lag_half_sec_y
  ) |>
  mutate(ang_rad = atan2(delta_y, delta_x)) |>
  mutate(ang_deg = NISTradianTOdeg(ang_rad)) |>
  mutate(turn = if_else((is_slot), case_when(
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(26 - x) < abs(26 - lag_half_sec_x)) ~ "inside",
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(157 / 6 - x) > abs(157 / 6 - lag_half_sec_x)) ~ "outside",
    .default = "vert"
  ), NA)) |>
  mutate(deets = if_else(is_slot, glue(
    "player: {displayName}
     direction: {turn}"
  ), NA))
makeViz(tracking_2876_O, plays_2876, "NE", "DET", "#002244", "#0076B6", 2022, 5,
  yardhigh = 45, frameEnd = 145, startFreeze = 20, endFreeze = 20, annotate = TRUE,
  toSave = TRUE, saveName = "17_Meyers_direction"
)

# Depth -------------------------------------------------------------------

tracking_2876_2 <- tracking_2876 |>
  filter((wasRunningRoute == TRUE | club == "football") | displayName == "Bailey Zappe") |>
  mutate(is_slot = if_else(displayName == "Jakobi Meyers", TRUE, NA)) |>
  group_by(gameId, playId, nflId) |>
  mutate(snap_frame = case_when(
    frameType == "SNAP" ~ frameId
  )) |>
  mutate(snap_frame = max(snap_frame, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    delta_x = x - lag_half_sec_x,
    delta_y = y - lag_half_sec_y
  ) |>
  mutate(ang_rad = atan2(delta_y, delta_x)) |>
  mutate(ang_deg = NISTradianTOdeg(ang_rad)) |>
  mutate(turn = if_else((is_slot & (frameType == "AFTER_SNAP" &
    frameId < snap_frame + ceiling(timeInTackleBox * 10))), case_when(
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(26 - x) < abs(26 - lag_half_sec_x)) ~ "inside",
    (((ang_deg < 45) & (ang_deg > -45)) | ((ang_deg > 135) | (ang_deg < -135))) &
      (lag_half_sec_s > 1) & (abs(157 / 6 - x) > abs(157 / 6 - lag_half_sec_x)) ~ "outside",
    .default = "vert"
  ), NA)) |>
  group_by(nflId) |>
  mutate(direction_break = case_when(
    any(turn == "inside") & any(turn == "outside") ~ "both",
    any(turn == "inside") & (!any(turn == "outside", na.rm = TRUE)) ~ "inside",
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
  group_by(nflId) |>
  mutate(start_break_play = fn_first(start_break, na_rm = TRUE)) |>
  ungroup() |>
  mutate(deets = if_else(!is.na(start_break), glue(
    "player: {displayName}
     Depth of Break: {start_break}"
  ), NA))


makeViz(tracking_2876_2, plays_2876, "NE", "DET", "#002244", "#0076B6", 2022, 5,
  yardhigh = 45, frameEnd = 142, startFreeze = 20, endFreeze = 20, annotate = TRUE
)
