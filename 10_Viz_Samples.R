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

tracking_2353 <- joined |>
  filter(gameId == 2022091108 & playId == 2353)

plays_2353 <- plays_filtering |>
  filter(gameId == 2022091108 & playId == 2353)

plays_1386 <- plays_filtering |>
  filter(gameId == 2022100206 & playId == 1386)
tracking_1386 <- joined |>
  filter(gameId == 2022100206 & playId == 1386)
# plays_1570 <- plays_filtering |>
#   filter(gameId == 2022091109 & playId == 1570)
#
# tracking_1570 <- joined |>
#   filter(gameId == 2022091109 & playId == 1570)

# Viz ---------------------------------------------------------------------


# makeViz(tracking_2876, plays_2876, "NE", "DET", "#002244", "#0076B6", 2022, 5,
#          yardhigh = 65, frameStart = 1)

at_snap <- slots_3 |>
  filter(gameId == 2022100206 & playId == 1386) |>
  mutate(deets = glue(
    "player: {displayName},
    cb_align_x: {format(cb_align_x, digits = 3)}
    cb_align_y: {format(cb_align_y, digits = 3)}
    dist_to_football: {format(dist_football, digits = 3)}
    dist_to_outside_WR: {format(dist_outside, digits = 3)}"
  ))

tracking_1386_up <- tracking_1386 |>
  left_join(at_snap)

makeViz(tracking_1386_up, plays_1386, "LAC", "HOU", "#0080C6", "#03202f", 2022, 6,
  frameStart = 1, frameEnd = 93, yardlow = 55, yardhigh = 105, endFreeze = 50, annotate = TRUE,
  toSave = TRUE, saveName = "anim_1386"
)

makeViz(tracking_1386_up, plays_1386, "LAC", "HOU", "#0080C6", "#03202f", 2022, 6,
  frameStart = 83, yardlow = 55, yardhigh = 105, endFreeze = 50
)


# makeViz(tracking_857, plays_857, "ATL", "CLE", "#a71930", "#FF3C00", 2022, 4,
#         yardlow = 25, yardhigh = 120-35)

# makeViz(tracking_824, plays_824, "TB", "DAL", "#a71930", "#0076B6", 2022, 5,
#         yardlow = 70)

makeViz(tracking_2353, plays_2353, "TEN", "NYG", "#0076B6", "#a71930", 2022, 3)


# makeViz(tracking_1570, plays_1570, "JAX", "WAS", "#0076B6", "#a71930", 2022, 6)
