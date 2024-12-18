library(tidyverse)
library(nflverse)  

library(arrow)

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


tracking <- open_dataset("data/tracking_train.parquet")
games <- read_csv("data/games.csv")
plays <- read_csv("data/plays.csv") |> 
  select(-starts_with("pff_runConcept"), -qbKneel, -qbSneak, - rushLocationType)
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")



tracking_added <- tracking |>
  mutate(
    new_y = ifelse(playDirection == "left", 120 - x, x),
    new_x = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  ) |> 
  mutate(y = new_y, x = new_x) |> 
  select(-new_x, -new_y) |> 
  head(n = 10) |> 
  collect() 



tracking_sample <- tracking |> 
  slice_sample(n = 500) |> 
  collect()



adding_lag <- as_tibble(tracking) |> 
  group_by(gameId, playId, nflId) |> 
  mutate(lag_half_sec_x = lag(x, n = 5) , 
         lag_half_sec_y = lag(y, n = 5))
  


player_play_sample <- player_play |> 
  slice_sample(n = 10)



ftn <- load_ftn_charting(season = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass, starting_hash, 
         qb_location, is_screen_pass, is_motion, 
         is_trick_play,is_qb_out_of_pocket)



pbp <- load_pbp(2022) |> 
  select(play_id, old_game_id, game_id)

ftn_added <- left_join(ftn, pbp, by = join_by(nflverse_game_id == game_id, nflverse_play_id == play_id)) |> 
  mutate(old_game_id = as.double(old_game_id))

plays_added <- left_join(plays, ftn_added, by = join_by(gameId == old_game_id, playId ==nflverse_play_id))
