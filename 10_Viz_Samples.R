source("09_makeVizFunc.R")
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


