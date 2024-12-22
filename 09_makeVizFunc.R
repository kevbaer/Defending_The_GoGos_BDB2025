library(gganimate)
library(glue)

makeViz <- function(trackingDataset, playDataset, club1, club2, club1col, club2col,
                    yearNum, weekNum, yardlow = 0, yardhigh = 120, 
                    toSave = FALSE, saveName = NA){


start_event <- trackingDataset |> 
  distinct(gameId, playId, frameId, event) |> 
  filter(event %in% c("ball_snap", "autoevent_ballsnap", "snap_direct")) |> 
  group_by(gameId, playId) |> 
  slice_min(frameId) |> 
  distinct(gameId, playId, start_frame = frameId)

end_event <- trackingDataset |> 
  distinct(gameId, playId, frameId, event) |> 
  filter(event %in% c("fumble", "fumble_defense_recovered", "fumble_offense_recovered",
                      "out_of_bounds", "safety", "tackle", "touchdown", 
                      "pass_outcome_incomplete")) |>
  group_by(gameId, playId) |> 
  slice_min(frameId) |> 
  select(gameId, playId, end_frame = frameId)

frames <- start_event |> 
  full_join(end_event, by = join_by(gameId, playId)) 

begin <- frames |> pull(start_frame)
end <- frames |> pull(end_frame)
length <- (end[1] - begin[1]) / 10


ex <- trackingDataset |> 
  mutate(
    pt_color = case_when(
      club == club1 ~ club1col,
      club == club2 ~ club2col,
      club == "football" ~ "white"
    ),
    pt_size = case_when(
      club == club1 ~ 2.8,
      club == club2 ~ 2.8,
      club == "football" ~ 1.4
    )
  )
desc <- playDataset |> 
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

qNum <- playDataset |> 
  pull(quarter)

anim <- ggplot()  +
  annotate("text", 
           y = seq(20, 100, 10),
           x = 10,
           color = "#000000",
           family = "sans",
           label = c("1 0", "2 0","3 0","4 0","5 0","4 0","3 0","2 0","1 0"),
           angle = 270) +
  annotate("text", 
           y = seq(20, 100, 10),
           x = 40,
           color = "#000000",
           family = "sans",
           label = c("1 0", "2 0","3 0","4 0","5 0","4 0","3 0","2 0","1 0"),
           angle = 90) +
  annotate("text", 
           y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
           x = 0,
           color = "#000000",
           label = "—",
           angle = 0) +
  annotate("text", 
           y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
           x = 160 / 3,
           color = "#000000",
           label = "—",
           angle = 0) +
  annotate("text", 
           y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
           x = 23.36667,
           color = "#000000",
           label = "–",
           angle = 0) +
  annotate("text", 
           y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
           x = 29.96667,
           color = "#000000",
           label = "–",
           angle = 0) +
  annotate("segment", 
           x = c(-Inf, Inf),
           xend = c(-Inf, Inf),
           y = 0,
           yend = 120,
           color = "#000000") +
  geom_hline(yintercept = seq(10, 110, 5), color = "#000000") +
  geom_point(data = filter(ex, frameId %in% {begin[1]}:{end[1]}), shape = 21,
             aes(160/3 - x, y, size = pt_size, fill = pt_color)) +
  scale_size_identity() +
  scale_fill_identity() +
  transition_time(frameId) +
  ease_aes("linear") +
  coord_cartesian(ylim = c(yardlow, yardhigh), xlim = c(0, 160 / 3), expand = FALSE) +
  theme_minimal() +
  labs(title = glue("<span style = 'color:{club1col};'>**{club1}**</span> vs. <span style = 'color:{club2col};'>**{club2}**</span>, {yearNum} Week {weekNum}"),
       subtitle = str_c("Q{qNum}: ", desc)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 12),
        text = element_text(family = "sans", color = "#000000"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


tot <- yardhigh - yardlow

a1 <- animate(
  anim,
  width = 700,
  height = (40 * tot) / 3,
  duration = {length},
  fps = 10,
  end_pause = 8,
  res = 105,
)

if(toSave){
  anim_save(glue("{saveName}.gif"), a1)
}

return(a1)
}







