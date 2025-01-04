library(gganimate)
library(glue)

makeViz <- function(trackingDataset, playDataset, club1, club2, club1col, club2col,
                    yearNum, weekNum, yardlow = 0, yardhigh = 120, frameStart = NA,
                    frameEnd = NA, toSave = FALSE, saveName = NA, endFreeze = 8, annotate = FALSE) {
  start_event <- trackingDataset |>
    distinct(gameId, playId, frameId, event) |>
    filter(event %in% c("ball_snap", "autoevent_ballsnap", "snap_direct")) |>
    group_by(gameId, playId) |>
    slice_min(frameId) |>
    distinct(gameId, playId, start_frame = frameId)

  end_event <- trackingDataset |>
    distinct(gameId, playId, frameId, event) |>
    filter(event %in% c(
      "fumble", "fumble_defense_recovered", "fumble_offense_recovered",
      "out_of_bounds", "safety", "tackle", "touchdown",
      "pass_outcome_incomplete", "pass_outcome_touchdown"
    )) |>
    group_by(gameId, playId) |>
    slice_min(frameId) |>
    select(gameId, playId, end_frame = frameId)

  frames <- start_event |>
    full_join(end_event, by = join_by(gameId, playId))

  if (is.na(frameStart)) {
    begin <- (frames |> pull(start_frame))[1]
  } else {
    begin <- frameStart
  }
  if (is.na(frameEnd)) {
    end <- (frames |> pull(end_frame))[1]
  } else {
    end <- frameEnd
  }
  length <- (end - begin) + 1 + endFreeze


  ex <- trackingDataset |>
    mutate(
      pt_color = case_when(
        if (annotate) {
          is_slot ~ "gold"
        },
        club == club1 ~ club1col,
        club == club2 ~ club2col,
        club == "football" ~ "#D3D3D3"
      ),
      pt_size = case_when(
        club == club1 ~ 3.8,
        club == club2 ~ 3.8,
        club == "football" ~ 2.5
      )
    )
  desc <- playDataset |>
    pull(playDescription) |>
    str_replace("\\)\\.", "\\)")

  qNum <- playDataset |>
    pull(quarter)

  anim <- ggplot() +
    annotate("text",
      y = seq(20, 100, 10),
      x = 10,
      color = "#000000",
      family = "sans",
      label = c("1 0", "2 0", "3 0", "4 0", "5 0", "4 0", "3 0", "2 0", "1 0"),
      angle = 270
    ) +
    annotate("text",
      y = seq(20, 100, 10),
      x = 40,
      color = "#000000",
      family = "sans",
      label = c("1 0", "2 0", "3 0", "4 0", "5 0", "4 0", "3 0", "2 0", "1 0"),
      angle = 90
    ) +
    annotate("text",
      y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
      x = 0,
      color = "#000000",
      label = "—",
      angle = 0
    ) +
    annotate("text",
      y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
      x = 160 / 3,
      color = "#000000",
      label = "—",
      angle = 0
    ) +
    annotate("text",
      y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
      x = 23.36667,
      color = "#000000",
      label = "–",
      angle = 0
    ) +
    annotate("text",
      y = setdiff(seq(10, 110, 1), seq(10, 110, 5)),
      x = 29.96667,
      color = "#000000",
      label = "–",
      angle = 0
    ) +
    annotate("segment",
      x = c(-Inf, Inf),
      xend = c(-Inf, Inf),
      y = 0,
      yend = 120,
      color = "#000000"
    ) +
    geom_hline(yintercept = seq(10, 110, 5), color = "#000000") +
    geom_point(
      data = filter(ex, frameId %in% {
        begin
      }:{
        end
      }), shape = 21,
      aes(160 / 3 - x, y, size = pt_size, fill = pt_color)
    ) +
    scale_size_identity() +
    scale_fill_identity() +
    transition_time(frameId) +
    ease_aes("linear") +
    coord_cartesian(ylim = c(yardlow, yardhigh), xlim = c(0, 160 / 3), expand = FALSE) +
    theme_minimal() +
    labs(
      title = glue("<span style = 'color:{club1col};'>**{club1}**</span> vs. <span style = 'color:{club2col};'>**{club2}**</span>, {yearNum} Week {weekNum}"),
      subtitle = str_c("Q{qNum}: ", desc)
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
      plot.title = ggtext::element_markdown(hjust = 0.5, size = 12),
      text = element_text(family = "sans", color = "#000000"),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    {
      if (annotate) {
        geom_label(
          data = filter(ex, frameId %in% {
            begin
          }:{
            end
          }),
          aes(160 / 3 - x + 1, 64, label = deets, fill = "lightblue", size = 3)
        )
      }
    }

  tot <- yardhigh - yardlow

  a1 <- animate(
    nframes = length,
    anim,
    width = 700,
    height = (40 * tot) / 3,
    fps = 10,
    end_pause = endFreeze,
    res = 105,
  )

  if (toSave) {
    anim_save(glue("{saveName}.gif"), a1)
  }

  return(a1)
}
