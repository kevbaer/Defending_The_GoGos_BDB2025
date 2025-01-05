# Mailroom ----------------------------------------------------------------
library(tidyverse)
library(patchwork)

# Graph #1 ----------------------------------------------------------------
data <- tribble(
  ~Year, ~Zone_Rate, ~Slot_Rate,
  2015, 0.626, 0.631,
  2016, 0.629, 0.645,
  2017, 0.627, 0.628,
  2018, 0.656, 0.673,
  2019, 0.620, 0.673,
  2020, 0.658, 0.670,
  2021, 0.693, 0.677,
  2022, 0.730, 0.682,
  2023, 0.736, 0.697
)


a <- data |>
  ggplot() +
  aes(data, x = Year, y = Zone_Rate) +
  geom_point(color = "#7570B3", size = 3) +
  geom_line(color = "#7570B3", linewidth = 1.3) +
  theme_bw() +
  scale_y_continuous(limit = c(.55, .8), labels = scales::percent) +
  scale_x_continuous(n.breaks = 9) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 15)) +
  ylab("Zone Rate") +
  ggtitle("% of Pass Plays with Zone Coverage by Year") +
  theme(title = element_text(size = 16.25))


b <- data |>
  ggplot() +
  aes(data, x = Year, y = Slot_Rate) +
  geom_point(color = "#1B9E77", size = 3) +
  geom_line(color = "#1B9E77", linewidth = 1.3) +
  theme_bw() +
  scale_y_continuous(limit = c(.55, .8), labels = scales::percent) +
  scale_x_continuous(n.breaks = 9) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 15)) +
  ylab("Slot Rate") +
  ggtitle("% of Plays with Slot Receiver(s) by Year") +
  theme(title = element_text(size = 16.25))


b + a
