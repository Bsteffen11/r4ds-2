#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(readxl)
#
#
#
births <- read_excel("data/us_births_1994_2014.xlsx")
glimpse(births)
#
#
#
summary(births |> select(births, year))
#
#
#
#| cache: true
births_tibble <- births |>
  mutate(day_of_week = factor(
    day_of_week,
    levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
    ordered = TRUE
  ))
#
#
#
average_births <- births_tibble |>
  group_by(month, date_of_month) |>
  summarise(
    average_births = mean(births),
    .groups = "drop"
  )

average_births
#
#
#
average_births |>
  mutate(month = factor(month, levels = 1:12, labels = month.name)) |>
  ggplot(aes(x = date_of_month, y = month, fill = average_births)) +
  geom_tile() +
  scale_y_discrete(limits = rev(month.name)) +
  scale_fill_viridis_c(name = "Mean births") +
  labs(
    x = "Day of month",
    y = "Month"
  )
#
#
#
christmas_births <- births_tibble |>
  filter(month == 12, date_of_month == 25) |>
  select(year, christmas_births = births)

surrounding_births <- births_tibble |>
  filter(month == 12, date_of_month %in% c(20:24, 27:30)) |>
  group_by(year) |>
  summarise(
    baseline_births = mean(births),
    .groups = "drop"
  )

christmas_data <- christmas_births |>
  left_join(surrounding_births, by = "year") |>
  mutate(pct_of_baseline = 100 * christmas_births / baseline_births)
#
#
#
summary(christmas_data)
#
#
#
ggplot(christmas_data, aes(x = year, y = pct_of_baseline)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "December 25 births (% of baseline)"
  )
#
#
#
#
