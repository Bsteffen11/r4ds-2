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
#| cache: true
births_tibble <- read_excel("data/us_births_1994_2014.xlsx") |>
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
      ordered = TRUE
    )
  )
#
#
#
births_model <- lm(births ~ factor(year) + factor(month) + day_of_week,
                   data = births_tibble)

calendar_resid <- births_tibble |>
  mutate(pct_resid = resid(births_model) / mean(births) * 100) |>
  filter(!(month == 2 & date_of_month == 29)) |>
  group_by(month, date_of_month) |>
  summarize(mean_pct_resid = mean(pct_resid), .groups = "drop") |>
  mutate(calendar_date = as.Date(paste("2001", month, date_of_month,
                                       sep = "-")))

calendar_resid |>
  ggplot(aes(x = calendar_date, y = mean_pct_resid)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_line(linewidth = 0.4) +
  annotate("text", x = as.Date("2001-01-01"), y = -21, label = "Jan. 1",  size = 3, hjust = 0.2) +
  annotate("text", x = as.Date("2001-07-04"), y = -23, label = "July 4",  size = 3, hjust = 0.5) +
  annotate("text", x = as.Date("2001-12-25"), y = -36, label = "Dec. 25", size = 3, hjust = 1) +
  scale_x_date(date_labels = "%b.", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Fewer Babies Are Born on Holidays",
    subtitle = "US births relative to year-, month-, and weekday-adjusted baseline (1994–2014)",
    x = NULL,
    y = "% vs. adjusted baseline",
    caption = "Source: FiveThirtyEight/SSA (1994–2014)"
  ) +
  theme_minimal()
#
#
#
#| cache: true
basketball_tibble <- read_excel("data/nba_recruits.xlsx") |>
  mutate(
    tier = factor(
      tier,
      levels = c("Never played", "Brief career", "Solid career",
                 "All-Star level", "Superstar")
    ),
    recruit_group = factor(
      recruit_group,
      levels = c("#1–10", "#11–25", "#26–50", "#51–100", "Outside top 100")
    )
  )
#
#
#
basketball_tibble |>
  filter(!is.na(rank), !is.na(top_mean_wa)) |>
  ggplot(aes(x = rank, y = top_mean_wa, color = tier)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_text(
    data = \(d) d |> filter(name %in% c("LeBron James", "Kevin Durant",
                                        "Klay Thompson")),
    aes(label = name),
    nudge_y = 1.2, hjust = 0,
    size = 3, show.legend = FALSE
  ) +
  geom_text(
    data = \(d) d |> filter(name %in% c("Draymond Green", "Gilbert Arenas")),
    aes(label = name),
    nudge_y = 1.2, hjust = 1,
    size = 3, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Never played"   = "gray80",
    "Brief career"   = "#74add1",
    "Solid career"   = "#fee090",
    "All-Star level" = "#f46d43",
    "Superstar"      = "#d73027"
  )) +
  labs(
    title = "Does High School Recruit Rank Predict NBA Stardom?",
    subtitle = "Among players who reached the NBA — rank predicts average careers but not superstars",
    x = "High school recruit rank (1 = top recruit)",
    y = "Peak Wins Added (best 5-season average)",
    color = "Career level",
    caption = "Source: The Pudding, 2019"
  ) +
  theme_minimal()
```
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
  select(year, christmas_births = births, day_of_week)

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
  geom_point(aes(color = day_of_week)) +
  labs(
    x = "Year",
    y = "December 25 births (% of baseline)"
  )
#
#
#
births_model <- lm(
  births ~ year + month + day_of_week,
  data = births_tibble
)

summary(births_model)$r.squared
#
#
#
births_adjusted <- births_tibble |>
  mutate(pct_resid = 100 * resid(births_model) / mean(births))

str(births_adjusted)
#
#
#
calendar_resid <- births_adjusted |>
  filter(!(month == 2 & date_of_month == 29)) |>
  group_by(month, date_of_month) |>
  summarise(
    mean_pct_resid = mean(pct_resid),
    .groups = "drop"
  ) |>
  mutate(calendar_date = as.Date(sprintf(
    "2001-%02d-%02d",
    month,
    date_of_month
  ))) |>
  arrange(calendar_date)

calendar_resid
#
#
#
holiday_dips <- calendar_resid |>
  filter(calendar_date %in% as.Date(c(
    "2001-01-01",
    "2001-11-22",
    "2001-12-25"
  ))) |>
  mutate(holiday = case_when(
    month == 1 ~ "New Year's Day",
    month == 11 ~ "Thanksgiving",
    month == 12 ~ "Christmas Day"
  ))

ggplot(calendar_resid, aes(x = calendar_date, y = mean_pct_resid)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = holiday_dips, color = "firebrick", size = 2) +
  geom_text(
    data = holiday_dips,
    aes(label = holiday),
    vjust = -0.8,
    color = "firebrick"
  ) +
  labs(
    x = "Calendar date",
    y = "Mean residual (% of overall mean births)"
  )
#
#
#
recruits <- read_excel("data/nba_recruits.xlsx")
glimpse(recruits)
#
#
#
summary(recruits |> select(
  rank,
  nba_mean_ws48,
  top_mean_wa,
  total_seasons,
  drafted
))
#
#
#
recruits |> count(tier)
#
#
#
#| cache: true
basketball_tibble <- recruits |>
  mutate(tier = factor(
    tier,
    levels = c(
      "Never played",
      "Brief career",
      "Solid career",
      "All-Star level",
      "Superstar"
    ),
    ordered = TRUE
  ), recruit_group = factor(
    recruit_group,
    levels = c(
      "#1–10",
      "#11–25",
      "#26–50",
      "#51–100",
      "Outside top 100"
    ),
    ordered = TRUE
  ))
#
#
#
basketball_tibble |>
  filter(!is.na(rank), !is.na(top_mean_wa)) |>
  ggplot(aes(x = rank, y = top_mean_wa, color = tier)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(
    data = basketball_tibble |>
      filter(!is.na(rank), !is.na(top_mean_wa)) |>
      slice_max(top_mean_wa, n = 4, with_ties = FALSE),
    aes(label = name),
    vjust = -0.8,
    show.legend = FALSE
  ) +
  labs(
    x = "Recruit rank",
    y = "Peak Wins Added",
    color = "Career tier"
  )
#
#
#
#
