# -----------------------------
# IJC445 – Composite PM2.5 Visualization
# Sheffield Tinsley
# -----------------------------

# Load packages ----------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggridges)
library(openair)

# Load and clean PM2.5 data ------------------------------------------

# Path to folder containing all PM2.5 CSV files
data_path <- "Dataset/PM"

# Read all CSV files and combine into a single data frame
files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

air_all <- files %>%
  lapply(read_csv) %>%
  bind_rows()

# Clean and parse datetime
air_all <- air_all %>%
  mutate(
    # remove trailing "Z" from datetime string if present
    datetime_clean = gsub("Z", "", datetimeUtc),
    datetime       = ymd_hms(datetime_clean, quiet = TRUE)
  ) %>%
  # keep only rows with valid datetime
  filter(!is.na(datetime))


# Graph 1 – Daily PM2.5 heatmap by month -----------------------------

# Aggregate to daily mean PM2.5
daily_pm25 <- air_all %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    pm25   = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# Create a complete sequence of dates to show missing days explicitly
all_dates <- tibble(
  date = seq.Date(from = min(daily_pm25$date),
                  to   = max(daily_pm25$date),
                  by   = "day")
)

daily_pm25_full <- all_dates %>%
  left_join(daily_pm25, by = "date")

# Add day-of-month and month labels (Nov–Oct)
daily_pm25_month <- daily_pm25_full %>%
  mutate(
    day   = mday(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  mutate(
    month = factor(
      month,
      levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
                 "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
      ordered = TRUE
    )
  )

# Heatmap of daily PM2.5 by month
ggplot(daily_pm25_month, aes(x = day, y = 1, fill = pm25)) +
  geom_tile(color = "white") +
  facet_wrap(~ month, ncol = 3) +
  scale_fill_gradientn(
    colours  = c("#F1EEF6", "#DF65B0", "#980043"),
    na.value = "grey90"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title    = "Daily PM2.5 by day of month – Sheffield Tinsley",
    subtitle = "Grey tiles indicate days with no measurement",
    x        = "Day of month",
    y        = NULL,
    fill     = "PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey90", colour = NA)
  )


# Graph 2 – Monthly PM2.5 ridgeline plot -----------------------------

pm25_monthly <- air_all %>%
  filter(parameter == "pm25") %>%          # keep PM2.5 only
  filter(!is.na(datetime)) %>%
  mutate(
    month = factor(
      month(datetime),
      levels = 1:12,
      labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct")
    )
  )

# Ridgeline density plot of PM2.5 distributions by month
ggplot(pm25_monthly, aes(x = value, y = month, fill = month)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  labs(
    title    = "Monthly PM2.5 Distribution – Sheffield Tinsley",
    subtitle = "Ridgeline plot showing seasonal patterns in particulate pollution",
    x        = "PM2.5 (µg/m³)",
    y        = "Month"
  ) +
  theme_minimal()


# Load and clean wind data -------------------------------------------

wind_raw <- read.csv("Dataset/WIND/wind.csv")

# Remove header rows and keep first three columns (time, speed, direction)
wind_clean <- wind_raw[-c(1, 2), 1:3]

names(wind_clean) <- c("time", "wind_speed_10m", "wind_direction_10m")

wind_clean <- wind_clean %>%
  mutate(
    time               = as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "GMT"),
    wind_speed_10m     = as.numeric(wind_speed_10m),
    wind_direction_10m = as.numeric(wind_direction_10m)
  )

# Convert to openair-friendly format:
#   date = datetime, ws = wind speed (m/s), wd = wind direction (degrees)
wind_df <- wind_clean %>%
  transmute(
    date = time,
    wind_speed  = wind_speed_10m / 3.6,   # convert from km/h to m/s
    wind_direction   = wind_direction_10m
  )


# Merge PM2.5 and wind data at hourly level --------------------------

pm_hourly <- air_all %>%
  transmute(
    date = as.POSIXct(datetime_clean),
    pm25 = value
  )

pm_wind_hourly <- pm_hourly %>%
  inner_join(wind_df, by = "date")


# Graph 3 – PM2.5 pollution rose -------------------------------------

pollutionRose(
  pm_wind_hourly,
  pollutant  = "pm25",
  ws         = "wind_speed",
  wd         = "wind_direction",
  key.header = "PM2.5 (µg/m³)"
)


# Graph 4 – Polar plot of PM2.5 by wind speed and direction ----------

polarPlot(
  pm_wind_hourly,
  pollutant = "pm25",
  x         = "wind_speed",
  wd        = "wind_direction",
  main      = "PM2.5 vs wind (Sheffield Tinsley)"
)
