## ---- sensor-map
library(sugrrants)
library(tidyverse)
library(ggmap)

sensor_loc <- rwalkr::pull_sensor()
qmplot(x = Longitude, y = Latitude, data = sensor_loc,
  colour = I("#d95f02"), size = I(4))

## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 14)
  )
}
theme_set(theme_remark())

## ---- selected-sensor
sensors <- c("State Library", "Flagstaff Station",
  "Flinders Street Station Underpass")

sensor_loc %>% 
  mutate(Selected = ifelse(Sensor %in% sensors, TRUE, FALSE)) %>% 
  qmplot(
    x = Longitude, y = Latitude, data = .,
    colour = Selected, shape = Selected, size = I(4)
  ) +
  scale_colour_brewer(palette = "Dark2")

## ---- ped-data
pedestrian <- as_tibble(rwalkr::run_melb(year = 2016))
subdat <- pedestrian %>% 
  filter(Sensor %in% sensors) %>% 
  mutate(Day = wday2(Date, label = TRUE))
subdat

## ---- ts-plot
# conventional time series plot
subdat %>% 
  ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom") +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat %>% 
  ggplot(aes(x = Time, y = Count, group = Date, 
    colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ Day, 
    labeller = labeller(Sensor = label_wrap_gen(20))
  ) +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- flinders-2016
# calendar plot for flinders street station
flinders <- subdat %>% 
  filter(Sensor == "Flinders Street Station Underpass")

flinders_cal <- flinders %>%
  frame_calendar(x = Time, y = Count, date = Date)
flinders_cal

## ---- flinders-2016-plot
p_flinders <- flinders_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line()
prettify(p_flinders)

## ---- flinders-free
# calendar plot for flinders street station using local scale
flinders_cal_free <- flinders %>% 
  frame_calendar(x = Time, y = Count, date = Date, scale = "free")

p_flinders_free <- flinders_cal_free %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line()
prettify(p_flinders_free, size = 3, label.padding = unit(0.15, "lines"))

## ---- overlay
# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Count, Date)

sensor_cols <- c(
  "#1b9e77" = "#1b9e77", 
  "#d95f02" = "#d95f02", 
  "#7570b3" = "#7570b3"
) # Dark2
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[1]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[2]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[3]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols),
    labels = c(
      "State Library", 
      "Flagstaff Station",
      "Flinders Street Station Underpass"
    ),
    guide = "legend"
  ) +
  theme(legend.position = "bottom")
prettify(p_three, size = 3, label.padding = unit(0.15, "lines"))

## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor) %>% 
  frame_calendar(x = Time, y = Count, date = Date, nrow = 2)

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line(aes(colour = Sensor)) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom")
prettify(p_facet, label = NULL)

## ---- scatterplot
# lagged scatterplot for flinders street station in the daily calendar format
flinders_cal_day <- flinders %>% 
  mutate(Lagged_Counts = dplyr::lag(Count)) %>% 
  frame_calendar(x = Count, y = Lagged_Counts, date = Date, 
    width = 0.95, height = 0.8)

p_flinders_day <- flinders_cal_day %>% 
  ggplot(aes(x = .Count, y = .Lagged_Counts, group = Date)) +
  geom_point(size = 0.5, alpha = 0.6)
prettify(p_flinders_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- boxplot
# boxplots for hourly counts across all the sensors in 2016 December
pedestrian_dec <- pedestrian %>% 
  filter(Date >= as.Date("2016-12-01")) %>% 
  frame_calendar(
    x = Time, y = Count, date = Date, width = 0.97, height = 0.97
)
p_boxplot <- pedestrian_dec %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Count, group = Date_Time),
    outlier.size = 0.3, width = 0.004, position = "identity",
    colour = "grey50"
  ) +
  geom_smooth(
    aes(.Time, .Count, group = Date), 
    se = FALSE, method = "loess"
  )
prettify(p_boxplot, label = c("label", "text", "text2"))