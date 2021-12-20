library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(gganimate)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)
library(caret)
library(purrr)
library(FNN)
library(stargazer)
library(dplyr)

# station_list=c("BNA",
#                "PHX",
#                "FAT",
#                "BZN",
#                "DEN",
#                "SEA",
#                "LAS",
#                "PDX",
#                "CLE",
#                "ONT",
#                "ITO",
#                "SLC",
#                "LIT")
station_list=c("BNA",
               "TUS",
               "PHX",
               "PDX",
               "SFO",
               "PARC",
               "MTJ",
               "MIA",
               "LAX")
# station_list[1]

weather <- data.frame()
weather_2020 <- data.frame()
weather_2019 <- data.frame()
weather_2018 <- data.frame()
weather_2017 <- data.frame()
weather_2016 <- data.frame()

for (i in 1:9) {
  weather_2020 <- rbind(weather_2020 , riem_measures(station = station_list[i], date_start = "2020-12-12", date_end = "2020-12-25") %>%
                     dplyr::select(station, valid, tmpf, p01i, sknt) %>%
                     replace(is.na(.), 0) %>%
                     mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
                     group_by(station, interval60) %>%
                     summarize(Temperature = max(tmpf),
                               Precipitation = sum(p01i),
                               Wind_Speed = max(sknt)))
}
weather_2020 <-
  weather_2020 %>%
  mutate(Year = '2020')

for (i in 1:9) {
  weather_2019 <- rbind(weather_2019, riem_measures(station = station_list[i], date_start = "2019-12-12", date_end = "2019-12-25") %>%
                          dplyr::select(station, valid, tmpf, p01i, sknt) %>%
                          replace(is.na(.), 0) %>%
                          mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
                          group_by(station, interval60) %>%
                          summarize(Temperature = max(tmpf),
                                    Precipitation = sum(p01i),
                                    Wind_Speed = max(sknt)))
}
weather_2019 <-
  weather_2019 %>%
  mutate(Year = '2019')

for (i in 1:9) {
  weather_2018 <- rbind(weather_2018, riem_measures(station = station_list[i], date_start = "2018-12-12", date_end = "2018-12-25") %>%
                          dplyr::select(station, valid, tmpf, p01i, sknt) %>%
                          replace(is.na(.), 0) %>%
                          mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
                          group_by(station, interval60) %>%
                          summarize(Temperature = max(tmpf),
                                    Precipitation = sum(p01i),
                                    Wind_Speed = max(sknt)))
}
weather_2018 <-
  weather_2018 %>%
  mutate(Year = '2018')

for (i in 1:9) {
  weather_2017 <- rbind(weather_2017, riem_measures(station = station_list[i], date_start = "2017-12-12", date_end = "2017-12-25") %>%
                          dplyr::select(station, valid, tmpf, p01i, sknt) %>%
                          replace(is.na(.), 0) %>%
                          mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
                          group_by(station, interval60) %>%
                          summarize(Temperature = max(tmpf),
                                    Precipitation = sum(p01i),
                                    Wind_Speed = max(sknt)))
}
weather_2017 <-
  weather_2017 %>%
  mutate(Year = '2017')

for (i in 1:9) {
  weather_2016 <- rbind(weather_2016, riem_measures(station = station_list[i], date_start = "2016-12-12", date_end = "2016-12-25") %>%
                          dplyr::select(station, valid, tmpf, p01i, sknt) %>%
                          replace(is.na(.), 0) %>%
                          mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
                          group_by(station, interval60) %>%
                          summarize(Temperature = max(tmpf),
                                    Precipitation = sum(p01i),
                                    Wind_Speed = max(sknt)))
}
weather_2016 <-
  weather_2016 %>%
  mutate(Year = '2016')

weather <-
  rbind(weather_2020, weather_2019, weather_2018, weather_2017, weather_2016)
write.csv(weather,"./data/weather_data_2016_2020.csv", row.names = TRUE)






