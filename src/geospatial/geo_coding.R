library(ggmap)
library(tidyverse)

zip.df <- read_csv("./codes/input/KEN_ALL_ROME.CSV",
                   locale = locale(encoding = "cp932"))

fujieda.df <-
  zip.df[zip.df$PREF_ROME == "SHIZUOKA KEN" &
           zip.df$ADD1_ROME == "FUJIEDA SHI", ]

rm(zip.df)
view(fujieda.df)

fujieda.df[, "ADDRESS"] <-
  paste0(fujieda.df$PREF, fujieda.df$ADD1, fujieda.df$ADD2)

# samp <- fujieda.df[sample(1:nrow(fujieda.df), 1), "ADDRESS"][[1]]
samp <- as.character(fujieda.df[sample(1:nrow(fujieda.df), 1), "ZIP"][[1]])

register_google(key = "")  # Check the gcp api-key in credential
LonLatData <- geocode(samp, source = "google")
LonLatData <- geocode("426-0004", source = "google")

GMapData <-
  get_googlemap(
    center = c(lon = LonLatData[1, 1][[1]], lat = LonLatData[1, 2][[1]]),
    zoom = 16,
    size = c(640, 640),
    scale = 2,
    format = "png8",
    maptype = "hybrid",
    language = "jpn",
    sensor = FALSE,
    messaging = FALSE,
    urlonly = FALSE,
    filename = "ggmapTemp",
    color = "color",
    force = FALSE
  )
ggmap(GMapData)

route.df <-
  route(
    from = "東京都中野区落合駅",
    to = "東京都新宿区新宿駅",
    mode = "walking",
    structure = "legs"
  )

qmap("東京都中野区落合駅, 東京都新宿区新宿駅", zoom = 13) +
  geom_path(data = route.df, 
            aes(x = start_lon, y = start_lat),
            colour = "deeppink", 
            size = 1, 
            lineend = "round")
