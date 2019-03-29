library(tidyverse)

zip.df <- read_csv("./codes/input/KEN_ALL_ROME.CSV",
                   locale = locale(encoding = "cp932"))

fujieda.df <-
  zip.df[zip.df$PREF_ROME == "SHIZUOKA KEN" &
           zip.df$ADD1_ROME == "FUJIEDA SHI", ]

rm(zip.df)
view(fujieda.df)
