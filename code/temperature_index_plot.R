library(tidyverse)

data_url <-   "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

read_csv(data_url, skip=1, na="***")

read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, t_diff = `J-D`, )

