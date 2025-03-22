library(tidyverse)
library(glue) # needed to make the title dynamic

# How to recreate climate temperature spirals in R with ggplot2 (CC218), 2022,
# https://riffomonas.org/code_club/2022-06-02-climate-spiral

data_url <-   "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

read_csv(data_url, skip=1, na="***")

# t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na = "***") %>% 
t_diff <- read_csv(data_url, skip=1, na="***") %>% 
  select(year=Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to="month", values_to="t_diff") %>% 
  drop_na() %>% 
  mutate(month = factor(month, levels = c(month.abb)))

# t_diff %>% tail(n=12)

# last_dec <- t_diff %>% 
#   filter(month == "Dec") %>% 
#   mutate(year = year + 1,
#          month = "last_Dec")
# 
next_jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")
# 
# t_diff
# last_dec
# next_jan

# bind_rows(last_dec, t_diff, next_jan) %>% count(month)


t_data <- bind_rows(t_diff, next_jan) %>% 
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1)
         #this_year = year == max(year)) 

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 12, # 12 o'clock position
  y = c(1.5, 2.0),
  labels = c("1.5\u00B0C", "2.0\u00B0C")
)

month_labels <- tibble(
  x=1:12,
  labels = month.abb,
  y = 2.5
)

t_data %>% 
  ggplot(aes(x=month_number, y=t_diff, group=year, color = year)) + 
  geom_hline(yintercept = c(1.5, 2.0), color = "red") +
  geom_line() +
  geom_point(data = annotation, aes(x=month_number, y=t_diff, color=year),
             inherit.aes = FALSE, size = 2) + # puts a point at end of spiral line
  geom_label(data = temp_lines, aes(x=x, y=y, label = labels), 
             color = "red", fill = "black", label.size = 0,
             inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color = "white",
            angle = seq(360 - 360/12, 0, length.out = 12)) +
  geom_text(aes(x=1, y=-2,  label = max(year))) +
  scale_x_continuous(breaks=1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 2.5),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_color_viridis_c(breaks = seq(1880, 2020, 20),
                        guide = "none") + 
  # coord_cartesian(xlim=c(1,12)) +
  coord_polar() +
  labs(x = NULL,
       y = NULL,
       title = glue("Global temperature change since (1880-{max(t_data$year)})")) +
  theme(
    panel.background = element_rect(fill = "black", size=1) ,
    plot.background = element_rect(fill = "#444444", color = "#444444"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 13),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white", size = 13),
    plot.title = element_text(color = "white", hjust = 0.5, size = 13),
  )

ggsave("figures/climate_spiral.png", width =8, height=4.5)
    

  

