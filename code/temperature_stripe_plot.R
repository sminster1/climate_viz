library(tidyverse)
library(scales) # needed to use the "rescale()" function
library(glue) # needed to make the title dynamic

# temperature_stripe_plot.R
# Episode CC216

data_url <-   "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

read_csv(data_url, skip=1, na="***",
         show_col_types = FALSE) %>% # to quiet error
  select(year = Year, t_diff = 'J-D') %>% 
  drop_na()

t_data <- read_csv("data/GLB.Ts+DSST.csv", skip =1, na = "***",
                   show_col_types = FALSE) %>% # to quiet error
  select(year = Year, t_diff = 'J-D') %>% 
  drop_na()

t_data %>% 
  ggplot(aes(x = year, y = 1, fill = t_diff)) +
  geom_tile(show.legend = FALSE) +
  #  scale_fill_steps2(low="blue", mid="white", high="red",
  #                    n.breaks = 12)
  scale_fill_stepsn(colors=c("#08306B", "white", "#67000D"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    n.breaks = 12) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = seq(1890, 2020, 30)) +
  labs(title=glue("Global temperature change ({min(t_data$year)}-{max(t_data$year)})")) +
  theme_void() +
  theme(
    axis.text.x = element_text(color="white",
                               margin=margin(t=5, b=10, unit="pt")),
    plot.title = element_text(color="white",
                              margin=margin(b=5, t=10, unit="pt"),
                              hjust=0.05),
    plot.background = element_rect(fill = "black")
  )


ggsave("figures/warming_stripes.png", width = 8, height = 4.5)

