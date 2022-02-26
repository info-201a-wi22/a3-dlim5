library(ggplot2)
library(tidyverse)
library(usdata)

incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# Summary information
## 1: Average prison admission rate for each race per year since 2000
average_prison_admission <- incarceration_trends %>% 
  filter(year > 1999) %>%
  group_by(year) %>%
  select(aapi_prison_adm_rate, black_prison_adm_rate, latinx_prison_adm_rate, 
         native_prison_adm_rate, white_prison_adm_rate) %>%
  summarize(avg_aapi_prison_adm_rate = mean(aapi_prison_adm_rate, na.rm = TRUE),  
            avg_black_prison_adm_rate = mean(black_prison_adm_rate, na.rm = TRUE),
            avg_latinx_prison_adm_rate = mean(latinx_prison_adm_rate, na.rm = TRUE),
            avg_native_prison_adm_rate = mean(native_prison_adm_rate, na.rm = TRUE),
            avg_white_prison_adm_rate = mean(white_prison_adm_rate, na.rm = TRUE))

## 2: Average jail population rate for "people of color" and white per region since 2000
average_jail_population <- incarceration_trends %>%
  filter(year > 1999) %>%
  group_by(year) %>%
  select(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate,
         native_jail_pop_rate, white_jail_pop_rate) %>%
  summarize(avg_aapi_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE),
            avg_black_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE),
            avg_latinx_pop_rate = mean(latinx_jail_pop_rate, na.rm = TRUE),
            avg_native_pop_rate = mean(native_jail_pop_rate, na.rm = TRUE),
            avg_white_pop_rate = mean(white_jail_pop_rate, na.rm = TRUE))

## 3: Top 5 states with the highest black prison admission count
highest_black_adm_count_state <- incarceration_trends %>%
  filter(year > 1999) %>%
  arrange(desc(black_prison_adm)) %>%
  select(year, state, black_prison_adm) %>%
  distinct(state) %>%
  head(5)

## 4: Total jail population count for black and white in 2018
total_jail_pop_count <- incarceration_trends %>%
  filter(year == 2018) %>%
  mutate(black_total_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  mutate(white_total_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  select(black_total_jail_pop, white_total_jail_pop) %>%
  head(1)

## 5: Total population count for black and white in 2018
total_pop_count <- incarceration_trends %>%
  filter(year == 2018) %>%
  mutate(black_population = sum(black_pop_15to64)) %>%
  mutate(white_population = sum(white_pop_15to64)) %>%
  select(black_population, white_population) %>%
  head(1)

## Chart one
reshaped_data1 <- data.frame(x = average_prison_admission$year,
                             y = c(average_prison_admission$avg_aapi_prison_adm_rate,
                                   average_prison_admission$avg_black_prison_adm_rate,
                                   average_prison_admission$avg_latinx_prison_adm_rate,
                                   average_prison_admission$avg_native_prison_adm_rate,
                                   average_prison_admission$avg_white_prison_adm_rate),
                             Race = c(rep("Asian/Pacific", nrow(average_prison_admission)),
                                       rep("Black", nrow(average_prison_admission)),
                                       rep("Latin", nrow(average_prison_admission)),
                                       rep("Native", nrow(average_prison_admission)),
                                       rep("White", nrow(average_prison_admission))))

chart_one <- ggplot(reshaped_data1) +
  geom_col(mapping = aes(x = x, y = y, fill = Race)) + 
  labs(title = "Time Trend of Average Prison Admission Rate",
       x = "Year",
       y = "Average Prison Admission Rate")

## Chart two
population_count <- incarceration_trends %>%
  filter(year == 2018) %>%
  mutate(black_jail_pop_ratio = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64)) %>%
  mutate(white_jail_pop_ratio = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64)) %>%
  select(white_jail_pop_ratio, black_jail_pop_ratio) %>%
  head(1)

reshaped_data2 <- data.frame(Race = c(rep("Jail Population Ratio (White)"),
                                      rep("Jail Population Ratio (Black)")),
                             Ratio = c(population_count$white_jail_pop_ratio,
                                   population_count$black_jail_pop_ratio))

chart_two <- ggplot(reshaped_data2, mapping = aes(x = Race, y = Ratio, fill = Race)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Comparison of Ratio of Population of Jail to Total Population for Black and White",
       x = "Race",
       y = "Jail / Total Population Ratio")

## Map
black_adm_count <- incarceration_trends %>%
  filter(year > 1999) %>%
  group_by(state) %>%
  summarize(black_admission_count = sum(black_prison_adm, na.rm = TRUE))

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  mutate(state = state2abbr(state)) %>%
  left_join(black_adm_count, by = "state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

map <- ggplot(state_shape) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_admission_count),
    color = "white",
    size = .1
  ) +
  coord_map() + 
  scale_fill_continuous(low = "#FFA07A", high = "#CD5C5C") +
  labs(title = "Total Prison Admission of Black",
       fill = "Black Prison Admission Count") + 
  blank_theme
map

