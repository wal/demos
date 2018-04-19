library(tidyverse)

# Step 1 : Import Data
premier_league_2017_2018_url <- "http://www.football-data.co.uk/mmz4281/1718/E0.csv"
data <- read.csv(premier_league_2017_2018_url)

# filter columns to just those we are interested in
data <- data %>% select(HomeTeam, AwayTeam, Date, FTR)

# Convert Date column (DD/MM/YYYY)
data$Date <- as.Date(data$Date, format="%d/%m/%y")

#  Step 2 : Tidy Data
home_data <- data %>% 
  select(HomeTeam, Date, FTR) %>%
  mutate(Points = case_when(
    .$FTR == 'H' ~ 3,
    .$FTR == 'D' ~ 1,
    .$FTR == 'A' ~ 0)) %>%
  rename(Team = HomeTeam)

away_data <- data %>%
  select(AwayTeam, Date, FTR) %>%
  mutate(Points = case_when(
    .$FTR == 'H' ~ 0,
    .$FTR == 'D' ~ 1,
    .$FTR == 'A' ~ 3)) %>%
  rename(Team = AwayTeam)

# Combine home and away data
season_data <- rbind(home_data, away_data) %>% arrange(Date) 

# Step 3 - Transform Data
season_data_cumulative <- season_data %>% 
  group_by(Team) %>% 
  mutate(cumulative_points = cumsum(Points))

# Step 4 - Visualise Data
man_city_data <- season_data_cumulative %>% filter(Team == 'Man City')

ggplot(season_data_cumulative, aes(Date, cumulative_points, group = Team)) + 
  geom_line(color = "#98999B", size =1) +
  geom_line(data = man_city_data, color = "#95D9FE", size = 3) +
  theme_minimal() +
  labs(y = "Points", x = "Date") +
  ggtitle("Man City - 2017/2018 points acumulation")