library(tidyverse)
library(ggthemes)

# Source: https://www.kaggle.com/drgilermo/nba-players-stats
# Details: https://www.basketball-reference.com/about/glossary.html

# Player info
players <- read_csv("data/Players.csv") %>% select(-1)
dim(players)
glimpse(players)

# Player stats
player_stats <- read_csv("data/player_data.csv") 
dim(player_stats)
glimpse(player_stats)

# Season stats
season_stats <- read_csv("data/Seasons_Stats.csv", guess_max = 20000) %>% select(-1)
dim(season_stats)
glimpse(season_stats)
summary(season_stats)


# join datasets

players %>% anti_join(player_stats, by = c("Player" = "name"))
player_stats %>% anti_join(players, by = c("name" = "Player"))

# players who passed away(?) have a star attached to their name in the players data frame
players %>% filter(str_detect(Player, "\\*"))
# remove this flag
players <- players %>%
  mutate(Player = str_replace(Player, "\\*", "")) %>%
  rename(
    name = Player,
    college = collage
    )
players %>% filter(str_detect(name, "\\*"))

player_stats %>% anti_join(players, by = "name")
players %>% anti_join(player_stats, by = "name")

names(players)
names(player_stats)



# there are cases missing in both data frame, let's try a full join
ft_to_cm <- 30.48
lbs_to_kg <- 0.453592

transform_ft_to_cm <- function(ft, with_inches = TRUE, separator = "-", round_values = TRUE) {
  if (with_inches == FALSE) {
    ft <- as.numeric(ft)
    cm <- ft * ft_to_cm
  } else {
    tokens <- str_split_fixed(ft, separator, 2)
    ft <- as.numeric(tokens[1, 1])
    inches <- as.numeric(tokens[1, 2])
    inches <- ifelse(is.na(inches), 0, inches)
    cm <- (ft + inches / 12) * ft_to_cm
    if (round_values) {
      cm <- round(cm)
    }
  }
  return(cm)
}

player_data <- player_stats %>% 
  full_join(players, by = "name") %>%
  mutate(
    college = ifelse(!is.na(college.x), college.x, college.y),
    birth_date = lubridate::mdy(birth_date),
    born = ifelse(!is.na(born), born, lubridate::year(birth_date)),
    height = ifelse(!is.na(height.y), height.y, transform_ft_to_cm(height.x)),
    weight = ifelse(!is.na(weight.y), weight.y, weight.x * lbs_to_kg),
    position_prime = str_split_fixed(position, "-", 2)[, 1],
    position_prime = ifelse(position_prime == "", NA, position_prime)
    ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  rename(birth_year = born)

# check player positions
player_data %>% count(position_prime, position)

# format season stats data frame
season_stats <- season_stats %>%
  mutate(
    Player = str_replace(Player, "\\*", ""),
    position_prime = str_split_fixed(Pos, "-", 2)[, 1],
    position_prime = ifelse(position_prime == "", NA, position_prime)
    ) %>%
  rename(name = Player,
         year = Year,
         position = Pos,
         age = Age,
         team = Tm)

# join player data with season stats
player_data %>% anti_join(season_stats, by = "name")
# 611 players with no season stats

# 3P were introduced in 1979/80
start_3p <- 1980

theme_set(theme_base())

# 3 point attempts and rate over time
season_stats %>%
  filter(year >= start_3p) %>%
  group_by(year, position_prime) %>%
  summarize(`3PA` = sum(`3PA`),
            `3P%` = mean(`3P%`, na.rm = TRUE)) %>%
  ggplot(aes(year, `3P%`, col = position_prime)) +
  geom_line() +
  scale_y_continuous(label = scales::percent) +
  geom_line() +
  theme_fivethirtyeight() +
  labs(title = "3-point percentage per player", 
       subtitle = "Between 1995 and 1997 the 3-point line was shortened\nincreasing shot accuracy",
       col = "Position"
  )

season_stats %>%
  filter(year >= start_3p) %>%
  group_by(year, position_prime) %>%
  summarize(`3PA` = sum(`3PA`) / n(),
            `3P%` = mean(`3P%`, na.rm = TRUE)) %>%
  ggplot(aes(year, `3PA`, col = position_prime)) +
  geom_line() +
  theme_fivethirtyeight() +
  labs(title = "3-point attempts per player", 
       subtitle = "Between 1995 and 1997 the 3-point line was shortened\nresulting in more attempts",
       col = "Position"
       )

# 


