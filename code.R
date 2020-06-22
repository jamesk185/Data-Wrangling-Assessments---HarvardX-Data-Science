# 2.2 Combining Tables
library(tidyverse)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
awards_players_2016 <- AwardsPlayers %>% filter(yearID == 2016) %>%
  select(playerID)
top_names_IDonly <- top_names %>% select(playerID)
intersect(top_names_IDonly, awards_players_2016)
setdiff(awards_players_2016, top_names_IDonly)
