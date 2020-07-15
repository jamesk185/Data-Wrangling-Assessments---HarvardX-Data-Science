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


# 2.3 Web Scraping
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])
html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])
tab_1 <- html_table(nodes[[10]]) %>% select(-X1) %>% .[-1,] %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- html_table(nodes[[19]]) %>% .[-1,] %>% setNames(c("Team", "Payroll", "Average"))
full_join(tab_1, tab_2, by="Team")

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
i <- read_html(url)
tab <- html_nodes(i, "table")
tab
html_table(tab[[1]], fill=TRUE)
html_table(tab[[2]], fill=TRUE)
html_table(tab[[3]], fill=TRUE)
html_table(tab[[4]], fill=TRUE)
head(html_table(tab[[5]], fill=TRUE))


# 3.3 String Processing Part 3
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
names <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls %>% setNames(names) %>% .[-1,]
head(polls)
index <- str_detect(polls$remain, "%")
sum(index)
which(!index)
polls_1 <- polls %>% .[-c(16, 75, 82, 111),]
str_detect(polls_1$remain, "%")