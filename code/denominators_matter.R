library(tidyverse)

# Fun fact: using url(), you can read data directly from GitHub!
data <- readRDS(url("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/votes_2020_sample.rds"))

# This are REAL votes from 2020 in every county compiled by the MIT Election Lab! 
# (I created a 1% sample so the file isn't too big)
View(data)

# "I want to calculate the percent of Trump voters in each district within state"

# Note: in this example, we're going to switch from district to county just b/c 
# data is more easily available

# Option A. Percent of Trump voters -- out of all Trump voters in state (concentration)
data %>%
  group_by(state, county) %>%
  summarise(t_voters = sum(vote == "trump")) %>%
  group_by(state) %>%
  mutate(t_voters = t_voters / sum(t_voters))

# What does this tell us?

# Option B. Percent of Trump voters -- out of all voters in county (competition)
data %>%
  group_by(state, county) %>%
  summarise(t_voters = sum(vote == "trump")/n())

# What does this tell us?


