# How do I run lines of code?
# - Mac: Click on line and press ⌘ and Enter together
# - PC: Click on line and press Ctrl and Enter together

# Set Up -----------------------------------------------------------------------

## Set your working directory here
## setwd("~/Path/To/Your/Folder/")
  
library(tidyverse)

# Reading and checking data ----------------------------------------------------

## Make sure that ces_2020_sub.rds is downloaded to the /data subfolder in your class folder
## (https://github.com/soubhikbarari/pols-4716-f25/blob/main/data/ces_2020_sub.rds)
ces_2020 <- readRDS("data/ces_2020_sub.rds")

## Looking at our data
ncol(ces_2020)
nrow(ces_2020)
View(ces_2020) # Nice, variables are labelled (thanks .rds file!)

sum(is.na(ces_2020$CC20_401))

class(ces_2020$birthyr)
class(ces_2020$inputstate)
class(ces_2020$race_other)

sum(is.na(ces_2020$CC20_401)) # count of missing values
mean(is.na(ces_2020$CC20_401)) # percent of missing values

# 1. What was voter turnout like state-by-state? -------------------------------

## Wrangling
ces_2020_wrangled <- ces_2020 %>%
  select(inputstate, voted = CC20_401) %>%
  # note: voted is NA if votereg is No, so subsetting among registered voters
  filter(!is.na(voted)) %>% 
  mutate(turned_out = voted == 5) %>% 
  group_by(inputstate) %>%
  summarise(turnout = mean(turned_out))
head(ces_2020_wrangled)
View(ces_2020_wrangled)

## Visualization
ces_2020_wrangled %>%
  ggplot(aes(x=turnout, y=inputstate)) +
  geom_bar(stat = "identity")

## Slightly nicer visualization (advanced)

library(forcats) ## Included in tidyverse
library(scales) ## Included with ggplot2

ces_2020_wrangled %>%
  ## Let's sort states by turnout
  mutate(inputstate = fct_reorder(inputstate, turnout)) %>%
  ggplot(aes(x=turnout, y=inputstate)) +
  geom_bar(stat = "identity", fill="darkblue") +
  geom_vline(xintercept = 0.5, color = "white") +
  ## Let's label percents
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Estimated Turnout in the 2020 Pres Election",
       subtitle = "Source: Cooperative Election Study (2020)",
       x = "Turnout by State", y = "",
       caption = "Author: Soubhik Barari") +
  theme_minimal()

## To save (many output formats available!)
state_turnout_plot <- ces_2020_wrangled %>%
  ggplot(aes(y=inputstate, x=turnout)) +
  geom_bar(stat = "identity")
ggsave(state_turnout_plot, filename = "ces_2020_state_turnout.pdf")
ggsave(state_turnout_plot, filename = "ces_2020_state_turnout.png")

# 2. What was Democratic turnout for Trump like state-by-state? ----------------

dem_turnout_trump <- ces_2020 %>%
  select(inputstate, voted = CC20_401, party = CC20_433a, pres_vote = CC20_410) %>%
  # filter(TODO) %>%
  mutate(turned_out_for_trump = voted == 5 & pres_vote == "Donald J. Trump (Republican)") %>%
  group_by(inputstate) %>%
  summarise(pct_trump_turnout = 100 * mean(turned_out_for_trump))

dem_turnout_trump %>%
  ggplot(aes(x = pct_trump_turnout, y = inputstate)) +
  geom_bar(stat = "identity") +
  labs(x="% turnout for Trump (among registered Dem voters)")

quantile(dem_turnout_trump$pct_trump_turnout)

# 3. Are state turnout and Trump vote related? ---------------------------------

turnout_trump <- ces_2020 %>%
  select(inputstate, voted = CC20_401, pres_vote = CC20_410) %>%
  mutate(turned_out = voted == 5) %>%
  group_by(inputstate) %>%
  # summarise(turnout = mean(turned_out, na.rm = T),
  #           trumpvote = TODO)

turnout_trump %>%
  arrange(desc(turnout)) %>%
  ggplot(aes(x = trumpvote, y = turnout)) +
  geom_point() +
  labs(title = "Turnout and Trump Vote Choice (2020)",
       subtitle = paste0("Correlation:", TODO))

