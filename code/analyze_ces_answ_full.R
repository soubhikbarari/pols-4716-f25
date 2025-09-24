# How do I run lines of code?
# - Mac: Click on line and press ⌘ and Enter together
# - PC: Click on line and press Ctrl and Enter together

# Set Up -----------------------------------------------------------------------

setwd("/Users/soubhikbarari/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025/Repo")

library(tidyverse)

# Reading and checking data ----------------------------------------------------

## Full path just to be safe 
ces_2020 <- readRDS("~/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025/Repo/data/ces_2020_sub.rds")

## If you're already in the working directory, don't need full path
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

ces_2020_wrangled <- ces_2020 %>%
  select(inputstate, votereg, voted = CC20_401) %>%
  # note: this is among all voters
  mutate(turned_out = case_when(voted == 5 ~ 1, 
                                votereg != "Yes" ~ 0,
                                .default = 0)) %>%
  group_by(inputstate) %>%
  summarise(turnout = mean(turned_out))
head(ces_2020_wrangled)
View(ces_2020_wrangled)

## Visualization
ces_2020_wrangled %>%
  ggplot(aes(x=turnout, y=inputstate)) +
  geom_bar(stat = "identity")

## Slightly nicer visualization

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

mean(is.na(ces_2020$CC20_401))

dem_turnout_trump <- ces_2020 %>%
  select(inputstate, voted = CC20_401, party = CC20_433a, pres_vote = CC20_410) %>%
  # keep those registered to vote
  filter(!is.na(voted)) %>%
  # keep those who provided an answer to their vote choice
  filter(!is.na(pres_vote)) %>%
  # keep Democrats
  filter(party == 1) %>%
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
  summarise(turnout = mean(turned_out, na.rm = T),
            trumpvote = mean(pres_vote == "Donald J. Trump (Republican)", na.rm = T))

turnout_trump %>%
  arrange(desc(turnout)) %>%
  ggplot(aes(x = trumpvote, y = turnout)) +
  geom_point() +
  labs(title = "Turnout and Trump Vote Choice (2020)",
       subtitle = paste0("Correlation:", cor(turnout_trump$trumpvote, turnout_trump$turnout)))

## Slightly nicer visualization
ces_2020 %>%
  select(inputstate, voted = CC20_401, pres_vote = CC20_410) %>%
  mutate(turned_out = voted == 5) %>%
  group_by(inputstate) %>%
  summarise(turnout = mean(turned_out, na.rm = T),
            trumpvote = mean(pres_vote == "Donald J. Trump (Republican)", na.rm = T),
            samplesize = n()) %>%
  arrange(desc(turnout)) %>%
  ggplot(aes(x = trumpvote, y = turnout)) +
  labs(title = "Turnout and Trump Vote Choice (2020)",
       subtitle = paste0("Correlation:", 
                         ### round to the nearest tenth place
                         round(cor(turnout_trump$trumpvote, turnout_trump$turnout), 2))) +
  ### make points a little more transparent, size by number of respondents
  geom_point(aes(size = samplesize), alpha = 0.5) +
  ### add text labels
  geom_text(aes(label = inputstate), size = 3) +
  ### add a smoothing line
  geom_smooth(method = "lm") +
  ### 'minimal ink' design
  theme_minimal() +
  ### make legend less obtrusive
  theme(legend.position = "bottom")

## Even nicer
install.packages("ggrepel")
library(ggrepel)

ces_2020 %>%
  select(inputstate, voted = CC20_401, pres_vote = CC20_410) %>%
  mutate(turned_out = voted == 5) %>%
  group_by(inputstate) %>%
  summarise(turnout = mean(turned_out, na.rm = T),
            trumpvote = mean(pres_vote == "Donald J. Trump (Republican)", na.rm = T),
            samplesize = n()) %>%
  arrange(desc(turnout)) %>%
  ggplot(aes(x = trumpvote, y = turnout)) +
  labs(title = "Turnout and Trump Vote Choice (2020)",
       subtitle = paste0("Correlation:", 
                         ### round to the nearest tenth place
                         round(cor(turnout_trump$trumpvote, turnout_trump$turnout), 2))) +
  ### add a reference line (Trump won in state vs. not)
  geom_vline(xintercept = 0.5, lty = 2) +
  ### make points a little more transparent, size by number of respondents
  geom_point(aes(size = samplesize), alpha = 0.5) +
  ### add EASIER TO READ text labels
  geom_text_repel(aes(label = inputstate), size = 3) +
  ### add a smoothing line
  geom_smooth(method = "lm") +
  ### 'minimal ink' design
  theme_minimal() +
  ### make legend less obtrusive
  theme(legend.position = "bottom")

  