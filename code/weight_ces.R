# Set Up -----------------------------------------------------------------------

setwd("your/directory/here")

library(tidyverse)
library(readxl) # Remember: must install separately via `install.packages("readxl")`

# Reading data -----------------------------------------------------------------

## 2020 Cooperative Election Study
pols_4716_github_url <- "https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/"
ces_2020 <- readRDS(url(paste0(pols_4716_github_url, "ces_2020_opin.rds")))

# Overall Trump vote -----------------------------------------------------------

ces_2020 %>%
  summarise(trump_vote = mean(vote_choice == "Trump", na.rm = TRUE))

# Adjusted Trump vote ----------------------------------------------------------

ces_2020_vote <- ces_2020 %>%
  filter(!is.na(vote_choice), !is.na(weight_vv)) %>%
  summarise(trump_vote_uwtd = mean(vote_choice == "Trump", na.rm = TRUE),
            trump_vote_wtd = weighted.mean(vote_choice == "Trump", w = weight_vv, na.rm = TRUE))
ces_2020_vote

# Adjusted Trump vote by race --------------------------------------------------

ces_2020_race_vote <- ces_2020 %>%
  group_by(race5) %>%
  filter(!is.na(vote_choice), !is.na(weight_vv)) %>%
  summarise(trump_vote_uwtd = mean(vote_choice == "Trump", na.rm = TRUE),
            trump_vote_wtd = weighted.mean(vote_choice == "Trump", w = weight_vv, na.rm = TRUE)) %>%
  arrange(desc(trump_vote_wtd - trump_vote_uwtd))

ces_2020_race_vote %>%
  ggplot(aes(y=race5)) +
  geom_bar(aes(x=trump_vote_uwtd), stat="identity", fill="red") +
  scale_x_continuous(limits = c(0, 0.75)) +
  labs(x="Trump Vote %", y="Racial Group")

ces_2020_race_vote %>%
  ggplot(aes(y=race5)) +
  geom_bar(aes(x=trump_vote_uwtd), stat="identity", fill="red") +
  geom_bar(aes(x=trump_vote_wtd), stat="identity", fill="red", alpha=0.5) +
  scale_x_continuous(limits = c(0, 0.75)) +
  labs(x="Trump Vote % (After Weighting)", y="Racial Group")

# Validation --------------------------------------------------------------

## Note: the `sheet` argument below reads in a specific tab in a spreadsheet
cnn_2020_race_vote <- read_excel("data/cnn_2020_exitpolls.xlsx", sheet = "race")

## Select only the variables we actually need
ces_2020_race_vote <- ces_2020_race_vote %>%
  select(race5, ces_trump_vote = trump_vote_wtd) 
cnn_2020_race_vote <- cnn_2020_race_vote %>%
  select(race5, cnn_trump_vote = trump_vote_prop)

## Replace TODO below with the correct join key
## Hint: it's just one variable!
join_key <- TODO

## Left join (keep all rows in CES)
ces_2020_race_vote %>%
  left_join(cnn_2020_race_vote, by = join_key)

## Right join (what's different now?)
ces_2020_race_vote %>%
  right_join(cnn_2020_race_vote, by = join_key)

## Full join (what's different now?)
ces_2020_race_vote %>%
  full_join(cnn_2020_race_vote, by = join_key)

## Inner join (what's different now?)
ces_2020_race_vote %>%
  inner_join(cnn_2020_race_vote, by = join_key)

## Visualize differences
viz_data <- ces_2020_race_vote %>%
  # replace TODO below with the correct join type
  TODO(cnn_2020_race_vote, by = join_key) %>%
  mutate(across(is.numeric, ~.x*100))

viz_data %>%
  ggplot(aes(y=race5)) +
  geom_point(aes(x=ces_trump_vote), color="blue", size=3, shape=15) +
  geom_point(aes(x=cnn_trump_vote), color="red", size=3, shape=16) +
  geom_segment(aes(x=cnn_trump_vote, xend=ces_trump_vote, 
                   y=race5, yend=race5), color="black") +
  labs(x="Trump Vote %", y="Racial Group",
       title="Comparison of 2020 Trump Vote Estimates",
       subtitle="Red dots are CNN | Blue squares are CES")

## Visualize the differences, slightly nicer
viz_data %>%
  rename(CES=ces_trump_vote, CNN=cnn_trump_vote) %>%
  pivot_longer(names_to="source", values_to="estimate", -race5) %>%
  ggplot(aes(y=race5, x=estimate, fill=source)) +
  geom_bar(stat="identity", position=position_dodge(width=1)) +
  geom_text(aes(label=round(estimate, 1)), 
            color="white", position=position_dodge(width=1), hjust=1.5) +
  scale_fill_manual(values=c("darkblue","red"), name="Source:") +
  labs(x="Trump Vote %", y="Racial Group",
       title="Comparison of 2020 Trump Vote Estimates") +
  theme_minimal() +
  theme(legend.position = "top")

# Attitudes by race ------------------------------------------------------------

ces_2020 %>%
  group_by(race5) %>%
  filter(!is.na(gun_control), !is.na(weight_common)) %>%
  summarise(support_gun_control = mean(gun_control == "Support"),
            support_gun_control_wtd = weighted.mean(gun_control == "Support", w = weight_common)) %>%
  ## Tip: this sorts your rows by the difference in descending order
  arrange(desc(support_gun_control_wtd - support_gun_control))

ces_2020 %>%
  group_by(race5) %>%
  filter(!is.na(abortion), !is.na(weight_common)) %>%
  summarise(support_abortion = mean(abortion == "Support"),
            support_abortion_wtd = weighted.mean(abortion == "Support", w = weight_common)) %>%
  arrange(desc(support_abortion_wtd - support_abortion))

ces_2020 %>%
  group_by(race5) %>%
  filter(!is.na(border), !is.na(weight_common)) %>%
  summarise(support_border = mean(border == "Support"),
            support_border_wtd = weighted.mean(border == "Support", w = weight_common)) %>%
  arrange(desc(support_border_wtd - support_border))



