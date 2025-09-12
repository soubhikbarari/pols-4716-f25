
setwd("~/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025")

potusdata <- read.csv("data/potusdata.csv")

View(potusdata) # Look at your data!

library(ggplot2)

# From videos
ggplot(data = potusdata, aes(x = growth, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = potusdata, aes(x = growth, y = presvote, color = incpty)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = potusdata, aes(x = growth, y = presvote)) +
  geom_point(aes(color = incpty)) +
  geom_smooth(method = "lm") +
  labs(x = "Growth in per-capita disposable income",
       y = "Incumbent party share of the 2-party vote",
       y = "Incumbent party share of the 2-party vote",
       color = "Incumbent Party",
       title = "Economic Voting in Presidential Elections")

# One idea for an extension
p <- ggplot(data = potusdata, aes(x = electionyear, y = presvote)) +
  geom_point() +
  geom_line()
p
help(ggsave)

ggplot(data = potusdata, aes(x = electionyear, y = presvote, color = incpty)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(data = potusdata, aes(x = electionyear, y = presvote)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ incpty) + # create a horizontal grid
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = potusdata, aes(x = electionyear, y = presvote)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y = "Incumbent party share of the 2-party vote",
       y = "Incumbent party share of the 2-party vote",
       color = "Incumbent Party",
       title = "Economic Voting in Presidential Elections") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")
