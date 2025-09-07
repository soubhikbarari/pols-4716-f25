library(ggplot2)

potusdata <- read.csv("data/potusdata.csv")

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