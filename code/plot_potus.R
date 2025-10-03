library(ggplot2)

potusdata <- read.csv("data/potusdata.csv")

ggplot(data = potusdata, aes(x = growth, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = potusdata, aes(x = growth, y = presvote, color = incpty)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = potusdata, aes(x = growth, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="skyblue") +
  annotate("point", x=0.15, y=47.3, color="orange", size=5, shape=21) +
  labs(x = "Growth in per-capita disposable income (X)",
       y = "Incumbent party share of the 2-party vote (Y)",
       title = "Economic Voting in Presidential Elections") +
  theme_classic()

# Fit the model
model <- lm(presvote ~ growth, data = potusdata)

# Add fitted values to your dataset
potusdata$fit <- fitted(model)

# Plot with residuals as segments
ggplot(potusdata, aes(x = growth, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="skyblue") +
  annotate("point", x=0.15, y=47.3, color="orange", size=5, shape=21) +
  geom_segment(aes(xend = growth, yend = fit), alpha = 0.6, color = "red", size=2) +
  labs(
    x = "Growth in per-capita disposable income (X)",
    y = "Incumbent party share of the 2-party vote (Y)",
    title = "Economic Voting in Presidential Elections"
  ) +
  theme_classic()
