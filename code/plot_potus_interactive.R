library(ggplot2)
library(plotly)
library(htmlwidgets)

potusdata <- read.csv("potusdata.csv")

potusplot <- ggplot(data = potusdata, aes(x = growth, y = presvote)) +
  geom_point(aes(color = incpty)) +
  geom_smooth(method = "lm") +
  labs(x = "Growth in per-capita disposable income",
       y = "Incumbent party share of the 2-party vote",
       y = "Incumbent party share of the 2-party vote",
       color = "Incumbent Party",
       title = "Economic Voting in Presidential Elections")

potus_interactive <- ggplotly(potusplot, tooltip = "text")
potus_interactive
saveWidget(as_widget(potus_intergctive), "potus_interactive.html", selfcontained=F)