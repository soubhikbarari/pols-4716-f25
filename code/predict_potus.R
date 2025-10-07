library(ggplot2)
library(tidyverse)

# Download and read
potusvotes <- readRDS(url("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/potusvotes.rds"))

if (!file.exists("data/potusvotes.rds")) {
  # Check if file exists, otherwise save 
  saveRDS(potusvotes, file = "data/potusvotes.rds")
}

# Univariate regression ---------------------------------------------------
# PresVote ~ DPI

# Fit the model
model <- lm(presvote ~ dpi, data = potusvotes)
print(model)

# Save fitted or predicted values (y_hat)
potusvotes$presvotefit <- fitted(model)

# Plot fitted values
potusvotes %>%
  ggplot(aes(x=dpi)) +
  geom_line(aes(y=presvotefit), color="skyblue") +
  geom_point(aes(y=presvote)) +
  theme_classic()

# How good is this model?

# TODO

# Univariate regression (binary predictor) --------------------------------
# PresVote ~ IncRun

model.bin <- lm(presvote ~ incrun, data = potusvotes)
print(model.bin)
potusvotes$presvotefit2 <- fitted(model.bin)

potusvotes %>%
  group_by(incrun) %>%
  summarise(presvotefit2 = mean(presvotefit2),
            presvote = mean(presvote))

potusvotes %>%
  group_by(incrun) %>%
  summarise(presvote = mean(presvote)) %>%
  summarise(diff = diff(presvote))

# Multivariate regression -------------------------------------------------
# PresVote ~ IncRun + DPI

model.mtv <- lm(presvote ~ incrun + dpi, data = potusvotes)
potusvotes$presvotefit3 <- fitted(model.mtv)

# Multivariate regression (interactions) ----------------------------------
# PresVote ~ DPI + IncPty + (DPI x IncPty)

model4.intr <- lm(presvote ~ dpi + incpty + dpi:incpty, data = potusvotes) 
print(model4.intr)

## note: x1*x2 automatically expands to x1 + x2 + x1:x2
model4.intr <- lm(presvote ~ dpi*incpty, potusvotes) 
potusvotes$presvotefit4 <- fitted(model4.intr)

potusvotes %>%
  mutate(incpty = if_else(incpty=="D", "0 (Dem)", "1 (Rep)")) %>%
  ggplot(aes(x=dpi, group=incpty, color=incpty)) +
  geom_line(aes(y=presvotefit4), size=2) +
  geom_point(aes(y=presvote), size=2) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  theme(legend.position="top")



