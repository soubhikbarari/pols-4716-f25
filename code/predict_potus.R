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


# Categorical predictor ---------------------------------------------------

potusvotes <- potusvotes %>%
  mutate(dpi3 = case_when(dpi < quantile(dpi, 0.3) ~ "bad",
                          dpi < quantile(dpi, 0.6) ~ "ok",
                          dpi <= max(dpi) ~ "good")) %>%
  mutate(dpi3 = factor(dpi3, levels = c("bad","ok","good")))
head(potusvotes)

model.cat <- lm(presvote ~ dpi3, data = potusvotes)
model.cat

potusvotes$presvotefit.cat <- fitted(model.cat)

potusvotes %>%
  group_by(incpty) %>%
  summarise(presvotefit.cat = mean(presvotefit.cat),
            presvote = mean(presvote))

# Lagged outcome ----------------------------------------------------------

potusvotes <- potusvotes %>%
  mutate(presvote.last = lag(presvote))

model.lag <- lm(presvote ~ presvote.last, data = potusvotes) 
print(model.lag)

View(potusvotes)

# Quadratic terms ---------------------------------------------------------

model.quad <- lm(presvote ~ I(dpi^2), data = potusvotes)
potusvotes <- potusvotes %>% 
  mutate(dpi.sq = dpi^2)
model.quad <- lm(presvote ~ dpi.sq, data = potusvotes) 

potusvotes$presvotefit.quad <- fitted(model.quad)

potusvotes %>%
  ggplot(aes(x=dpi)) +
  geom_line(aes(y=presvotefit), size=1, color="skyblue") +
  geom_line(aes(y=presvotefit.quad), size=2, color="pink") +
  geom_point(aes(y=presvote), size=2) +
  theme_classic() +
  theme(legend.position="top", axis.title = element_text(size=14))

potusvotes %>%
  ggplot(aes(x=dpi^2)) +
  geom_line(aes(y=presvotefit), size=1, color="skyblue") +
  geom_line(aes(y=presvotefit.quad), size=2, color="pink") +
  geom_point(aes(y=presvote), size=2) +
  theme_classic() +
  theme(legend.position="top", axis.title = element_text(size=14))

# Prediction --------------------------------------------------------------

model.full <- lm(presvote ~ dpi + incpty + incrun + dpi:incpty + incrun:incpty, data = potusvotes) 
print(model.full)

predict(model.full, data.frame(dpi=1.11, incpty="R", incrun=0))

# Uncertainty -------------------------------------------------------------

## Coefficients
summary(model.full)

## Predicted Values
data_2028 <- data.frame(dpi=1.11, incpty="R", incrun=0)
predict(model.full, data_2028, interval = "confidence")

## Confidence intervals (and more customization)
library(broom) # install.packages("broom")

model.full.tidy <- tidy(model.full, conf.int = TRUE, conf.level = 0.95)

ggplot(model.full.tidy, aes(estimate, term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0) +
  geom_pointrange()

library(estimatr) # install.packages("sandwich")

model.robust <- lm_robust(presvote ~ dpi + incpty + incrun + dpi:incpty + incrun:incpty, 
                          data = potusvotes, se_type = "HC2") 
model.robust.tidy <- tidy(model.robust, conf.int = TRUE, conf.level = 0.95)

ggplot(model.robust.tidy, aes(estimate, term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0) +
  geom_pointrange()


