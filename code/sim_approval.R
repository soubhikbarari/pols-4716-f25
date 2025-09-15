library(dplyr)
library(ggplot2)

# Binomial Random Variable (Y)
# Across some n "trials", Y captures the number of "successes"
# assuming some probability p of success

# Visualizing Binomial Distributions -------------------------------------------

p  <- 0.8                        # Parameter: Success probability
n  <- 10                         # Parameter: Number of trials
y  <- 0:n                        # Possible successes
P_y <- dbinom(y, size=n, prob=p) # Probability mass function P(Y=y)

tibble(y, P_y) %>%
  ggplot(aes(x=y, y=P_y)) +
  geom_bar(stat="identity") +
  labs(x="y", y="P(Y=y)") +
  labs(title="Binomial Distribution",
       subtitle=paste("n =", n, "| p =", p)) +
  theme_classic()

# Simulating from Binomial Distributions ---------------------------------------
## Suppose we magically knew that Donald Trump had a population approval rating of 42%.  
## Equivalently, 0.42 of the population approves of Trump. 

set.seed(4716) ## Fun fact: this ensures that simulations are replicable

## Draw a random sample of 1000 respondents where Y = number who support Trump. 
Y <- rbinom(TODO, size=TODO, prob=TODO)

## How do we infer the proportion of the population who supports Trump from this sample?
prop_approve <- TODO

## 1. How can we simulate 100 repeated samples of 1000 respondents? 
Y_repeated_100 <- rbinom(TODO, size=TODO, prob=0.42)

## Visualize the resulting distribution of % approve.
tibble(approval=Y_repeated_100) %>%
  mutate(prop_approve=TODO) %>% ## Hint: Hint: same operation as on line 31!
  ggplot(aes(x=prop_approve)) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0.3, 0.5)) +
  theme_classic()

## 2. What about 10,000 repeated samples? 
Y_repeated_10k <- rbinom(TODO, size=TODO, prob=0.42)

## Visualize the resulting distribution of % approve. 
## How does it compare to before?
tibble(approval=Y_repeated_10k) %>%
  mutate(prop_approve=TODO) %>% ## Hint: same operation as on line 31!
  ggplot(aes(x=prop_approve)) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0.3, 0.5)) +
  theme_classic()

## 3. What about 100 repeated samples of 10,000 respondents?
Y_10k_repeated_100 <- rbinom(TODO, size=TODO, prob=0.42)

## Visualize the resulting distribution of % approve. 
## How does it compare to before?
tibble(approval=Y_10k_repeated_100) %>%
  mutate(prop_approve=TODO) %>% ## Hint: same operation as on line 31!
  ggplot(aes(x=prop_approve)) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits = c(0.3, 0.5)) +
  theme_classic()
