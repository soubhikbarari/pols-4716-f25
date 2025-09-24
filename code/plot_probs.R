library(tidyverse)


# Bernoulli ---------------------------------------------------------------

data.frame(
  party = c("Democrat", "Republican"),
  prob = c(0.43, 0.57)
) %>%
  ggplot(aes(x=party, y=prob)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x="x", y="P(Party=x)")

# Binomial ----------------------------------------------------------------

## Probability Mass Function
p <- 0.8
n <- 10
x <- 0:n
prob_x <- dbinom(x, size = n, prob = p)

tibble(x, prob_x) %>%
  ggplot(aes(x=x, y=prob_x)) +
  geom_bar(stat="identity") +
  labs(x="x", y="P(X=x)") +
  labs(title="Binomial Distribution",
        subtitle=paste("n =", n, "| p =", p)) +
  theme_classic()

## Samples
draws <- rbinom(100, size = 1000, prob = 0.42)
draws

tibble(approval=draws) %>%
  mutate(approval=draws/1000) %>%
  ggplot(aes(x=approval)) +
  geom_histogram() +
  theme_bw()


# Normal Distribution -----------------------------------------------------

tibble(x=seq(-5, 5, by=0.01), 
       y=dnorm(seq(-5, 5, by=0.01))) %>%
  ggplot(aes(x=x, y=y)) +
  geom_point(color="grey") +
  theme_classic() +
  labs(x="x", y="p(x)") +
  theme(axis.text = element_blank(),
        axis.ticks.y = element_blank())

x_vals <- seq(-6, 6, by = 0.01)

## base dist
p <- tibble(x = x_vals,
            y = dnorm(x_vals, mean = 0, sd = 1)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "grey") +
  theme_classic() +
  annotate("text", x = 0, y = 0.5, label = "N(0,1)",
           color = "grey", fontface = "bold") +
  labs(x = "x", y = "p(x)") +
  theme(axis.text = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank())
p

## add second dist
p <- p +
  geom_point(data = tibble(x = x_vals,
                           y = dnorm(x_vals, mean = 1, sd = 2)),
             aes(x = x, y = y),
             color = "red") +
  annotate("text", x = 1.89, y = 0.28, label = "N(1,4)",
           color = "red", fontface = "bold")
p

## add third dist
p <- p +
  geom_point(data = tibble(x = x_vals,
                           y = dnorm(x_vals, mean = -3, sd = 0.5)),
             aes(x = x, y = y),
             color = "lightblue") +
  annotate("text", x = -4.79, y = 0.58, label = "N(-3,1/4)",
           color = "lightblue", fontface = "bold")
p



