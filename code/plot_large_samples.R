
set.seed(4716) ## Fun fact: this ensures that simulations are replicable

X                  <- rbinom(1, size=1000, prob=0.42)
X_repeated_100     <- rbinom(100, size=1000, prob=0.42)
X_repeated_10k     <- rbinom(10000, size=1000, prob=0.42)
X_10k_repeated_100 <- rbinom(100, size=10000, prob=0.42)

## Plot layered distributions one after the other
## (Fun fact: you don't actually have to specify data right away in ggplot!)
p <- ggplot() + 
  # true population line
  geom_vline(xintercept = 0.42, color="red", linetype="solid") +
  annotate("label", x = 0.42, y = 80, label = "True Percent Approval", color="red") +
  # formatting
  scale_x_continuous(limits = c(0.32, 0.52)) +
  scale_y_continuous(limits = c(0, 82)) +
  labs(
    x = "Percent Approval",
    y = "Density",
    title = "Sampling Distributions"
  ) +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
p

p <- p + 
  # 1 realized sample
  annotate("point", x = X/1000, y = 0.01, size = 2) +
  annotate("text", x = 0.46, y = 0.01, label = "1 Sample of 1000")
p

p <- p +
  # 100 samples (n=1000)
  geom_histogram(aes(x = X_repeated_100/1000, y = after_stat(density)),
               fill = "blue", alpha = 0.3, bins = 100) +
  annotate("text", x = 0.49, y = 24, label = "100 Samples of 1000", color="blue")
p

p <- p + 
  # 10k samples (n=1000)
  geom_histogram(aes(x = X_repeated_10k/1000, y = after_stat(density)),
                 fill = "green", bins = 100, alpha = 0.75) +
  annotate("text", x = 0.36, y = 20, label = "10k Samples of 1000", color="green")
p
p +
  stat_function(fun = dnorm,
                args = list(mean = 0.42, sd = sqrt(0.42 * (1 - 0.42) / 1000)),
                color = "darkgreen", size = 1, alpha = 0.75)

p <- p +
  # 100 samples (n=10k)
  geom_histogram(aes(x = X_10k_repeated_100/10000, y = after_stat(density)),
                 fill = "orange", bins = 100, alpha = 0.75) +
  annotate("text", x = 0.37, y = 60, label = "100 Samples of 10k", color="orange")
p
 





