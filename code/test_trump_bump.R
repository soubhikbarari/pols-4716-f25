

sample_size <- 2000

last_week_pct <- 0.381
this_week_pct <- 0.40

# Assume null hypothesis (Trump's true approval is exactly last week's)
# What *would* estimates look like?
null_ests <- rbinom(10000, sample_size, last_week_pct)
null_ests <- null_ests/sample_size

# Now how does that compare to our actual estimate? (one-sided)
tibble(null_est = null_ests) %>%
  ggplot(aes(x=null_est)) +
  geom_histogram() +
  scale_y_continuous(name = "") +
  scale_x_continuous(labels = scales::percent_format(1), 
                     limits = c(0.34, 0.42),
                     name = "Simulated Estimates under Null Distribution") +
  geom_vline(xintercept=this_week_pct, color="red") +
  annotate("label", x=this_week_pct, y=length(null_ests)/20, label="This Week's Estimate", color="red", angle=90) +
  theme_classic()

# Probability of this estimate or higher if null were true?
mean(null_ests >= this_week_pct)

# Two-sided test extension:
# probability of an estimate this far from last week in either direction?
tibble(null_est = null_ests) %>%
  ggplot(aes(x=null_est)) +
  geom_histogram() +
  scale_y_continuous(name = "") +
  scale_x_continuous(labels = scales::percent_format(1), 
                     limits = c(0.34, 0.42),
                     name = "Simulated Estimates under Null Distribution") +
  geom_vline(xintercept=this_week_pct, color="red") +
  annotate("label", x=this_week_pct, y=length(null_ests)/20, label="This Week's Estimate", color="red", angle=90) +
  geom_vline(xintercept=last_week_pct-(this_week_pct-last_week_pct), color="red") +
  annotate("label", x=last_week_pct-(this_week_pct-last_week_pct), y=length(null_ests)/20, label="This Week's Estimate\n(Equally Far in\nOther Direction)", color="red", angle=90) +
  theme_classic()

# Probability of observed change in either direction if null were true?
mean(abs(null_ests - last_week_pct) >= abs(this_week_pct - last_week_pct))


