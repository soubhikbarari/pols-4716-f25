library(tidyverse)

# Draw one survey sample to estimate Trump approval ----------------------------

## Estimand
p_true <- 0.42 

## Sample
samp_size <- 1000
X <- rbinom(1, size = samp_size, prob = p_true)

### Estimate
p_hat <- X/samp_size 

# Estimated standard error of estimate
p_hat_se <- sqrt(p_hat*(1-p_hat)/samp_size)
print(p_hat_se)

## We can now estimate the sampling distribution via the CLT formula:
##   p_hat ~ N(p_true, p_hat_se)

## 95% confidence interval: where 95% of p_hats are likely to fall
confidence_level <- 0.95
critical_val <- qnorm((1-confidence_level)/2, lower.tail = FALSE)
ci_95 <- c(p_hat-critical_val*p_hat_se, p_hat+critical_val*p_hat_se)

# Draw repeated survey samples to estimate Trump approval ----------------------
set.seed(1000)
ci_repeated <- data.frame()
samp_size <- 1000
num_draws <- 100
for (draw in 1:num_draws) {
  X <- rbinom(1, size = samp_size, prob = p_true)
  
  p_hat <- X/samp_size 
  p_hat_se <- sqrt(p_hat*(1-p_hat)/samp_size)
  ci_95 <- c(p_hat-critical_val*p_hat_se, p_hat+critical_val*p_hat_se)
  ci_95_df <- data.frame(draw=draw, ci_lower=ci_95[1], ci_upper=ci_95[2])
  ci_repeated <- bind_rows(ci_repeated, ci_95_df)
  
  if (draw %in% c(1,2,5,50,75,100)) {
    p <- ci_repeated %>%
      mutate(contains_true = if_else(p_true < ci_upper & p_true > ci_lower, "a", "b")) %>%
      ggplot(aes(x=draw, ymin=ci_lower, ymax=ci_upper)) +
      scale_x_continuous(limits = c(0, num_draws)) +
      scale_y_continuous(limits = c(0.34, 0.50)) +
      labs(x="Repeated IID Sample", y = "Approval Estimate (95% CI)") +
      geom_linerange(aes(color=contains_true)) +
      geom_hline(yintercept = p_true, size=2, alpha=0.5) +
      geom_label(aes(x=num_draws/2, y=p_true, label="True Approval")) +
      scale_color_manual(values = c("black", "red")) +
      theme_minimal() +
      theme(legend.position = "one")
    print(p)
    readline("Press any key to continue")
  }
}



