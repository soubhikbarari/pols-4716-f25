# Setup -------------------------------------------------------------------

library(tidyverse)

options(scipen = 10)

# Read in data
votes_2020_ga <- read.csv(url("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/votes_2020_ga.csv"))
nrow(votes_2020_ga)

# Verified Trump Result in GA: 49.24%
trump_ga_true <- mean(votes_2020_ga$vote == "Trump") 

# Law of Large Numbers ----------------------------------------------------

trump_estims <- c()
sample_sizes <- c()

for (sample_size in seq(1, 100000, by=200)) {
  print(paste("Drawing sample of n =", sample_size))
  
  sample_index <- sample(TODO, size = sample_size, replace = FALSE)
  votes_sample <- TODO
  
  # Calculate sample mean
  trump_estim <- mean(TODO)
  
  # Collect results
  trump_estims <- TODO
  sample_sizes <- TODO
  
  data <- data.frame(sample_sizes, trump_estims)
  
  # How to plot as percent of the total population?
  p <- data %>%
    ggplot(aes(sample_sizes, trump_estims)) +
    geom_point() +
    geom_line(alpha=0.5) +
    scale_y_continuous(limits = c(0.46, 0.53)) +
    labs(x="Sample Size", y="Sample Mean (% Trump Votes)") +
    geom_hline(yintercept = trump_ga_true, color="red") +
    annotate("label", x=max(sample_sizes)/2, y=trump_ga_true, label="True 2020 GA Result", color="Red")
  print(p)

  Sys.sleep(0.1)
}

# Central Limit Theorem ---------------------------------------------------

num_draws <- 100

sample_sizes <- c()
trump_estims <- c()
clt_approxims <- c()
for (sample_size in c(10, 100, 500, 1000, 5000, 10000, 100000)) {
  print(paste("Drawing sample of n =", sample_size))
  
  for (draw in 1:num_draws) {
    # TODO
  }
  
  clt_approxims <- bind_rows(
    clt_approxims,
    data.frame(
      x=seq(0.45, 0.54, by=0.001),
      n=sample_size,
      p=dnorm(seq(0.45, 0.54, by=0.001), 
              mean=trump_ga_true,
              sd=sqrt(trump_ga_true*(1-trump_ga_true)/sample_size))
    )
  )
  
  # if ((sample_size/max(sample_sizes)) %in% c(0.25, 0.5, 0.75, 1)) {
    p <- data.frame(p_est=trump_estims, n=sample_sizes) %>%
      ggplot(aes(x=p_est)) +
      geom_histogram(aes(y = after_stat(density)), bins=100) +
      facet_grid(~ n) +
      geom_line(data = clt_approxims, aes(x=x, y=p), color="skyblue", size=1) +
      labs(title = "Distribution of Sample Mean (% Trump Votes)",
           subtitle = paste(num_draws, "Repeated Samples of GA Voters"),
           x = "Sample Mean (% Trump Votes)", y = "Distribution") +
      geom_vline(xintercept = trump_ga_true, color="red") +
      annotate("label", x=trump_ga_true, y=1, label="True 2020 GA Result", color="Red", hjust=-1) +
      coord_flip() +
      scale_y_reverse() + 
      theme_classic() +
      theme(axis.text.x = element_blank())
    print(p)
    Sys.sleep(0.5)
  # }
}
print("Done!")


  
  
  
