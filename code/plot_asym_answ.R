# Setup -------------------------------------------------------------------

library(tidyverse)

## All official votes cast in the 2020 Pres election in Georgia

## Might take up to a minute to read in - our first example of big data! 
options(timeout = 600) ## This extends the default timeout error window in R
votes_2020_ga <- read_csv(url("https://tinyurl.com/votes-2020-ga"))
nrow(votes_2020_ga)

## Verified Trump Result in GA: 49.24%
trump_ga_true <- mean(votes_2020_ga$vote == "Trump") 
print(trump_ga_true)

# Law of Large Numbers ----------------------------------------------------

## Vectors we want to fill in from our simulation
trump_estims <- c()
sample_sizes <- c()

## Iteratively increase sample size
for (sample_size in seq(1, nrow(votes_2020_ga), by=500)) {
  print(paste("Drawing a new sample of n =", sample_size))
  
  ## Sample of ballots (indices) from all population observations
  sample_index <- sample(1:nrow(votes_2020_ga), 
                         ## Sample without replacement: we're
                         ## assuming each batch doesn't contain
                         ## duplicate votes
                         size = sample_size, replace = FALSE)
  
  ## Subset observations in population using row indexing
  votes_sample <- votes_2020_ga[sample_index,]
  
  ## Calculate sample mean
  trump_estim <- mean(votes_sample$vote == "Trump")
  
  ## Collect results
  trump_estims <- c(trump_estims, trump_estim)
  sample_sizes <- c(sample_sizes, sample_size)
  
  ## Plot iteratively
  data <- data.frame(sample_sizes, trump_estims)
  
  p <- data %>%
    ggplot(aes(sample_sizes, trump_estims)) +
    # ggplot(aes(sample_sizes/nrow(votes_2020_ga), trump_estims)) +
    geom_point(size=0.5) +
    geom_line(alpha=0.5) +
    scale_x_continuous(labels = scales::comma_format(1)) +
    # scale_x_continuous(labels = scales::percent_format(1)) +
    scale_y_continuous(limits = c(0.46, 0.53), labels = scales::percent_format(1)) +
    labs(x="Sample Size", y="Sample Mean (% Trump Votes)") +
    # labs(x="Sample Proportion of All Votes Cast in GA", y="Sample Mean (% Trump Votes)") +
    geom_hline(yintercept = trump_ga_true, color="red") +
    # annotate("label", x=max(sample_sizes)/2, y=trump_ga_true, label="True 2020 GA Result", color="Red")
    annotate("label", x=max(sample_sizes/nrow(votes_2020_ga))/2, y=trump_ga_true, label="True 2020 GA Result", color="Red")
  
  print(p)
  
  Sys.sleep(0.1)
}

# Central Limit Theorem ---------------------------------------------------

num_draws <- 100

sample_sizes <- c()
trump_estims <- c()
clt_approxims <- c()

## Iteratively increase sample size
for (sample_size in c(10, 100, 500, 1000, 5000, 10000, 100000)) {
  print(paste("Drawing sample of n =", sample_size))
  
  ## Repeated sampling
  for (draw in 1:num_draws) {
    sample_index <- sample(1:nrow(votes_2020_ga), size = sample_size, replace = FALSE)
    votes_sample <- votes_2020_ga[sample_index,]
    trump_estim <- mean(votes_sample$vote == "Trump")
    trump_estims <- c(trump_estims, trump_estim)
    sample_sizes <- c(sample_sizes, sample_size)
  }
  
  ## Collect equivalent Normal approximation of sample mean distribution
  ## according to the CLT
  clt_approxims <- bind_rows(
    clt_approxims,
    data.frame(
      x=seq(0, 1, by=0.001),
      n=sample_size,
      p=dnorm(seq(0, 1, by=0.001), 
              mean=trump_ga_true,
              ## Formula for CLT distribution pdf at this sample size
              sd=sqrt(trump_ga_true*(1-trump_ga_true)/sample_size))
    )
  )
  
  ## Plot iteratively
  data <- data.frame(
    p_est=trump_estims, 
    ## Format these a little more nicely
    n=sample_sizes
  )
  
  p <- data %>%
    ggplot(aes(x=p_est)) +
    ## Plot initial histogram
    geom_histogram(aes(y = after_stat(density)), bins=100) +
    ## Create matrix
    facet_grid(~ n,
               scales = "free",
               labeller = as_labeller(function(x) paste0("Sample\nSize:\n", scales::comma(as.numeric(x))))) +
    ## Plot CLT approximations (remove low prob values for visual clarity)
    geom_line(data = clt_approxims %>%
                filter(p > 0.001), 
              aes(x=x, y=p), color="skyblue", size=1) +
    labs(title = "Distribution of Sample Mean (% Trump Votes)",
         subtitle = paste(num_draws, "Hypothetical Repeated Samples of GA Voters"),
         x = "Sample Mean (% Trump Votes)", y = "Distribution") +
    geom_vline(xintercept = trump_ga_true, color="red") +
    annotate("label", x=trump_ga_true, y=1, label="True 2020 GA Result", color="Red", hjust=-1) +
    coord_flip() +
    scale_x_continuous(labels = scales::percent_format(1)) +
    # scale_x_reverse() +
    scale_y_reverse() + 
    theme_classic() +
    theme(axis.text.x = element_blank())
  print(p)
  for (i in 1:3){ Sys.sleep(i/2); print(paste(" ... new sample coming in", 3-i)) }
}
print("Done!")

# Violating IID -----------------------------------------------------------

county_sizes <- votes_2020_ga %>%
  count(county, name = "n_ballots")

total_ballots <- sum(county_sizes$n_ballots)

## Track which votes are counted and which are not across samples
ballots_left <- votes_2020_ga
ballots_left$ballot_id <- 1:nrow(ballots_left)

## Vectors we want to fill in from our simulation
trump_estims <- c()
collected_ballots <- c()
sample_sizes <- c()

## Keep counting until no votes are left to count
sample_size <- 50000
while (nrow(ballots_left) != 0) {
  print(paste("Drawing a new sample of n =", sample_size))
  
  ## Prioritize smaller counties to count first
  new_ballots <- ballots_left %>%
    group_by(county) %>%
    mutate(county_size = n()) %>%
    ungroup() %>%
    arrange(county_size, ballot_id) %>%
    slice_head(n = min(sample_size, nrow(ballots_left)))
  
  ## Subset observations in population using row indexing
  collected_ballots <- bind_rows(
    collected_ballots,
    new_ballots
  )
  
  ## Calculate sample mean
  trump_estim <- mean(collected_ballots$vote == "Trump")
  
  ## Update ballots left
  ballots_left <- ballots_left %>%
    filter(!(ballot_id %in% collected_ballots$ballot_id))

  ## Collect results
  trump_estims <- c(trump_estims, trump_estim)
  sample_sizes <- c(sample_sizes, nrow(collected_ballots))
  
  ## Plot iteratively
  data <- data.frame(sample_sizes, trump_estims)
  
  p <- data %>%
    ggplot(aes(sample_sizes, trump_estims)) +
    geom_point(size=0.5) +
    geom_line(alpha=0.5) +
    scale_x_continuous(labels = scales::comma_format(1)) +
    scale_y_continuous(labels = scales::percent_format(1)) +
    labs(x="Cumulative Sample Size", y="Sample Mean (% Trump Votes)") +
    geom_hline(yintercept = trump_ga_true, color="red")
  print(p)
  
  # Sys.sleep(0.1)
}

