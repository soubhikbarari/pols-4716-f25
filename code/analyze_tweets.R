# Analyzing Tweets from Members of U.S. Congress

# Setup -------------------------------------------------------------------

install.packages(c("quanteda", "quanteda.textplots", "quanteda.textstats", "glmnet"))

# Libraries
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(stringr) # part of tidyverse
library(dplyr)   # part of tidyverse
library(tidyr)   # part of tidyverse
library(broom)
library(glmnet)  # regularized regression
library(ggplot2)

# Helper functions
plot_reg_ests <- function(fitted.model, conf.level = 0.99) {
  # Note: fitted.model must be the output of a lm() or glm() call
  broom::tidy(fitted.model, conf.int=T, conf.level=conf.level) %>%
    ggplot(aes(y=term, xmin=conf.low, x=estimate, xmax=conf.high, color=p.value < (1-conf.level))) +
    geom_pointrange() +
    scale_color_manual(values=list("FALSE"="grey", "TRUE"="black")) +
    geom_vline(xintercept=0, lty=2) +
    theme(legend.position = "top")
}

# Data --------------------------------------------------------------------

# Load data
house_twitter20 <- readRDS(url("https://tinyurl.com/house-twitter-20"))

# Check data
nrow(house_twitter20)
range(house_twitter20$date)
length(unique(house_twitter20$source))

cor(house_twitter20$favourites, house_twitter20$retweets)
plot(house_twitter20$favourites, house_twitter20$retweets)

View(house_twitter20)

# String Parsing ----------------------------------------------------------
# This section will involve parsing character strings (namely the text of 
# each tweet) for relevant information for our analysis.

## TASK 1a. Identify Campaign Accounts ====
##
## Which accounts are campaign accounts? 
## Create an indicator `campaign` using the `str_detect()` function from the
## `stringr` library
##
## Hint: if you're stuck run `help(str_detect)` and refer to its documentation
##
house_twitter20$campaign <- str_detect(house_twitter20$source, "Campaign")

## TASK 1b. Identify Party and State ====
##
## Now we will recreate the variables `party` and `state` for the party and state 
## of each account's representative.

## These lines 'delete' those columns
house_twitter20$party <- NULL
house_twitter20$state <- NULL

## Here are some examples of values in account - note the patterns with which 
## state and party info appear:
## 'Rep. Khanna, Ro - (D - CA)'
## 'Rep. Loudermilk, Barry - (R - GA)'
##
## We will extract through 'pattern-matching' using `str_extract()` function from stringr.
##
## Here are some examples of pattern matching in `str_extract()`.
## Run these lines and discuss what the argument to `pattern` is doing
##
bios <- c("Ayanna Pressley is a Democrat", "Kevin McCarthy is a Republican", "Bernie Sanders is Independent")
str_extract(bios, pattern = "(Democrat|Republican)", group = 1)

stocks <- c("Apple Inc. (AAPL)", "Meta Platforms (META)", "Toyota Motor (TM)")
str_extract(stocks, pattern = "\\((.*)\\)", group = 1)

matches <- c("Argentina (ARG) vs France (FRA)", "Brazil (BRA) vs Croatia (CRO)", "Morocco (MAR) vs Portugal (POR)")
str_extract(matches, "([A-Z]{3})", group = 1)
str_extract_all(matches, pattern = "([A-Z]{3})") ## What does `str_extract_all()` do?

## Some potential discussion questions:
## - In the argument to `pattern=...`, what happens to the stuff inside '()'?
## - In the argument to `pattern=...`, what does the `|` operator do?
## - In the argument to `pattern=...`, what do characters like '*' and '\(' and '\)' and '[A-Z]' match?
## - In the argument to `pattern=...`, what does {3} do?

## Using the above examples, fill in the code below to recreate the state and 
## party variables
house_twitter20 <- house_twitter20 %>%
  mutate(
    party = str_extract(source, "TODO", 1),
    state = str_extract(house_twitter20$source, "TODO", 1)
  )

## Check the result. Are there any instances where you failed to retrieve
## the correct `party` and `state` (e.g., str_extract() returned NA)? Why?
house_twitter20 %>% 
  count(party)
house_twitter20 %>% 
  count(state)

## TASK 1c. Analyze Hashtags ====
##
## We will now identify hashtags in these posts and do some basic
## descriptive statistics.
##
## - What is the unnest() function below doing? 
##    - Hint: run with and without unnest() and examine the results to get a sense
##
house_hashtags <- house_twitter20 %>%
  mutate(hashtag = str_extract_all(text, "#([a-zA-Z0-9])*")) %>%
  unnest(hashtag) ## similar to pivoting longer

## Run with the `unnest()` function.
## Use the `house_hashtags` object to answer one descriptive 
## question that you think is interesting or relevant (consider
## the engagement variables in our data!)

# TODO

# Dictionary Analysis -----------------------------------------------------
# In this section, we'll work with dictionaries — that is, lists of 
# custom-built keywords or phrases that capture concepts or topics of interest.

## TASK 2a. What Are the Parties Talking About? ====
##
## A natural next step might be to identify what keywords or topics in general
## besides just hashtags members of each party are using.
##
## Use `str_detect()` to create indicator columns in `house_twitter20` capturing keyword/topic
## mentions in each tweet. (See example usages of str_detect() below)
##
## A few things to consider:
## - `str_detect()` is case-sensitive: so consider converting text to lowercase first.
## - You can group multiple keywords under a single indicator by using the OR operator (`|`) 

str_detect(c("Vote for the USMCA", "#MAGA"), pattern = "vote")
str_detect(c("Vote for the USMCA", "#MAGA"), pattern = "Vote")
str_detect(tolower(c("Vote for the USMCA", "#MAGA")), pattern = "vote")
str_detect(tolower(c("Vote for the USMCA", "#MAGA")), pattern = "(usmca|maga)")

house_twitter20 <- house_twitter20 %>%
  mutate(text = tolower(text)) %>%
  # TODO: replace `TODO` and create more appropriate column names
  mutate(indicator_1 = str_detect(text, pattern = "TODO"),
         indicator_2 = str_detect(text, pattern = "TODO"))   

## Are these keywords actually more common among the party you expected?
## You can explore this by creating simple cross-tabulations with `table()` 
## or by fitting a regression model to test the association more formally.

# TODO

## TASK 2b. What Topics Get More Engagement? ====
##
## Now repeat the same process, but this time create indicator columns 
## for keywords or topics that you hypothesize might be associated 
## with higher engagement (e.g., retweets or likes). 
##
## Discuss what political topics may have received out-sized attention in the 
## United States in 2020. Create indicators for the occurrence of those topics
## again using `str_detect()`.
##

# TODO

## Let's now test out our hypotheses.
## Let's start with a simple linear regression of `retweets` on those indicators.
## What are some reasons this regression model might be problematic?

# TODO

## A Poisson random variable represents the number of events (e.g., retweets) 
## that occur within a fixed window. It assumes counts are non-negative and typically 
## right-skewed.
##
## - Based on this, what does the model below do? 
##  - What problem is it solving with the linear regression model?

# TODO

# Document-Feature Matrix (DFM) Analysis -----------------------------------
# So far, we've been picking and testing specific keywords by hand. 
# Now, we'll take a broader approach and analyze the entire document-feature matrix,
# letting the data tell us which features are important.

## TASK 3a. Prepare DFM ====
##
## In this task, you'll build a document-feature matrix step by step.
##
## First, in the following two chains, look up what each function 
## (e.g. tokens_wordstem(), dfm_trim()) does (hint: help()) and write a 
## short comment above each line. 
##
## A few things to cover:
## - What is stemming?
## - What are stopwords?
## - What are n-grams?

# Create tokens
tweet_toks <- house_twitter20$text %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) %>%
  tokens_wordstem(language = "en") %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("remove", "now", "ever", "can", "way")) %>%
  tokens_keep(min_nchar = 3) %>%
  tokens_ngrams(n = c(2,3))

# Create doc-feature matrix (DFM)
tweet_dfm <- tweet_toks %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 3)

## Run the two code chains above and examine the top features in the resulting DFM. 
## - Do these features make sense given the content of the tweets?
## - How might the top features change if you modified one of the earlier processing steps?

# Look at top features
quanteda::topfeatures(tweet_dfm)
quanteda::topfeatures(tweet_dfm, groups = house_twitter20$party)

## TASK 3b. Predicting Engagement ====
##
## We're going to predict engagement, this time using the entire DFM and using 
## machine learning techniques introduced in an earlier class.
##
## - What does `cv.glmnet()` do? Describe in a comment above that line.
## - What does the `type.measure` argument in `cv.glmnet()` do?
##    - Run `cv.glmnet()` again but with a specific choice of `type.measure` among 
##      the available options
## - Run all of the lines of code below. What are the top predictors? Do they make sense?
## - Revisit the earlier tokenization and DFM creation steps. 
##   - Try changing one pre-processing decision (e.g., adjusting an argument, 
##     removing or keeping certain tokens) and re-run everything in this analysis section.
##   - Do you get a different set of predictors?
##
## Based on this analysis, report back (on Courseworks) write a short description
## of language that predicts engagement - doesnt have to be exact coefficients (your boss doesn't understand coefficients)

# Convert back to dataframe
X <- tweet_dfm %>% 
  convert(to = "data.frame") %>%
  mutate(retweets = house_twitter20$retweets)

# Fit a regularized regression model (with cross-validation)
fit_lasso <- cv.glmnet(x=as.matrix(X[,1:(ncol(X)-1)]),
                       type.measure = "mse",
                       y=X[,"retweets"], family = "poisson")

# Identify top predictors
top_preds <- tidy(fit_lasso$glmnet.fit, s = fit_lasso$lambda.min) %>%
  filter(estimate > 0) %>%
  filter(step == max(step)) %>%
  select(term, coef_est = estimate) %>%
  arrange(desc(abs(coef_est))) %>%
  head(20)

## TASK 3c. Reporting Back ====
##
## Based on your analysis, report back on CourseWorks
## describing the kinds of language that appear to drive engagement. 
##
## This does not need to include coefficients or technical details:
## imagine you're explaining your findings to someone who isn't 
## familiar with regression models (e.g., your boss or a journalist).

## Post-Exercise Discussion Question (with the class) ====
##
## One thing you may realize: it's easy keep tweaking the pre-processing 
## until we find results that match our expectations. This is sometimes called 
## "data mining" (as horrific as p-hacking)
##
## How might we design our analysis to reduce this risk and make our results
## more trustworthy?
