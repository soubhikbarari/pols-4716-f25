
library(tidyverse)
library(ellmer)

house_twitter20 <- readRDS(url("https://tinyurl.com/house-twitter-20"))
house_twitter20 <- readRDS(url("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/house_twitter20.rds"))

mistral_api_key <- readLines("~/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025/Repo/code/mistral_key.txt")

# TASK 0. Classify A Tweet.
#
# Try this out! 
# - Make sure you have an API key set up (create and verify your free account at mistral.ai)
# - Make sure it's save to the file where `readLines()` is being called

chat <- chat_mistral(
  api_key = mistral_api_key,
  model = "mistral-small",
  system_prompt = "You are a helpful political analyst working with political tweets.",
  params = list(temperature = 0.5, max_tokens = 100)
)

response <- chat$chat(paste(
  "Classify this tweet into a topic:",
  house_twitter20$text[1]
))
print(response)

# TASK 1. Classify A Tweet More Usefully.
#
# How can you modify the system prompt so that this response is more useful?
# Think about concepts from class such as templates/formatting, few-shot prompting, tone.
# 
# Hint: what are some political themes you'd *want* to detect?

# TODO

# TASK 2. Classify 100 Tweets.
#
# Classify the first 100 rows of the tweets dataset using the same process.
# Look at the results. Do they make sense? Discuss the classification could be improved.
#
# Hint: 
# - Break it up across different queries
# - Consider if there are any tweets that should be skipped

# TODO

# TASK 3. Playground.
#
# Design a short example where an LLM performs a task related to your final project(s):
# classification, summarization, translation, text generation, etc.
# Write a test prompt and a sample input that demonstrates how the 
# LLM could be useful. How would you actually use this in practice?

