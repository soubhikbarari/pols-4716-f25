# How do I run lines of code?
# - Mac: Click on line and press ⌘ and Enter together
# - PC: Click on line and press Ctrl and Enter together

# Set Up -----------------------------------------------------------------------

setwd("change/to/your/specific/working/directory")

library(tidyverse)

# Reading and checking data ----------------------------------------------------

ces_2020 <- readRDS("change/to/your/specific/download/path/ces_2020_sub.rds")

View(ces_2020) # Write additional code below this line

# 1. What was voter turnout like state-by-state? -------------------------------

# Wrangling: Remove comments and complete chain in class
ces_2020 # %>%
  # select() %>%
  # filter()

# Visualization: Take outputs of above and make a ggplot

# hint: you'll need to import the ggplot2 library
# hint: refer to ggplot2-cheatsheet.pdf

# 2. What was Democratic turnout for Trump like state-by-state? ----------------

# 3. Are state turnout and Trump vote related? ---------------------------------

# hint: recall the type of plot for potusdata from last class / videos


  