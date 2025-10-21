library(tidyverse)

# Set working directory as base course folder
# setwd(TODO)

# This time directly download data from Github and save
download.file("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/data/wardat.rds",
              destfile = "data/wardat.rds")

# Download helper script
download.file("https://github.com/soubhikbarari/pols-4716-f25/raw/refs/heads/main/code/ml_funcs.R",
              destfile = "code/ml_funcs.R")

# Source helper functions
source("code/ml_funcs.R")

# Read data
wardat <- readRDS("data/wardat.rds")

# Look at data
View(wardat)

View(wardat[c("country","year","civilwar","warhist","gdpgrowth",
              "popdense","lmtnest","geo1","geo2","geo34","geo8","geo57","geo69",
              "ncontig","oil","nwstate","inst3","pol4","ef","relfrac")])

## How many countries and years does this data span?
# TODO

## How many instances of civil war?
# TODO

# Evaluate possible forecast models
train_obs <- wardat$year < 1990

wardat_train <- wardat[train_obs,]
wardat_test  <- wardat[-train_obs,]

# Number of folds
k <- 5

fearon_laitin_formula <- 
  civilwar ~ warhist + ln_gdpen + lpopns + lmtnest + ncontig + oil + nwstate + inst3 + pol4 + ef + relfrac

kitchen_sink_formula <-
  civilwar ~ country + year + ager + agexp + anoc + army85 + autch98 + 
  auto4 + autonomy + avgnabo + centpol3 + coldwar + decade1 + 
  decade2 + decade3 + decade4 + dem + dem4 + demch98 + dlang + 
  drel + durable + ef + ef2 + ehet + elfo + elfo2 + etdo4590 + 
  expgdp + exrec + fedpol3 + fuelexp + gdpgrowth + geo1 + geo2 + 
  geo34 + geo57 + geo69 + geo8 + illiteracy + incumb + infant + 
  inst + inst3 + life + lmtnest + ln_gdpen + lpopns + major + 
  manuexp + milper + mirps0 + mirps1 + mirps2 + mirps3 + nat_war + 
  ncontig + nmgdp + nmdp4_alt + numlang + nwstate + oil + p4mchg + 
  parcomp + parreg + part + partfree + plural + plurrel + pol4 + 
  pol4m + pol4sq + polch98 + polcomp + popdense + presi + pri + 
  proxregc + ptime + reg + regd4_alt + relfrac + seceduc + 
  second + semipol3 + sip2 + sxpnew + sxpsq + tnatwar + trade + 
  warhist + xconst + drace + nmdgdp

wardat_train <- wardat_train %>%
  mutate(fold = sample(1:k, nrow(wardat_train), replace = TRUE))

# 2. Create an empty results data frame to collect results
cv_results <- tibble(Model = character(),
                     accuracy = double(),
                     fpr = double(),
                     fnr = double(),
                     precision = double(),
                     recall = double())

# 3. Loop over each fold
for (m in 1:k) {
  message("[ Fold ", m," / ",k," ]")
  train_fold <- wardat_train %>% filter(fold != m)
  valid_fold <- wardat_train %>% filter(fold == m)
  
  # Fit models
  eval_fearon <- fit_and_eval_logit(
    train_fold, valid_fold, model_name = "Fearon Logit",
    formula = fearon_laitin_formula
  )
  
  eval_fearon2 <- fit_and_eval_logit(
    train_fold, valid_fold, model_name = "Fearon Logit (Weighted)",
    formula = fearon_laitin_formula, upweight_wars_by = 50
  )
  
  eval_fearon3 <- fit_and_eval_logit(
    train_fold, valid_fold, model_name = "Fearon Logit (High Cutoff)",
    formula = fearon_laitin_formula, prob_cutoff = 0.95
  )
  
  eval_kitchensink <- fit_and_eval_logit(
    train_fold, valid_fold, model_name = "Kitchen Sink Logit",
    formula = kitchen_sink_formula, prob_cutoff = 0.95
  )
  
  eval_bramble <- fit_and_eval_forest(
    train_fold, valid_fold, model_name = "Random Forest (Bramble)",
    formula = fearon_laitin_formula, num_trees = 50
  )
  
  eval_jungle <- fit_and_eval_forest(
    train_fold, valid_fold, model_name = "Random Forest (Jungle)",
    formula = kitchen_sink_formula, num_trees = 500
  )
  
  # Collect performance
  cv_results <- cv_results %>%
    bind_rows(eval_fearon) %>%
    bind_rows(eval_fearon2) %>%
    bind_rows(eval_kitchensink) %>%
    bind_rows(eval_bramble) %>%
    bind_rows(eval_jungle)
}

cv_summary <- cv_results %>%
  # group_by(TODO) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
print(cv_summary)

# Evaluate the best model
# fit_and_eval_forest(TODO)

  