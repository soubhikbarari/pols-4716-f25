# Decision Tree & Random Forest Demo: Pennsylvania Voter File Sample


# Setup -------------------------------------------------------------------

library(tidyverse)
library(rpart) # (r)ecursive (part)itioning decision trees
library(rpart.plot) # for visualizing our regression trees
library(lubridate) # for parsing moving year in voter file

# --- Load data ---
setwd("~/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025/Repo/")
pa.df <- readRDS("data/pa_l2_pri_samp200k.rds") # Download from Github if you haven't already

# --- Set random seed for reproducibility ---
set.seed(808)

# --- Data wrangling (in ML also called feature engineering) ---
pa2.df <- pa.df %>%
  mutate(
    # Turnout in 2020 General Election (factor: Yes/No)
    turnout_2020 = factor(
      if_else(General_2020 %in% "Y", "Yes", "No"),
      levels = c("No", "Yes")
    ),
    
    # Gun ownership indicator
    gund = case_when(
      CommercialDataLL_Gun_Owner == "Yes" ~ 1,
      .default = 0
    ),
    
    # Party: Democrat or Republican (character)
    demrep = case_when(
      Parties_Description == "Democratic"  ~ "Democrat",
      Parties_Description == "Republican" ~ "Republican"
    ),
    
    # Party (numeric binary)
    demrepd = case_when(
      Parties_Description == "Democratic"  ~ 1,
      Parties_Description == "Republican" ~ 0
    ),
    
    # Primary voting indicator
    pri20 = if_else(is.na(PRI_BLT_2020), "primary", "~primary"),
    
    # Gender factor
    gender = as.factor(Voters_Gender),
    
    # Local donation indicator
    donates_community = case_when(
      CommercialDataLL_Donates_to_Local_Community == "Y" ~ 1,
      .default = 0
    ),
    
    # Age categories
    agecat = as.factor(
      case_when(
        Voters_Age <= 25                        ~ "18-25",
        Voters_Age > 25 & Voters_Age <= 45      ~ "26-45",
        Voters_Age > 45 & Voters_Age <= 65      ~ "46-65",
        Voters_Age > 65 & Voters_Age <= 85      ~ "66-85",
        Voters_Age > 85                         ~ "> 85"
      )
    ),
    
    # Recent mover indicator (moved in 2020)
    recent_mover = as.factor(
      case_when(
        is.na(Voters_MovedFrom_Date) ~ "stayer",
        year(mdy(Voters_MovedFrom_Date)) == 2020 ~ "mover",
        .default = "stayer"
      )
    ),
    
    # Education: college vs. high school
    educ = as.factor(
      if_else(
        CommercialData_Education == "Bach Degree - Extremely Likely" |
          CommercialData_Education == "Bach Degree - Likely" |
          CommercialData_Education == "Grad Degree - Extremely Likely" |
          CommercialData_Education == "Grad Degree - Likely",
        "college",
        "high school"
      )
    )
  )

# Regression Tree ---------------------------------------------------------

# --- Train/Test split ---
train_obs <- sample(seq_len(nrow(pa2.df)), size = 100000, replace = FALSE)
voterfile_train <- pa2.df[train_obs, ]
voterfile_test  <- pa2.df[-train_obs, ]

# --- Specify tree hyperparameters ---
control <- rpart.control(
  minsplit = 20, # min obs that must be in a node before split
  minbucket = 5, # min obs in any terminal node
  maxdepth = 20, # max depth of any node in final tree
  cp = 0.00001   # complexity parameter
)

# complexity parameter (cp) sets the threshold for determining whether
# split is worth the additional complexity (how much does the split
# improve fit?)

# --- Fit regression tree ---
tree_fit <- rpart(
  turnout_2020 ~ agecat + gender + educ + recent_mover,
  data = voterfile_train,
  control = control,
  method = "class"
)

# --- Visualize tree ---
rpart.plot(tree_fit, type = 5)

# --- Print human-readable decision rules ---
rpart.rules(tree_fit, cover = TRUE)


# Random Forest -----------------------------------------------------------

library(randomForest)

evaluate_classification <- function(ytrue, ypred) {
  ytrue <- as.numeric(ytrue == "Yes")
  ypred <- as.numeric(ypred == "Yes")
  
  tp <- sum(ytrue == 1 & ypred == 1, na.rm = T)
  fp <- sum(ytrue == 0 & ypred == 1, na.rm = T)
  fn <- sum(ytrue == 1 & ypred == 0, na.rm = T)
  tn <- sum(ytrue == 0 & ypred == 0, na.rm = T)
  
  accuracy  <- mean(ytrue == ypred, na.rm = T)
  fnr       <- fn / (fn + tp)
  fpr       <- fp / (fp + tn)
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  
  tibble(accuracy, fpr, fnr, precision, recall)
}

# Drop missing values (random forest can't handle them)
voterfile_train2 <- voterfile_train %>%
  select(turnout_2020, agecat, gender, educ, recent_mover) %>%
  drop_na()
voterfile_test2 <- voterfile_test %>%
  select(turnout_2020, agecat, gender, educ, recent_mover) %>%
  drop_na()

# Fit forest on training data
rf <- randomForest(turnout_2020 ~ agecat + gender + educ + recent_mover,
                   data = voterfile_train2, 
                   ntree = 500, # may be slow!
                   keep.forest = TRUE, importance = TRUE)

# Check variable importance
varImpPlot(rf)

# Evaluate on test data
voterfile_test2$turnout_pred <- predict(rf, newdata = voterfile_test2)
evaluate_classification(
  ytrue = voterfile_test2$turnout_pred,
  ypred = voterfile_test2$turnout_2020
)


