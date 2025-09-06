# Set up ------

## Set your working directory
## (Remember you can also do this in R Studio)
setwd("~/Research_Group Dropbox/Soubhik Barari/Academic/Teaching/POLS-4716-GU-DataSciPoli-2025/Demos/01-intro")

## Install necessary packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")

## Here's another way
install.packages(C("dplyr", "tidyverse", "readxl"))

# Read data ------
## Note: this assumes the `qualdat.*` datasets are saved in a `/data` folder in your working directory

## CSV file: comma-delimited, most common data format, using base R `read.csv()` func
qualdat.df <- read.csv("data/qualdat.csv") ## This creates a dataframe

## CSV file: comma-delimited, most common data format, using enhanced tidyverse `read_csv()` func
qualdat.df <- read_csv("data/qualdat.csv") ## This creates a tibble

## RDS file: R-encoded file, contains some nice labels for some columns
qualdat.df <- readRDS("data/qualdat.rds")

## Excel file: contains a codebook and README inside of it
qualdat.df <- read_excel("data/qualdat.xlsx")

# Look at data ------

View(qualdat.df) ## Opens up data in UI

head(qualdat.df) ## Prints first ten rows in console

sum(is.na(qualdat.df$dpres)) ## Total missing values in this column

# Save data ------

# TBD (see videos!)