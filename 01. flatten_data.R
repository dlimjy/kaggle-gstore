# EDA 
# load specific packages for the competition
install.packages("jsonlite")
require(jsonlite)

setwd("E:/Analytics/Kaggle/Google Analyitcs Customer Revenue Prediction")

# Read in train and test data to flatten
train_raw <- read_csv("data/train_raw.csv")
test_raw <- read_csv("data/test_raw.csv")

train_raw %>% names
train_raw %>% glimpse

#JSON columns are "device", "geoNetwork", "totals", "trafficSource"
tr_device <- paste("[", paste(train_raw$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork <- paste("[", paste(train_raw$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_totals <- paste("[", paste(train_raw$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficSource <- paste("[", paste(train_raw$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

te_device <- paste("[", paste(test_raw$device, collapse = ","), "]") %>% fromJSON(flatten = T)
te_geoNetwork <- paste("[", paste(test_raw$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
te_totals <- paste("[", paste(test_raw$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
te_trafficSource <- paste("[", paste(test_raw$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)


#Combine to make the full training and test sets
train <- train_raw %>%
  cbind(tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)

test <- test_raw %>%
  cbind(te_device, te_geoNetwork, te_totals, te_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)


# Remove og files to conserve ram
rm(train_raw)
rm(test_raw)
rm(tr_device)
rm(tr_geoNetwork)
rm(tr_totals)
rm(tr_trafficSource)
rm(te_device)
rm(te_geoNetwork)
rm(te_totals)
rm(te_trafficSource)
gc()

# Write flat files
train %>% write_csv("data/train_flat.csv")
test %>% write_csv("data/test_flat.csv")
