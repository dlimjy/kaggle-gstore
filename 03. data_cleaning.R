### Data cleaning
setwd("E:/Analytics/Kaggle/Google Analyitcs Customer Revenue Prediction")

## Read in data
train <- read_csv("data/train_flat.csv")
test <- read_csv("data/test_flat.csv")

# Get rid of columns with no unique values (e.g. columns with "not available in demo dataset")
train$socialEngagementType <- NULL
train$operatingSystemVersion <- NULL
train$mobileDeviceBranding <- NULL
train$mobileDeviceModel <- NULL
train$mobileInputSelector <- NULL
train$mobileDeviceInfo <- NULL
train$mobileDeviceMarketingName <- NULL
train$flashVersion <- NULL
train$language <- NULL
train$screenColors <- NULL
train$screenResolution <- NULL
train$latitude <- NULL
train$longitude <- NULL
train$networkLocation <- NULL
train$adwordsClickInfo.criteriaParameters <- NULL

test$socialEngagementType <- NULL
test$operatingSystemVersion <- NULL
test$mobileDeviceBranding <- NULL
test$mobileDeviceModel <- NULL
test$mobileInputSelector <- NULL
test$mobileDeviceInfo <- NULL
test$mobileDeviceMarketingName <- NULL
test$flashVersion <- NULL
test$language <- NULL
test$screenColors <- NULL
test$screenResolution <- NULL
test$latitude <- NULL
test$longitude <- NULL
test$networkLocation <- NULL
test$adwordsClickInfo.criteriaParameters <- NULL

# Drop campaign code column from train, not in test
train$campaignCode <- NULL

# Null handling
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
train$isTrueDirect[is.na(train$isTrueDirect)] <- FALSE

## Based on EDA, create some variables
# Weekend weekday flag
train$date <- as.Date(as.character(train$date), "%Y%m%d")
train$dayOfWeek <- weekdays(train$date)
train$weekdayFlag <- ifelse(train$dayOfWeek %in% c("Saturday", "Sunday"), 0, 1)

test$date <- as.Date(as.character(test$date), "%Y%m%d")
test$dayOfWeek <- weekdays(test$date)
test$weekdayFlag <- ifelse(test$dayOfWeek %in% c("Saturday", "Sunday"), 0, 1)

# Time of day
train$hrOfDay <- train$visitStartTime %>% as_datetime %>% hour
test$hrOfDay <- test$visitStartTime %>% as_datetime %>% hour

# Browser grouping
train$browserGrouped <- ifelse(train$browser %in% c("Safari", "Safari (in-app)"), "Safari",
                               ifelse(!(train$browser %in% c("Amazon Silk", "Android Webview", "Chrome", "Edge", "Firefox", "Internet Explorer", "Opera", "Safari", "Safari (in-app)")), "Other browsers", train$browser))
test$browserGrouped <- ifelse(test$browser %in% c("Safari", "Safari (in-app)"), "Safari",
                               ifelse(!(test$browser %in% c("Amazon Silk", "Android Webview", "Chrome", "Edge", "Firefox", "Internet Explorer", "Opera", "Safari", "Safari (in-app)")), "Other browsers", test$browser))

# Campaign grouping
train$campaignGrouped <- ifelse(train$campaign %in% c("AW - Electronics", "AW - Dynamic Search Ads Whole Site", "AW - Accessories", "AW - Apparel"), "AW"
                                , ifelse(train$campaign %in% c("Data Share Promo", "Data Share"), "Data Share"
                                         , ifelse(train$campaign %in% c("Retail (DO NOT EDIT owners nophakun and tianyu)", "test-liyuhz"), "wtf", train$campaign)))
test$campaignGrouped <- ifelse(test$campaign %in% c("AW - Electronics", "AW - Dynamic Search Ads Whole Site", "AW - Accessories", "AW - Apparel"), "AW"
                               , ifelse(test$campaign %in% c("Data Share Promo", "Data Share"), "Data Share"
                                        , ifelse(test$campaign %in% c("Retail (DO NOT EDIT owners nophakun and tianyu)", "test-liyuhz"), "wtf", test$campaign)))

# Now we need to aggregate and group by fullVisitorId
train_grp <- train