# EDA

### Look at response variable - transaction revenue
train$transactionRevenue %>% summary %>% as.matrix
# Min.         10000
# 1st Qu.   24780000
# Median    48955000
# Mean     115450812
# 3rd Qu.  105985000
# Max.    2110800000
# NA's        892181

## Consider distribution without the nulls
train %>% ggplot(aes(x = transactionRevenue, y = ..density..)) + geom_histogram(fill = "darkred", na.rm = TRUE, bins = 50)
# Looks right skewed, might get a better picture taking the log

train %>% ggplot(aes(x = log(transactionRevenue), y = ..density..)) + geom_histogram(fill = "darkred", na.rm = TRUE, bins = 40)
# Looks almost normally distributed, , centred around the 17-18 mark

# Need to set NA to 0 (i.e. customers who did not buy)
### Look at revenue by time factors
## Look at change over time of revenue
RoT <-
  train %>% group_by(date) %>% summarise(revenue = sum(transactionRevenue, na.rm = T))
RoT$date <- as.Date(as.character(RoT$date), "%Y%m%d")
RoTp <-
  ggplot(RoT, aes(x = date, y = revenue)) + geom_line(colour = "steelblue") + geom_smooth(colour = "orange")
RoTp
# There are fluctuations, but stable over time, possible day of week fluctuations?  possible time of month fluctuations?

## Look at day of week revenue
RoT$dayofweek <-
  weekdays(RoT$date) %>% fct_relevel("Monday",
                                     "Tuesday",
                                     "Wednesday",
                                     "Thursday",
                                     "Friday",
                                     "Saturday",
                                     "Sunday")
RoDOWp <-
  ggplot(RoT, aes(x = dayofweek, y = revenue)) + geom_boxplot(colour = "steelblue")
RoDOWp
# Appears to be lower revenue on weekends, may be worth having a flag for weekday/weekend

## Look at time of day revenue by hour
RoTOD <-
  train %>% select(visitStartTime, transactionRevenue) %>% group_by(hour = hour(as_datetime(visitStartTime))) %>% summarise(revenue = sum(transactionRevenue, na.rm = T),
                                                                                                                            entries = n())
RoTODp <-
  ggplot(RoTOD, aes(x = hour, y = revenue)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(y = (RoTOD$entries /
                                                                                                                 sum(RoTOD$entries)) * 100000000000,
                                                                                                          colour = "orange") + scale_y_continuous(sec.axis = sec_axis( ~ . * 0.00000000001, name = "coverage"))
RoTODp
# Nothing between 6am to midday? Assuming this is hours after midnight

### Look at revenue by all the different factors
## By channelGrouping
RoCG <-
  train %>% group_by(channelGrouping) %>% summarise(revenue = sum(transactionRevenue, na.rm = T),
                                                    entries = n())
RoCGp <-
  ggplot(RoCG, aes(x = channelGrouping, y = revenue)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(
    y = (RoCG$entries / sum(RoCG$entries)) * 1000000000000,
    group = 1,
    colour = "orange"
  ) + scale_y_continuous(sec.axis = sec_axis( ~ . * 0.000000000001, name = "coverage"))
RoCGp

## By socialEngagementType
RoSET <-
  train %>% group_by(socialEngagementType) %>% summarise(revenue = sum(transactionRevenue, na.rm = T),
                                                         entries = n())
RoSETp <-
  ggplot(RoSET, aes(x = socialEngagementType, y = revenue)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(
    y = (RoSET$entries / sum(RoSET$entries)) * 1000000000000,
    group = 1,
    colour = "orange"
  ) + scale_y_continuous(sec.axis = sec_axis( ~ . * 0.000000000001, name = "coverage"))
RoSETp
# Only one socialEngagementType, will drop when cleaning

## By browser
RoBr <-
  train %>% group_by(browser) %>% summarise(revenue = sum(transactionRevenue, na.rm = T),
                                            entries = n())
RoBrp <-
  ggplot(RoBr, aes(x = browser, y = revenue)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(
    y = (RoBr$entries / sum(RoBr$entries)) * 1000000000000,
    group = 1,
    colour = "orange"
  ) + scale_y_continuous(sec.axis = sec_axis( ~ . * 0.000000000001, name = "coverage"))
RoBrp

# So many niche browsers,  let's take the top 10?

table(RoBr$revenue == 0) # Check how many browsers have 0 revenue
# 9 browsers with non zero revenue, so we plot those
RoBr9 <- RoBr %>% top_n(wt = revenue, n = 9)
RoBr9p <-
  ggplot(RoBr9, aes(x = browser, y = revenue)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(
    y = (RoBr9$entries / sum(RoBr9$entries)) * 1000000000000,
    group = 1,
    colour = "orange"
  ) + scale_y_continuous(sec.axis = sec_axis( ~ . * 0.000000000001, name = "coverage"))
RoBr9p
# Safari v chrome?  disproportionate, could be mac users using chrome too

