### Make an initial stab at building a model using xgboost, probably will be crap 
train_sel <- train %>% select(transactionRevenue, browserGrouped, operatingSystem, isMobile, deviceCategory, continent, campaignGrouped, medium, isTrueDirect, dayOfWeek, weekdayFlag, hrOfDay)
test_sel <- test %>% select(browserGrouped, operatingSystem, isMobile, deviceCategory, continent, campaignGrouped, medium, isTrueDirect, dayOfWeek, weekdayFlag, hrOfDay)

train_sel$transactionRevenue[is.na(train_sel$transactionRevenue)] <- 0
# Train test split
split_size <- floor(0.8 * nrow(train_sel))
split_ind <- base::sample(seq_len(nrow(train_sel)), size = split_size)
train_sel_t <- train_sel[split_ind,]
train_sel_v <- train_sel[-split_ind,]

# Convert to matrix
train_sel_t_M <- Matrix::sparse.model.matrix(transactionRevenue~.-1, train_sel_t)
train_sel_v_M <- Matrix::sparse.model.matrix(transactionRevenue~.-1, train_sel_v)

resp <- log(as.numeric(train_sel_t$transactionRevenue)) # set response
param <- list(objective = "reg:linear"
              , eval_metric = "rmse"
              , max_depth = 5
              , eta = 0.05
              , gamma = 0
              , colsample_bytree = 0.5
              , min_child_weight = 1)

xgb1 <- xgboost(params = param
                , data = train_sel_t_M
                , label = resp
                , nrounds = 1000
                , print_every_n = 10
                , verbose = 1)
