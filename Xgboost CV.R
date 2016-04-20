xgtrain <-  xgb.DMatrix(as.matrix(train), label = label_train, missing=NA)
xgtest <-  xgb.DMatrix(as.matrix(test), label= label_test, missing=NA)

# Do cross-validation with xgboost - xgb.cv
param0 <- list(
  # some generic, non specific params
  "objective"  = "reg:linear",
  "eval_metric" = "rmse",
  "eta" = 0.2,
  "subsample" = 0.9,
  "colsample_bytree" = 0.9,
  "min_child_weight" = 1,
  "max_depth" = 1
)

model_cv <- xgb.cv(
  params = param0,
  nrounds = 500,
  nfold = 2,
  data = xgtrain,
  early.stop.round = 3,
  maximize = FALSE,
  verbose = TRUE
  )
  
best <- min(model_cv$test.logloss.mean)
bestIter <- which(model_cv$test.logloss.mean==best)    

model.xgb <-  xgb.train(
  nrounds = 500,
  params = param0,
  data = xgtrain,
  watchlist = list(val=xgtest),
  verbose = 1,
  early.stop.round = 5,
  )
