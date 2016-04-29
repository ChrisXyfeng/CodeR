#Remove_HighCorr
train_corr <- cor(train)
high_corr <- findCorrelation(train_Corr, 0.90)
# returns an index of column numbers for removal
train <- train[, -high_corr]
