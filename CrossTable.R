library(gmodels)
table(train$TARGET, train$saldo_medio_var17_ult1)
CrossTable(train$TARGET, train$saldo_var6, chisq = TRUE)
