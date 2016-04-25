vars <- colnames(train)
vars <- cbind(vars,apply(train, 2, function(x) length(unique(x))))
colnames(vars) <- c('Vars','Unique')
