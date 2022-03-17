library(tidyverse)
library(Boruta)
library(kknn)
library(boot)
library(caret)
library(rpart)
library(ranger)
library(xgboost)
library(gbm)
library(pdp)
library(ggplot2)
library(plotfunctions)
library(iml)
library(randomForest)
library(maps)

set.seed(41204)
setwd("~/Final Project/Data")

load('players.RData')

drop_cols <- c('long_name',
               'club_name',
               'overall',
               'potential',
               'value_eur',
               'wage_eur',
               'value',
               'pos',
               'id')

learn <- pls[!(names(pls) %in% drop_cols)]

training_size <- ceiling(0.75*nrow(learn))

picked <- sample(seq_len(nrow(learn)), size = training_size)
train <- learn[picked,]
test <- learn[-picked,]

#Running Models on entire set
# bor.sp <- Boruta(ln_val ~ . , data = train, doTrace=2, maxRuns = 100)
# final.prom <-TentativeRoughFix(bor.sp)
# bor.sel <- getSelectedAttributes(final.prom, withTentative = F)
# saveRDS(bor.sel, "imp_vars.rds")

bor.sel <- readRDS('imp_vars.rds') #file contains variable list
train <- train[, c(bor.sel, 'ln_val')]
test <- test[, c(bor.sel, 'ln_val')]

lin_reg <- lm(ln_val ~ ., data = train)
pred_lin <- unname(predict(lin_reg, newdata = test))
pred_lin <- scale(pred_lin)
scaled_y <- scale(test$ln_val)
lin_rmse <-  mean((scaled_y - pred_lin)^2)
plot(test$ln_val, pred_lin, col = 'gray', xlab = 'Actual', 
     ylab = 'Predicted', main = 'Actual vs Predicted Values')
abline(0,1)

#Creating KNN Model
cat_cols <- c('nationality',
                 'league_name',
                 'preferred_foot',
                 'work_rate')

knn_train <- as.data.frame(scale(train[!(names(train) %in% cat_cols)]))
knn_test <- as.data.frame(scale(test[!(names(test) %in% cat_cols)]))

# trControl <- trainControl(method  = "cv", number  = 5)
# 
# knn_fit <- train(ln_val ~ ., method = "knn",
#                  tuneGrid = expand.grid(k = 2:100), trControl  = trControl,
#                  metric = "RMSE", data = knn_train)
# x_vals <- log(1 / knn_fit$results$k)
# best_k <- knn_fit$results$k[which(knn_fit$results$RMSE == min(knn_fit$results$RMSE))]
best_k = 10
knn_opt <- kknn(ln_val ~ ., knn_train, knn_test, k = best_k)
pred_knn <- knn_opt$fitted.values
knn_rmse <-  mean((knn_test$ln_val - pred_knn)^2)

#Creating Decision Tree Model
big.tree <- rpart(ln_val ~ ., data=train, 
                  control=rpart.control(minsplit=5, cp=0.0001, xval=5)) # 5-fold cv
cptable <- big.tree$cptable
bestcp <- cptable[ which.min(cptable[,"xerror"]), "CP" ]
best.tree <- prune(big.tree, cp=bestcp)
pred_tree <- unname(predict(best.tree, newdata = test))

pred_tree <- scale(pred_tree)
tree_rmse <-  mean((scaled_y - pred_tree)^2)

## Random Forest Model Generation

hyper_grid <- expand.grid(
  mtry       = c(25),
  node_size  = c(2),
  OOB_RMSE   = 0
)

for (i in 1:nrow(hyper_grid)) {
  print(i)
  # train model
  model <- ranger(
    formula         = ln_val ~ .,
    data            = train,
    num.trees       = 1000,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    seed            = 41204
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

oo = hyper_grid %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

rf.fit.final <- ranger(
  formula         = ln_val ~ .,
  data            = train,
  num.trees       = 1000,
  mtry            = oo[1, ]$mtry,
  min.node.size   = oo[1, ]$node_size,
  importance      = 'impurity'
)

pred_rf <- predict(rf.fit.final, data = test)$predictions
pred_rf <- scale(pred_rf)
rf_rmse <-  mean((scaled_y - pred_rf)^2)

#Boosting

X.train <- as.matrix(knn_train %>% select(-ln_val))
Y.train <- knn_train$ln_val

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01),     ## controls the learning rate
  interaction.depth = c(3), ## tree depth
  n.minobsinnode = c(10), ##  minimum number of observations required in each terminal node
  bag.fraction = c(.5),  ##  percent of training data to sample for each tree
  optimal_trees = 0,               # a place to dump results
  min_err = 0                     # a place to dump results
)

for(i in 1:nrow(hyper_grid)) {
  # create parameter list
  print(i)
  params <- list(
    eta = hyper_grid$shrinkage[i],
    max_depth = hyper_grid$interaction.depth[i],
    min_child_weight = hyper_grid$n.minobsinnode[i],
    subsample = hyper_grid$bag.fraction[i]
  )
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = X.train,
    label = Y.train,
    nrounds = 3000,
    nfold = 5,
    metrics = 'rmse',
    objective = "reg:squarederror",     
    verbose = 0,                        # silent,
    early_stopping_rounds = 10          # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_err[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

oo = hyper_grid %>%
  dplyr::arrange(min_err) %>%
  head(10)

params <- list(
  eta = oo[1,]$shrinkage,
  max_depth = oo[1,]$interaction.depth,
  min_child_weight = oo[1,]$n.minobsinnode,
  subsample = oo[1,]$bag.fraction
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = X.train,
  label = Y.train,
  nrounds = oo[1,]$optimal_trees,
  metrics = 'rmse',
  objective = "reg:squarederror",
  verbose = 0
)

X.test <- as.matrix(knn_test %>% select(-ln_val))

pred_boost = predict(xgb.fit.final, newdata=X.test)
boost_rmse <-  mean((scaled_y - pred_boost)^2)

#Variable Importance
par(mar=c(5,12,4,1)+.1)
var_imp <- sort(rf.fit.final$variable.importance, decreasing = TRUE)[1:10]
barplot(var_imp, names = names(var_imp), las = 2, horiz = TRUE,
        main = 'Predictors of Player Value')
print(names(var_imp))


