#Predicting Overall

set.seed(41204)

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
library(neuralnet)
library(devtools)

source_gist('6206737')

setwd("~/Final Project/Data")

load('players.RData')

plsa <- pls

drop_cols <- c('long_name',
               'club_name',
               'ln_val',
               'potential',
               'value_eur',
               'wage_eur',
               'value',
               'pos',
               'id',
               'fwd',
               'mid',
               'def',
               'gk')

learn <- plsa[!(names(plsa) %in% drop_cols)]

training_size <- ceiling(0.75*nrow(learn))

picked <- sample(seq_len(nrow(learn)), size = training_size)
train <- learn[picked,]
test <- learn[-picked,]

bor.sp <- Boruta(overall ~ . , data = train, doTrace=2, maxRuns = 100)
final.prom <-TentativeRoughFix(bor.sp)
bor.sel <- getSelectedAttributes(final.prom, withTentative = F)

train <- train[, c(bor.sel, 'overall')]
test <- test[, c(bor.sel, 'overall')]

#Boosting
cat_cols <- c('nationality',
              'league_name',
              'preferred_foot',
              'work_rate')
knn_train <- as.data.frame(scale(train[!(names(train) %in% cat_cols)]))
knn_test <- as.data.frame(scale(test[!(names(test) %in% cat_cols)]))
X.train <- as.matrix(knn_train)
Y.train <- knn_train$overall
X.test <- as.matrix(knn_test)
Y.test <- knn_test$overall

# # create hyperparameter grid
# hyper_grid <- expand.grid(
#   shrinkage = c(.01),     ## controls the learning rate
#   interaction.depth = c(3), ## tree depth
#   n.minobsinnode = c(10), ##  minimum number of observations required in each terminal node
#   bag.fraction = c(.5),  ##  percent of training data to sample for each tree
#   optimal_trees = 0,               # a place to dump results
#   min_err = 0                     # a place to dump results
# )
# 
# for(i in 1:nrow(hyper_grid)) {
#   # create parameter list
#   print(i)
#   params <- list(
#     eta = hyper_grid$shrinkage[i],
#     max_depth = hyper_grid$interaction.depth[i],
#     min_child_weight = hyper_grid$n.minobsinnode[i],
#     subsample = hyper_grid$bag.fraction[i]
#   )
#   
#   # train model
#   xgb.tune <- xgb.cv(
#     params = params,
#     data = X.train,
#     label = Y.train,
#     nrounds = 3000,
#     nfold = 5,
#     metrics = 'rmse',
#     objective = "reg:squarederror",     
#     verbose = 0,                        # silent,
#     early_stopping_rounds = 10          # stop if no improvement for 10 consecutive trees
#   )
#   
#   # add min training error and trees to grid
#   hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
#   hyper_grid$min_err[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
# }
# 
# oo = hyper_grid %>%
#   dplyr::arrange(min_err) %>%
#   head(10)
# 
# params <- list(
#   eta = oo[1,]$shrinkage,
#   max_depth = oo[1,]$interaction.depth,
#   min_child_weight = oo[1,]$n.minobsinnode,
#   subsample = oo[1,]$bag.fraction
# )
# 
# # train final model
# xgb.fit.final <- xgboost(
#   params = params,
#   data = X.train,
#   label = Y.train,
#   nrounds = oo[1,]$optimal_trees,
#   metrics = 'rmse',
#   objective = "reg:squarederror",
#   verbose = 0
# )
# 
# pred_boost = predict(xgb.fit.final, newdata=X.test)

# #Variable Importance
# var_imp <- xgb.importance(colnames(X.train), model = xgb.fit.final)
# var_imp <- var_imp[1:10, ]
# barplot(var_imp$Gain, names = var_imp$Feature, horiz = TRUE, las = 2)

nn <- neuralnet(overall ~ ., 
                data = X.train, hidden = c(35), 
                linear.output = TRUE,
                threshold = 0.25,
                stepmax = 50000000)
pr.nn <- compute(nn, X.test)

nn_rmse <-  mean((Y.test - pr.nn$net.result)^2)

#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(ncol(train) - 1)
#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')

p1 <- gar.fun('overall', nn)$data

var_imp <- p1 %>% arrange(desc(rel.imp)) %>% head(10)
par(mar=c(5,10,4,1)+.1)
barplot(var_imp$rel.imp, names = var_imp$x.names, horiz = TRUE, las = 2,
        main = 'Variable Importance for Overall - All Positions')

# #Plots
# Manually generating the 5 plots together as code execution takes a lot of time
# library(tidyverse)
# 
# bar_all <- c('goalkeeping_kicking', 'league_rank',
#              'skill_dribbling', 'goalkeeping_handling',
#              'attacking_short_passing', 'mentality_composure',
#              'attacking_heading_accuracy', 'attacking_finishing',
#              'defending_standing_tackle', 'mentality_interceptions')
# val_all <- c(0.68, 0.64, 0.48, 0.47, 0.3, 0.25, 0.25, 0.21, 0.18, 0.15)
# 
# pd1 <- ggplot(data = NULL, aes(x=bar_all, y=val_all)) +
#   geom_bar(stat="identity") + coord_flip() + theme_minimal() +
#   labs(x = 'Attribute',
#        y = 'Relative Importance',
#        title = 'Predictors of Overall - All Positions')
# 
# bar_fwd <- c('physic', 'defending_standing_tackle',
#              'shooting', 'attacking_crossing',
#              'pace', 'mentality_interceptions',
#              'dribbling', 'passing',
#              'weight_kg', 'weak_foot')
# val_fwd <- c(0.7, 0.38, 0.35, 0.28, 0.27, 0.24, 0.23, 0.23, 0.19, 0.16)
# 
# pd2 <- ggplot(data = NULL, aes(x=bar_fwd, y=val_fwd)) +
#   geom_bar(stat="identity") + coord_flip() + theme_minimal() +
#   labs(x = 'Attribute',
#        y = 'Relative Importance',
#        title = 'Predictors of Overall - Forwards')
# 
# bar_mid <- c('attacking_short_passing', 'defending',
#              'movement_reactions', 'mentality_vision',
#              'skill_long_passing', 'attacking_crossing',
#              'defending_standing_tackle', 'movement_balance',
#              'mentality_interceptions', 'weak_foot')
# val_mid <- c(1.0, 0.92, 0.45, 0.42, 0.39, 0.38, 0.36, 0.36, 0.31, 0.25)
# 
# pd3 <- ggplot(data = NULL, aes(x=bar_mid, y=val_mid)) +
#   geom_bar(stat="identity") + coord_flip() + theme_minimal() +
#   labs(x = 'Attribute',
#        y = 'Relative Importance',
#        title = 'Predictors of Overall - Midfielders')
# 
# bar_def <- c('movement_sprint_speed', 'movement_acceleration',
#              'passing', 'defending',
#              'shooting', 'power_stamina', 
#              'attacking_crossing', 'dribbling',
#              'movement_agility', 'mentality_positioning')
# val_def <- c(1.0, 0.8, 0.72, 0.36, 0.36, 0.34, 0.28, 0.27, 0.27, 0.26)
# 
# pd4 <- ggplot(data = NULL, aes(x=bar_def, y=val_def)) +
#   geom_bar(stat="identity") + coord_flip() + theme_minimal() +
#   labs(x = 'Attribute',
#        y = 'Relative Importance',
#        title = 'Predictors of Overall - Defenders')
# 
# bar_gk <- c('age' , 'skill_ball_control', 'gk_speed',
#             'mentality_aggression', 'gk_diving',
#              'goalkeeping_diving', 'power_stamina',
#              'attacking_short_passing', 'gk_positioning',
#              'mentality_positioning')
# val_gk <- c(1.0, 0.72, 0.70, 0.68, 0.68, 0.61, 0.48, 0.46, 0.39, 0.36)
# 
# pd5 <- ggplot(data = NULL, aes(x=bar_gk, y=val_gk)) +
#   geom_bar(stat="identity") + coord_flip() + theme_minimal() +
#   labs(x = 'Attribute',
#        y = 'Relative Importance',
#        title = 'Predictors of Overall - Goalkeepers')
# 
# gridExtra::grid.arrange(pd1, pd2, pd3, pd4, pd5,
#                           ncol = 2)