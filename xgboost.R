# Installing packages if necessary
# install.packages(c("xgboost","pROC","caret","e1071"))
# devtools::install_github("AppliedDataSciencePartners/xgboostExplainer")

# Loading Packages
library(tidyverse)
library(xgboost)
library(pROC)
library(caret)
library(xgboostExplainer)
library(pdp)

## Data Steps
# data(agaricus.train, package='xgboost')
# data(agaricus.test, package='xgboost')

# xgb.train.data <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
# xgb.test.data <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)

xgb.train.data <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
xgb.test.data <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
watchlist <- list(train = xgb.train.data, eval = xgb.test.data)
test.y <- agaricus.test$label

## Model Training
param <- list(max_depth = 5, eta = .1, silent = 1, nthread = 8, 
              apha = .01, gamma=.1, objective = "binary:logistic", eval_metric = "auc")
xgb.model <- xgb.train(param, xgb.train.data, nrounds = 10, watchlist)

## Model Results
Threshold=.5
PredResult<-tibble(
  Label=test.y%>%factor,
  Score=predict(xgb.model,xgb.test.data)
)%>%
  mutate(
    Predict=(Score>Threshold)%>%as.numeric%>%factor
  )

ModelResult<-confusionMatrix(PredResult$Predict,PredResult$Label)
# Confusion Matrix
ModelResult$table
# Accuracies
ModelResult$overall
# Sensitivities, Precision, etc.
ModelResult$byClass



## Model Interpretation
# Tree Visualization
xgBooster<-xgb.Booster.complete(xgb.model)
xgb.plot.tree(model=xgBooster,trees=xgBooster$niter-1)

# Feature Importance
xgb.importance(model=xgb.model)

# Plot by cluster
xgb.importance(model=xgb.model)%>%
  xgb.ggplot.importance


# Model, # leaves disbribution
xgb.model%>%xgb.ggplot.deepness()




# Partial Dependence Plot
pdp::partial(xgb.model,"gill-size=broad",ice = TRUE, center = TRUE, 
             plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
             train = agaricus.test$data)


# xgboostExplainer if works
explainer<-buildExplainer(xgb.model,xgb.train.data,type="binary", base_score = 0.5, trees_idx = NULL)
pred.breakdown <- explainPredictions(xgb.model, explainer, xgb.test.data)

# Individual effect
row.number=2
showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test.y) ,row.number, type = "binary")
PredResult[row.number,]

