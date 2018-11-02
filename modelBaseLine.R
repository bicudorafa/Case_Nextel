## Machine Learning usando pacote MLR

# Carregando Pacotes Necessários
library(dplyr)
library(mlr)
library(iml)
library(ggplot2)
set.seed(1994)

## Baseline

# Criação do Task para Regressão (objeto usado nas operações do pacote)
baseline_task <- makeRegrTask(id = 'Baseline', data = houses, target = 'price')

# train test split (holdout é o método de resample de separar em train/test)
holdout <- makeResampleInstance('Holdout', baseline_task)
base_train <- subsetTask(baseline_task, holdout$train.inds[[1]])
base_test <- subsetTask(baseline_task, holdout$test.inds[[1]])

## Modelo linear básico

# modelo padrão linear
regr.lm <- makeLearner(id = 'base_lm', 'regr.lm')

# treinamento modelo
base_lm <- train(regr.lm, base_train)

## XGBoost

# Criação do modelo simples
xgb_model <- makeLearner("regr.xgboost")

# Grid de alguns parâmetros
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

# Controle da tunagem randômica
control <- makeTuneControlRandom(maxit = 3) # quantas iterações o modelo aleatório de seleção fará

# Plano de resampling
resample_desc <- makeResampleDesc("CV", iters = 5)

# tunador dos hiper parâmetros
tuned_params_base <- tuneParams(
  learner = xgb_model,
  task = base_train,
  resampling = resample_desc,
  measures = rmse,       # R-Squared performance measure, this can be changed to one or many
  par.set = xgb_params,
  control = control
)

# Modelo tunado
xgb_tuned_base <- setHyperPars(
  learner = xgb_model,
  par.vals = tuned_params_base$x
)

# Verificação da performance do modelo tunado usando as validações cruzadas usadas para tunagem
resample(xgb_tuned_base,base_train,resample_desc,measures = list(rmse, rsq))

# pela grande diferença entre as cvs, o modelo está com bastante overffiting

# Treinamento
base_XGBoost <- train(xgb_tuned_base, base_train)

## Performance - Baseline

# predição nos dados de teste
base_pred_lm <- predict(base_lm, base_test)
performance(base_pred_lm, measures = list(rmse, rsq))

base_pred_xgb <- predict(base_XGBoost, base_test)
performance(base_pred_xgb, measures = list(rmse, rsq))

# usando Predictor$new() para criar um plot das features mais importantes
X = getTaskData(base_test)[getTaskFeatureNames(base_test)]
base_predictor = Predictor$new(base_XGBoost, data = X, y = getTaskData(base_test)['price'])
base_imp = FeatureImp$new(base_predictor, loss = "rmse")
plot(base_imp)

## Feature engineering

# Criação do Task para Regressão (objeto usado nas operações do pacote) FE
fe_task <- makeRegrTask(id = 'Feature Engineering', data = houses_ohe, target = 'price')

# Separação do hold out FE usando os mesmo índices usados no baseline
fe_train <- subsetTask(fe_task, holdout$train.inds[[1]])
fe_test <- subsetTask(fe_task, holdout$test.inds[[1]])

# lm FE
fe_lm <- train(regr.lm, fe_train)

# tunador dos hiper parâmetros FE
tuned_params_fe <- tuneParams(
  learner = xgb_model,
  task = fe_train,
  resampling = resample_desc,
  measures = rmse,       
  par.set = xgb_params,
  control = control
)

# Modelo tunado FE
xgb_tuned_fe <- setHyperPars(
  learner = xgb_model,
  par.vals = tuned_params_fe$x
)

# Verificação da performance do modelo tunado usando as validações cruzadas usadas para tunagem
resample(xgb_tuned_fe,fe_train,resample_desc,measures = list(rmse, rsq))

# cvs bem mais similares, indicando uma generalização mais acurada

# Treinamento
fe_XGBoost <- train(xgb_tuned_fe, fe_train)

## Performance - FE

# predição nos dados de teste
fe_pred_lm <- predict(fe_lm, fe_test)
performance(fe_pred_lm, measures = list(rmse, rsq))

fe_pred_xgb <- predict(fe_XGBoost, fe_test)
performance(fe_pred_xgb, measures = list(rmse, rsq))

# Plotagem das features mais importantes e como a mais relevante influenciou o resultado do modelo

# Plot de como a variável mais importante afeta as previsões na média
pdp_area = Partial$new(base_predictor, feature = "size_house")
plot(pdp_area)