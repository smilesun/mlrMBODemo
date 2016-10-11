# fixme: currently due the weird behavior of Xgboost, this code does not run

library(mlr)
library(mlrMBO)
load_all('~/mlrMBO')


data = subset(iris.task$env$data, Species != "virginica")
data$Species = as.factor(as.character(data$Species))
trainTask = makeClassifTask(id = "iris2", data = data , target = "Species")

lrn = makeLearner("classif.ksvm", predict.type = "prob")

ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)

obj.fun = makeSingleObjectiveFunction(
  name = "xgboost",
  par.set = ps,
  fn = function(x, lrn, task) {
    library(mlr)
    lrn2 = setHyperPars(lrn, par.vals = as.list(x))
    cv = resample(lrn2, task, cv2, measures = auc)
    res = cv$aggr
    return(-res)
  }
)

set.seed(123)
init = 30L

# Set general controls
ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 50L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch")

lrn.se = makeLearner("regr.km",  predict.type = "se")
initdes = generateDesign(n = init, par.set = ps, fun = lhs::maximinLHS)
initdes

res = mbo(fun = obj.fun, design = initdes, learner = lrn.se, control = ctrl,
          more.args = list(lrn = lrn, task = trainTask))
res
