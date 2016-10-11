# fixme: currently due the weird behavior of Xgboost, this code does not run


library(mlr)
library(mlrMBO)
load_all('~/mlrMBO')


data(BreastCancer, package = "mlbench")
df = BreastCancer
df$Id = NULL
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
classif.task

ns = getTaskFeatureNames(classif.task)
lapply(classif.task$env$data, function (x) {class (x)})

trainTask = subsetTask(classif.task, features = ns[6:9])

lrn = makeLearner("classif.xgboost", predict.type = "prob")
ps = makeParamSet(
  makeIntegerParam("nrounds", lower = 30, upper = 60, trafo = function(x) x*10),
  makeNumericParam("eta", lower = -7, upper = -1.5, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = -5, trafo = function(x) 2^x),
  makeIntegerParam("min_child_weight", lower = 0, upper = 5),
  #makeNumericParam("colsample_bytree", lower = 2, upper = 3, trafo = function(x) x/3),
  #makeNumericParam("colsample_bylevel", lower = 2, upper = 3, trafo = function(x) x/3),
  makeNumericParam("subsample", lower = 2, upper = 3, trafo = function(x) x/3),
  makeIntegerParam("max_depth", lower = 8, upper = 15)
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