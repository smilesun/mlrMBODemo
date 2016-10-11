library(mlr)
library(mlrMBO)
library(ParamHelpers)
library(smoof)

set.seed(1)

foo = function(x) {  # expect input to be a matrix or list
  j = x[[1]]
  k = x[[2]]
  method = x[[3]]
  perf = ifelse(method == "a", k * sin(j) + cos(j), sin(j) + k * cos(j))
  return(perf)
}

objfun2 = makeSingleObjectiveFunction(
  name = "example",
  fn = foo,
  par.set = makeParamSet(
    makeNumericParam("j", lower = 0,upper = 1),  # Angel in rad
    makeIntegerParam("k", lower = 1L, upper = 2L),
    makeDiscreteParam("method", values = c("a", "b"))
  ),
  has.simple.signature = FALSE,
  minimize = FALSE
)

objfun2(list(j = 0.5, k = 1L, method = "a"))


init.points2 = 5 * sum(ParamHelpers::getParamLengths(getParamSet(objfun2)))
design2 = generateDesign(n = init.points2, par.set = getParamSet(objfun2), fun.args = list(k = 3, dup = 4), fun = maximinLHS, trafo = FALSE)


surr.rf = makeLearner("regr.randomForest")

control2 = makeMBOControl()
control2 = setMBOControlInfill(
  control = control2,
  crit = "mean",
  opt = "focussearch"
)
control2 = setMBOControlTermination(
  control = control2,
  iters = 10
)
mbo2 = mbo(objfun2, design = design2, learner = surr.rf, 
           control = control2, show.info = FALSE)

learner_rf = makeLearner("regr.randomForest", predict.type = "se")
control2$infill.crit = "ei"
mbo2 = mbo(objfun2, design = design2, learner = learner_rf, 
           control = control2, show.info = FALSE)
mbo2
