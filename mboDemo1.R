library(mlr)
library(mlrMBO)
library(ParamHelpers)
library(smoof)



set.seed(1)

objfun1 = makeAckleyFunction(5)
init.points1 = 5 * sum(ParamHelpers::getParamLengths(getParamSet(objfun1)))

getParamSet(objfun1)

design1 = generateDesign(n = init.points1, par.set = getParamSet(objfun1), fun = randomLHS, trafo = FALSE)


surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
surr.rf = makeLearner("regr.randomForest")


# Chain of Responsibility
control1 = makeMBOControl()
control1 = setMBOControlInfill(
  control = control1,
  crit = "ei",
  opt = "focussearch"  # Other options for propose new points are "cmaes","ea","nsga2"(multiObj)
)

?setMBOControlInfill

control1 = setMBOControlTermination(
  control = control1,
  iters = 10
)

control1

mbo1 = mbo(objfun1, design = design1, learner = surr.km, control = control1, 
           show.info = FALSE)

control1$infill.crit = "cb"
control1$iters = 5L
mbo1 = mbo(objfun1, design = design1, learner = surr.km, control = control1, show.info = FALSE)

mbo1

listLearners(obj = "regr", properties = "se")

