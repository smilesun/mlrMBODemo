library(mlr)
library(mlrMBO)
library(ParamHelpers)
library(smoof)
obj.fun = makeRastriginFunction(1)

learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
control = makeMBOControl()
control = setMBOControlTermination(control, iters = 10)
control = setMBOControlInfill(control, crit = "ei")
design = generateDesign(n = 10, par.set = getParamSet(obj.fun))


result = mbo(obj.fun, design = design, learner = learner, control = control, show.info = TRUE)

ex = exampleRun(obj.fun, control = control, show.info = FALSE)

print(result)
plotExampleRun(ex)



obj.fun2 = makeRastriginFunction(2L)
design2 = generateDesign(n = 10, par.set = smoof::getParamSet(obj.fun2))
ex2 = exampleRun(obj.fun2, control = control, show.info = FALSE)

print(ex2)
plotExampleRun(ex2)




