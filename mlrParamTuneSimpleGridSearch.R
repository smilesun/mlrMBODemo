#library(mlr)
load_all('~/mlr')
############################################
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)

ctrl = makeTuneControlGrid(resolution = 2L)



rdesc = makeResampleDesc("CV", iters = 2L)
res = tuneParams(learner = "classif.ksvm", iris.task, rdesc, par.set = ps, control = ctrl)
# learner could also be self defined, in which case it is no longger a character string
print(res)
# access data for all evaluated points
print(head(as.data.frame(res$opt.path)))
print(head(as.data.frame(res$opt.path, trafo = TRUE)))
# access data for all evaluated points - alternative
print(head(generateHyperParsEffectData(res)))
print(head(generateHyperParsEffectData(res, trafo = TRUE)))

