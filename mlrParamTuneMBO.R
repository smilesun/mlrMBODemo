# readme: tuning a classification svm
library('devtools')
library('mlr')
load_all('~/mlrMBO')

############################################
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 50L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch")

surrogate.se = makeLearner("regr.km",  predict.type = "se") # note that this learner is surrogate!

ctrl = makeTuneControlMBO(learner = surrogate.se, mbo.control = ctrl)

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


