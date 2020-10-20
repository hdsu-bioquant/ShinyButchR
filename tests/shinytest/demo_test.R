app <- ShinyDriver$new("../../")
app$snapshotInit("demo_test")

app$setInputs(load_demo = "click")
app$setInputs(params_kmax = 6)
app$setInputs(params_convthrs = 5)
app$setInputs(startNMF = "click")
app$snapshot()