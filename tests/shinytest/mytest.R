app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(load_demo = "click")
app$snapshot()
app$setInputs(startNMF = "click")
