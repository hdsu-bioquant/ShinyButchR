app <- ShinyDriver$new("../../")
app$snapshotInit("load_demo")

app$setInputs(load_demo = "click")
app$snapshot()
