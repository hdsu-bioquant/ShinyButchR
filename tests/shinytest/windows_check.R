app <- ShinyDriver$new("../../")
app$snapshotInit("windows_check")

app$setInputs(mainmenu = "nmfplots")
app$setInputs(mainmenu = "saveres")
app$setInputs(mainmenu = "faq")
app$snapshot()
