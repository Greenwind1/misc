library(meta)
library(grid)
library(broman)
library(extrafont)

data("Olkin1995")

# Olkin I (1995): 
# Statistical and theoretical considerations in meta-analysis. 
# Journal of Clinical Epidemiology, 48, pp.133
# author:   first author
# year:	    year of publication
# ev.exp:   number of events in experimental group
# n.exp:    number of observations in experimental group
# ev.cont:  number of events in control group
# n.cont:   number of observations in control group

m1 <- metabin(
    event.e = ev.exp, 
    n.e = n.exp, 
    event.c = ev.cont, 
    n.c = n.cont,
    data = Olkin1995, 
    subset = c(41, 47, 51, 59),
    sm = "RR", 
    method = "I",
    studlab = paste(author, year)
)

png(file = "fig/forest-plot_01.png", width = 3600, height = 1100, res = 300)
forest.meta(
    x = m1,
    sortvar = TE, 
    layout = "RevMan5",  # c("RevMan5", "JAMA", "subgroup")
    # fixed = FALSE,
    prediction = TRUE, 
    # print.tau2 = FALSE,
    col.fixed = crayons()["Violet Red"],
    col.random = crayons()["Violet Red"],
    col.equi = crayons()["Violet Red"],
    col.study = crayons()["Outer Space"],
    col.square = crayons()["Manatee"],
    col.square.lines = crayons()["Outer Space"],
    col.diamond.fixed = crayons()["Violet Red"],
    col.diamond.random = crayons()["Violet Red"],
    col.diamond.lines = crayons()["Razzmatazz"],
    col.predict = crayons()["Razzmatazz"],
    col.predict.lines = crayons()["snowwhite"],
    col.by = crayons()["Manatee"],
    col.label.right = crayons()["Outer Space"],
    col.label.left = crayons()["Outer Space"],
    label.right = "Favours [control]",
    label.left = "Favours [experimental]",
    leftlabs = c("Author", "g", "SE"),
    fontsize = 12,
    # fontfamily = "candara"
)
dev.off()
