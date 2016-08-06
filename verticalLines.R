library(CVRead)
source("readPDF2HTML.R")

doc = xmlParsePDFTOTHML("197098-7.xml")
bb = readPDF2XML(, doc)

showBoxes(doc[[1]], bb)

# In this page, we hage 3 tables.
# The tables at the top of the page have headers, the one on the bottom inherits the headers.
# There are 7 columns in each - each with 2 columns of numbers separated by a vertical line.
# So there are 14 columns.
#
# We probably want to break up the tables into separate units.
# Each table has a top and bottom line.
#
#
