# Consider 197098-8.pdf
# This is page 8 of the original multi-page PDF.
# While this is one table, it is explicitly
# separated into two by a horoziontal line
# and there are MEAN, CV, LSD

library(CVRead)
source("readPDF2HTML.R")
source("pdfTables.R")

doc = convertPDF2XML("197098-8.pdf")
bb = readPDF2XML(, doc)

ll = getLines(doc[[1]])

dims = as.integer(xmlAttrs(doc[[1]])[ c("width", "height")])

wdims = c(min(bb[, "left"]), max(bb[, "right"]))

# Skip weird blanks outside of the main body of the text. One at the top left, one at the bottom right

tbl1 = bb[ bb[, "top"] < 988.29 & bb[, "bottom"] > 673.74, ]


rownames(tbl1)  # lots of segments that need to be reassamled.

o1 = getTableFromBBox(tbl1, doc[[1]])

