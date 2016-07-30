# tbl.files = list.files("APR_before_1998", pattern = "1981-118-([4-9]|[0-9]{2}).png", full.names = TRUE)
ocrWords = 
function(file, ...)
{
   ocr(file, opts = c(tessedit_char_blacklist = "{}£ﬂ”‘ﬁ"))
}

ocrDoc =
function(filename, ...)
{
   
}


getTable =
function(filename, ts = tesseract(filename, ...), ...)
{
browser()    
    Recognize(ts)
    bb = BoundingBoxes(ts, "word")
    i = grep("grand|mean", rownames(bb), ignore.case = TRUE)
    if(length(i))
      bb = bb[ bb[, "top.right.y"] <  bb[i[1], "top.right.y"] - 5, ]

    i = grep("table", rownames(bb), ignore.case = TRUE)
    if(length(i))
      bb = bb[ bb[, "top.right.y"] >  bb[i[1], "top.right.y"] + 5, ]
# Zoom in and redoc ocr?

    plot(ts, img = readPNG(filename), bbox = bb)

     nlines = numLines(bb)

tmp = sort(table(bb[,1]/10), decreasing = TRUE)
w = as.integer(names(tmp)[tmp > 4])*10
abline(v = w, col = "orange")

    mi = 10^(nchar(as.integer(max(bb)))-2)
    tmp = table(round(bb[,1]/100))
    abline(v = as.integer(names(tmp))*mi, col = "darkgreen")

 D = as.matrix(dist(bb[, c(1, 3)]))
# quantile
# unique round values.
# Take first few lines to get between distances and then use these on other lines
    # skip the header first. How to identify header.
    # Can we identify the lines? Ask leptonica.
# clustering
}

numLines =
    #
    # group the boxes by vertical position and see how many lines there are
    # This is very similar to columns also!!!
    #
function(bbox, threshold = 20)
{
    cumsum(diff(sort(bbox[, 4])) > threshold)
}
