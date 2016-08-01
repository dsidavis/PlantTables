# This is for reading the 2004/ tables.
# It may work for the OCR tables also, but the bouding boxes are less  precisely aligned.

library(CVRead)

file = "2004/T20WinWheatList04.pdf"
file = "2004/T28SacDeltaWheat04.pdf"

# not ready to call it getTable() !
getColumnData =  
function(file, doc = convertPDF2XML(file),
           #doc = pdfMinerDoc(file, removeHeader = FALSE, removeZeroWidthLines = FALSE),
          show = TRUE,
          footerRX = "^(MEAN|Rating scale|Analysis provided by|Numbers? in parentheses|SOURCE:)", ...)
{
     # Just for my convience so I can call this with getColumnData("T22")
    if(!file.exists(file))
      file = grep(paste0("^2004/", file), list.files("2004", pattern = "pdf$", full = TRUE), value = TRUE)

#Open(file)
    
    if(is(doc, "PDFMinerDoc")) {
      p = doc[[1]]
      bb = getBBox(p, addNames = TRUE)
    } else {
       bb = readPDF2XML(doc = doc)
    }

#    lines = getLines(file)
    
    if(show)
       showBoxes(, bb, str.cex = .8)

    invisible(getColsFromBBox(bb, footerRX, show))
}

library(Rtesseract)
getScannedCols =
function(file, show = TRUE)
{
    ts = tesseract(file)
    Recognize(ts)
    bbox = BoundingBoxes(ts)

    if(show) 
       plot(ts, bbox = bbox, cropToBoxes = TRUE, margin = .005)
    
    colnames(bbox) = c("left", "bottom", "right", "top")
    m = max(bbox[, c(2,4)])
    bbox[,c(2, 4)] = m - bbox[, c(2,4)]

    
    getColsFromBBox(bbox, "GRAND|MEAN", show)
}

getColsFromBBox =
function(bb, footerRX, show = TRUE, ...)
{    
    pageWidth = diff(range(bb))


        # Remove everything from the summary statistics and below.
    i = grep(footerRX, rownames(bb))
    if(length(i))
        bb = bb[ bb[,2] > bb[i[1], 2] + 2, ]

    bb = bb[ ! grepl("^TABLE", rownames(bb)), ]


#if(any( i <- ((bb[,3] - bb[,1]) > .25*pageWidth)))
#    browser()


# See if we can identify horizontal lines that span the extent of the text
# as these are separators of the headers and content.
# Fix pdfminer to emit these.
#
# For now, we'll assume we can identify these.
# Same also with the sub headers that separate parts of the table
# e.g., under CULTIVARS, ADVANCED LIES, TRITICALE

#    bb = cbind(bb, center = (bb[, 1]  + bb[, 3])/2)


if(TRUE)
    cols.left = locateColumns(bb, ...)
else {    
    cols = findCols(bb[, 3])
    cols.left = findCols(bb[,1])
    cols.mid = findCols(bb[, "center"])
} 

     # get the mid points between each of the cols.left.
     # Probably need to make this more complex to handle the centered columns, etc.
    splits = c(0, cols.left[-length(cols.left)]) + diff(c(0, cols.left))/2
    
    if(show){
#       abline(v = cols, col = "red")        
#       abline(v = cols.left, col = "blue")
#       abline(v = cols.mid, col = "green")
        abline(v = splits, col = "lightgrey", lty = 2, lwd = 2)
    }


    g = cut(bb[,1], c(splits, max(bb) + 10)) 

    vals = rownames(bb)
    bb = as.data.frame(bb)
    bb$text = vals

    cols = by(bb, g, function(x) x$text[ order(x[,2], decreasing = TRUE) ])

    cols = cols[ sapply(cols, length) > 0 ]
    names(cols) = sapply(cols, `[`, 1)

    cols = lapply(cols, function(x) gsub("\\n", "", x)) #  XML:::trim)

    tbl = toTable(cols)
cat("# columns =", length(cols), "\n")    
    invisible( cols )
}

locateColumns =
function(bbox, threshold = 10, scale = 10)
{
   align = rep(c("right", "left", "center"), each = nrow(bbox))
   x = c(bbox[, 3], bbox[, 1], (bbox[, 1]  + bbox[, 3])/2)
#   d = data.frame(type = align, pos = x)
#   by(d, d$pos/scale, function(v) )
browser()    
   findCols(x, threshold, scale)
}

findCols =
function(pos, threshold = 10, scale = 10)  # length(pos)*.85
{
    tt = table(pos/scale)
    cols = as.numeric(names(tt)[ tt > threshold ])* scale
}


toTable =
    # This attempts to organize the columns with the header, etc into a data frame.
    # It may fail and just return the columns with different lengths.
function(d)
{


    d
}


checkMeans =
function(d)
{
    sapply(d, checkMean)
}

checkMean =
function(x)
{
      # remove any (rankNum)
   x = gsub("\\([0-9]+\\)", "", x)
   vals =  grep("^[0-9]+(\\.[0-9]+)?$", x, value = TRUE)
   mean(as.numeric(vals))
}
