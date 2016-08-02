# This is for reading the 2004/ tables.
# It may work for the OCR tables also, but the bouding boxes are less  precisely aligned.

library(CVRead)

#file = "2004/T20WinWheatList04.pdf"
#file = "2004/T28SacDeltaWheat04.pdf"

# not ready to call it getTable() !
getColumnData =  
function(file, doc = convertPDF2XML(file),
           #doc = pdfMinerDoc(file, removeHeader = FALSE, removeZeroWidthLines = FALSE),
          show = TRUE,
          footerRX = "^(MEAN|Rating scale|Analysis provided by|Numbers? in parentheses|SOURCE:)", showPDF = FALSE, ...)
{
     # Just for my convience so I can call this with getColumnData("T22")
    if(missing(doc) && !file.exists(file))
      file = grep(paste0("^2004/", file), list.files("2004", pattern = "pdf$", full = TRUE), value = TRUE)
if(showPDF)
Open(file)
    
    if(is(doc, "PDFMinerDoc")) 
      bb = getBBox(p, addNames = TRUE)
    else 
      bb = readPDF2XML(doc = doc)

    bb = bb[!grepl("^TABLE [0-9]{1,}", rownames(bb)), ]
      # Now see if we can throw out boxes that have lines under them
      # We may actually just know these as CULTIVARS and ADVANCED LINES
    bb = bb[!grepl("^CULTIVARS|ADVANCED LINES|TRITICALE$", rownames(bb)), ]    
    
#browser()
    i = grep(footerRX, rownames(bb))
    if(length(i))
        bb = bb[ bb[,2] > bb[i[1], 2] + 2, ]

    p = doc[[1]]
    lines = getLines(p)


      # Find the body of the table by looking for wide lines
    bodyY = findBody(bb, doc[[1]], lines = lines)
    if(length(bodyY))
       bb = bb[ bb[, "bottom"] < bodyY[1] &  bb[, "bottom"] > bodyY[2], ]


    bb = discardBlanks(bb, p)



       # Exploiting contextual knowledge about tables that have just Mean ..... in the last row.
    i = grepl("^Mean|ENTRIES", rownames(bb))
      # If there is a cell with a value Mean, then it is probably on the bottom row.
      # If so, we want to kill it off. We have to compare it to the other cells  in the tables.
    if(any(i)) {
          # find the other cells in this same row.
        pos = bb[i, c("bottom", "top")] 
        i = bb[, "bottom"] == pos[1] | bb[, "top"] == pos[2]
        if(min(bb[!i, "bottom"]) > pos[2])
          bb = bb[!i,]
    }
    
    
    if(show)
       showBoxes(, bb, str.cex = .8)

    invisible(getColsFromBBox(bb, footerRX, show))
}

discardBlanks =
function(bbox, page )
{
   i = rownames(bbox) == " "
   if(!any(i))
      return(bbox)

        # the blanks are above all the non-blanks, to the left of all the non blanks, or below all the non-blanks
   w = bbox[i, "bottom"] > max(bbox[!i, "top"]) | bbox[i, "left"] < min(bbox[!i, "left"]) | bbox[i, "top"] < max(bbox[!i, "bottom"])

   
   bbox[ - which(i)[w], ]
}

findBody =
    #
    # Find lines that span across the page that may identify the header and footer.
    #
function(bbox, page, threshold = .75, linesBB = getLines(page))
{

# T14 file has two line segments at the bottom of the table that combined span the table
# but they are two separate segments that don't quite connect.
    
  page.dims = as.numeric(xmlAttrs(page)[c("height", "width")])

  w = linesBB[, "right"] - linesBB[, "left"]
#  isAcrossPage = w > threshold*page.dims[2]  # names are discarded in the coercion.
  boxSpan = diff(range(bbox[, c("left", "right")]))
  isAcrossPage = w > boxSpan
  
  if(!any(isAcrossPage))
     return(numeric()) # c(page.dims[1], 0))

  ll = linesBB[isAcrossPage,]
  ll = ll[order(ll[, "bottom"], decreasing = TRUE), ]
  if(nrow(ll) == 3)
    ll[2:3, "bottom"]
  else {
     warning("take a look")
     return(numeric())
  }
}


# For the scanned documents.
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



if(FALSE)
    cols.left = locateColumns(bb, ...)
else {    
    cols = findCols(bb[, 3], ...)
    cols.left = findCols(bb[,1], ...)
    bb = cbind(bb, center = (bb[, 1]  + bb[, 3])/2)    
    cols.mid = findCols(bb[, "center"], ...)
} 

     # get the mid points between each of the cols.left.
     # Probably need to make this more complex to handle the centered columns, etc.
    splits = c(0, cols.left[-length(cols.left)]) + diff(c(0, cols.left))/2
    
    if(show){
       abline(v = cols, col = "red")        
       abline(v = cols.left, col = "blue")
       abline(v = cols.mid, col = "green")
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
cat("# columns =", length(cols), class(tbl), "\n")
if(is.list(tbl))
  print(sapply(tbl, length))   
    invisible( tbl )
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
    d = lapply(d, function(x) {
                    x[ XML:::trim(x) == "-" ] = NA
                    type.convert(x, as.is = TRUE)
                  })
    
    len = sapply(d, length)
    if(all(len == len[1])) 
       d = as.data.frame(d, stringsAsFactors = FALSE)

    d
}


# This is just for helping to verify the results quickly

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
