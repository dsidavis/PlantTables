# This is for reading the 2004/ tables.
# It may work for the OCR tables also, but the bouding boxes are less  precisely aligned.

library(CVRead)

#file = "2004/T20WinWheatList04.pdf"
#file = "2004/T28SacDeltaWheat04.pdf"


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


# not ready to call it getTable() !
getColumnData =  
function(file, doc = convertPDF2XML(file),
           #doc = pdfMinerDoc(file, removeHeader = FALSE, removeZeroWidthLines = FALSE),
          show = TRUE,
          footerRX = "^(MEAN|Rating scale|Analysis provided by|Numbers? in parentheses|SOURCE:)",
          ignoreLabels =  "\\( ?[0-9]+\\)",
          showPDF = FALSE, ...)
{
     # Just for my convience so I can call this with getColumnData("T22")
    if(missing(doc) && !file.exists(file))
       file = get2004Filename(file)

    if(showPDF && exists("Open", mode = "function"))  # Duncan's own function to call open() on OSX.
       Open(file)
    
    if(is(doc, "PDFMinerDoc")) 
      bb = getBBox(p, addNames = TRUE)
    else 
      bb = readPDF2XML(doc = doc)

      # Get rid of the cell that starts with "TABLE <number>"
    bb = bb[!grepl("^[[:space:]]*TABLE [0-9]{1,}", rownames(bb)), ]

      # Now see if we can throw out boxes that have lines under them
      # We may actually just know these as CULTIVARS and ADVANCED LINES, TRITICALE
      # But we could find ones with lines under them - BUT in the body of the table. So need to find that first.
      # So move this below bodyY but that causes grief as we 
    bb = bb[!grepl("^CULTIVARS|ADVANCED LINES|TRITICALE$", rownames(bb)), ]    
    

      # Identify cells that are in the footer and then discard the cells below this.
    i = grep(footerRX, rownames(bb))
    if(length(i))
        bb = bb[ bb[,2] > bb[i[1], 2] + 2, ]

      # First page since each PDF has only one page.
    p = doc[[1]]
    lines = getLines(p)

      # Find the body of the table by looking for wide lines
    bodyY = findBody(bb, doc[[1]], lines = lines)
    if(length(bodyY) == 2)
       bb = bb[ bb[, "bottom"] < bodyY[1] &  bb[, "bottom"] > bodyY[2], ]
    else if(is.matrix(bodyY) && nrow(bodyY) > 3) {
        # FOR the first file.
       # many lines hopefully separating all the lines.
#exploring code        
        tmp = rownames(bb)
        dd = as.data.frame(bb)
        rownames(dd) = NULL
        dd$text = tmp
        byLine = split(dd, cut( dd[, "top"], c(Inf, bodyY[, "top"])))
#      rowByLines()
    }

         # Discard any cells in which we are NOT interested, e.g., the ranks.
    if(length(ignoreLabels) && !is.na(ignoreLabels))
       bb = bb[ ! grepl(ignoreLabels, rownames(bb)), ]

      # Discard text elements that appear in the PDF but have no content.
      # These are typically below the header or to the left of the table itself.
    bb = discardBlanks(bb, p)

       # Exploiting contextual knowledge about tables that have just Mean ..... in the last row.
    i = grepl("^Mean|ENTRIES", rownames(bb))
      # If there is a cell with a value Mean, then it is probably below the real content of the table.
      # If so, we want to kill it off. We have to compare it to the other cells  in the tables.
    if(any(i)) {
          # find the other cells in this same row.
        pos = bb[i, c("bottom", "top")] 
        i = bb[, "bottom"] == pos[1] | bb[, "top"] == pos[2]
        if(min(bb[!i, "bottom"]) > pos[2])
          bb = bb[!i,]
    }

       # Move the missing value indicator cells a little bit to the left. They
# This is no longer necessary, but it is okay.    
#    i = rownames(bb) == " -"
#    bb[i, "right"] = bb[i, "right"] - 10 
    
    if(show)
       showBoxes(p, bb, str.cex = .8)

       # Here we go for the idea case that all rows have the same number of cells
       # and that we can then figure out the entire table based on each column's left, right or centered alignment.
       # If we get NULL back, we need to switch to the more heuristic approach in getColsFromBBox.
   ans = guessCells(bb)
   if(!is.null(ans)) {
      ans = toTable( lapply(ans, `[[`, "text") )
      ans = ans[rev(1:nrow(ans)), ]
      return(structure(ans, class = c("RegularGrid", class(ans))))
   }

    invisible(getColsFromBBox(bb, footerRX, show, ...))
}


guessCells =
function(bb)
{
  bb = orderBBox(bb)

  txt = rownames(bb)
  bb = as.data.frame(bb)
  rownames(bb) = NULL
  bb$text = txt


   # if there are bottom values that are close to another box, move them.
   # Have to be careful about this.
#??? Handle more than a 1 unit difference.
#browser()    
  dd = diff(c( bb[1, "bottom"], bb[, "bottom"]))
  for(delta in c(-1, -2)) {
      i = which (dd == delta)  # which(dd < 0 & dd > -3)
      w = bb[, "bottom"] %in% bb[i, "bottom"]
      bb[ w, "bottom"] = bb[w, "bottom"] - delta
      # Need to merge the boxes that are adjacent.
  }


  
#  lines = by(bb, bb[, "bottom"], mergeHorizontalBoxes)
  lines = split(bb, bb[, "bottom"])
  ncells = sapply(lines, nrow)

  if(length(unique(ncells)) != 1) {
     # not all have the same number of cells.
     # let's see if we should collapse some of the boxes that are close together.
     # see merge... below.
      # We may be missing cells in columns. Can we infer this
      # Can we look at the previous column and see if there is " -" there. If so, a missing rank is okay.
      # But we have a chicken and egg problem in that we can identify

      # Compute the most common number of cells, and if all have at least that many, we'll try to repair.
    tt = table(ncells)
    ml = as.integer(names(tt)[which.max(tt)])
    if(!all(ncells >= ml))
       return(NULL)

    # Okay, see if we can repair
    # First get the widths of the columns for the lines we think are correct, i.e. have ml columns.
    # This is for determining if we should allow combining boxes that are not overlapping but slightl
    # separate. This is all for T44.
    colPos = do.call(rbind, lapply(lines[ ncells == ml ], function(x) as.numeric(t(getColPositions(x)))))
    pos = apply(colPos[, seq(1, by = 2, length = ml)], 2, min)
    
    i = ncells > ml

    lines[i] = lapply(lines[i], repairCells, pos)
    if(length(unique(ncells <- sapply(lines, nrow))) > 1) {
      return(NULL)  # failed to combine ...
    }
  }
  
  bbnew = do.call(rbind, lines)
  bbnew$column = rep(1:ncells[1], length(lines)) # all ncell values are the same at this point.

  cols  = split(bbnew, bbnew$column)
}

getColPositions =
    #
    #  for a collection of  cells, get the left and right positions.
    #
function(bb)
{
    bb = orderBBox(bb, "left", FALSE)
    matrix(c(bb[,'left'], bb[, 'right']), , 2, dimnames = list(NULL, c("left", "right")))
                          # c(left = min(bb[, "left"]), right = max( bb[, "right"]))
}

repairCells =
    #
    #  See combineHBoxes and mergeHorizontalBoxes
    #
function(bb, colLocations)
{
    bb = orderBBox(bb, "left")
    i = 1:(nrow(bb)-1)
     # right part of the previous one is beyond the left part of the next one
    w = bb[i, "left"] <= bb[i+1, "right"]
    if(FALSE && any(w)) {
       j = c(which(w), which(w) + 1)
       tmp = combineHBoxes(bb[j,])
       bb = rbind(bb[-j, ], tmp[, c("left", "bottom", "right", "top", "text")])
    } else {
        tt = split(bb, cut(bb[, "left"], c(colLocations, Inf), include.lowest = TRUE, right = FALSE))        
        i = sapply(tt, nrow) > 1
        if(any(i)) {
           tt[i] = lapply(tt[i], combineHBoxes, FALSE)
           do.call(rbind, tt)
        } else
          bb
    }
}



colAlignment =
function(bb)
{
  if(all(bb[, "left"] == bb[1, "left"]))
     return("left")
  else if(all(bb[, "right"] == bb[1, "right"]))
     return("right")

  bb[, "center"] = (bb[, "left"] + bb[, "right"]) / 2
  if(all(bb[, "center"] == bb[1, "center"]))
     return("center")


  w = bb[, "right"] - bb[, "left"]
  if(colAlignmentFuzzy(bb[, "left"], w))
      return("left")
  if(colAlignmentFuzzy(bb[, "right"], w))
      return("right")
  if(colAlignmentFuzzy(bb[, "center"], w))
      return("center")         

  
# Work harder to handle the case
  return(NA)     
}

colAlignmentFuzzy =
function(vals, widths, threshold = .9)    
{
  tt = table(vals)
  (max(tt) >= threshold * length(vals) && diff(range(vals)) < 5)  || diff(range(vals)) < 2 # some function of the widths.
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
  else if(nrow(ll) %in% c(1,  2)) {
        # T14 has broken line segments for the footer so we don't get 3.
        # and it has two lines for the header
        # Compute proortion of cells above the second line and below the second line
     return(c(min(ll[, "bottom"]), 0))
  } else {
     warning("take a look in", docName(page))
     return(ll)
  }
}




getColsFromBBox =
function(bb, footerRX = character(), show = TRUE, threshold = NA,
         numLines = length(unique(bb[, "bottom"])), ...)
{    
    pageWidth = diff(range(bb[, c(1, 3)]))

        # Remove everything from the summary statistics and below.
    if(length(footerRX)) {
       i = grep(footerRX, rownames(bb))
       if(length(i))
          bb = bb[ bb[,2] > bb[i[1], 2] + 2, ]
    }

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
    numLines = length(unique(bb[, "bottom"]))
    if(is.na(threshold)) {
       threshold = ceiling(numLines*.3)
cat("threshold =", threshold, "\n")
    } else if(threshold < 1)
       threshold = threshold * numLines
    
    cols = findCols(bb[, 3], threshold,  ...)
    cols.left = findCols(bb[,1], threshold, ...)
    bb = cbind(bb, center = (bb[, 1]  + bb[, 3])/2)    
    cols.mid = findCols(bb[, "center"], threshold, ...)

if(FALSE) {
browser()        
pos = c(bb[, "left"], bb[, "right"], bb[, "center"])
label = rep(c("l", "r", "c"), each = nrow(bb))
tt = table(round(pos/10))
tt[tt > 10]    
k =  findCols(pos)
adj = label[ match(k, pos)]
}

} 

     # get the mid points between each of the cols.left.
     # Probably need to make this more complex to handle the centered columns, etc.
    splits = c(0, cols.left[-length(cols.left)]) + diff(c(0, cols.left))/4

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

    
     # compute the locations of the lines across the page. Use bottom of the text for now.
    ncols = length(levels(g))
    tt = table(bb[, "bottom"])
    pageLines =  as.numeric( names(tt)[ tt > ncols*.5 ])

    
    cols = by(bb, g, function(x) {
                        x = mergeHorizontalBoxes(x)
                        x = fillMissingCells(x, pageLines)                                                
                        x$text[ order(x[, "bottom"], decreasing = TRUE) ]
                      })

    cols = cols[ sapply(cols, length) > 0 ]
    names(cols) = sapply(cols, `[`, 1)

    cols = lapply(cols, function(x) gsub("\\n", "", x)) #  XML:::trim)

    tbl = toTable(cols)
cat("# columns =", length(cols), class(tbl), "\n")
if(is.list(tbl))
  print(table(sapply(tbl, length)))
    invisible( tbl )
}

locateColumns =
function(bbox, threshold = 10, scale = 10)
{
   align = rep(c("right", "left", "center"), each = nrow(bbox))
   x = c(bbox[, 3], bbox[, 1], (bbox[, 1]  + bbox[, 3])/2)
#   d = data.frame(type = align, pos = x)
#   by(d, d$pos/scale, function(v) )

   findCols(x, threshold, scale)
}

findCols =
function(pos, threshold = 10, scale = 10, numLines = NA)  # length(pos)*.85
{
    tt = table(pos/scale)
    cols = as.numeric(names(tt)[ tt > threshold ])* scale
}


toTable =
    # This attempts to organize the columns with the header, etc into a data frame.
    # It may fail and just return the columns with different lengths.
function(d, dropRanks = TRUE)
{

    if(dropRanks) {
        isRank = sapply(d, function(x) all(grepl("^[[:space:]]*\\([[:space:]]*[0-9]+\\)$", x)))
        d = d[!isRank]
    }
    
    d = lapply(d, function(x) {
                    x[ XML:::trim(x) == "-" ] = NA
                    type.convert(x, as.is = TRUE)
                  })
    
    len = sapply(d, length)
    if(all(len == len[1])) 
       d = as.data.frame(d, stringsAsFactors = FALSE)

    d
}


#############################################

mergeHorizontalBoxes =
function(bbox)
{
   bbox = orderBBox(bbox)    
   tmp = cut(bbox[, "bottom"], unique(c(0,  bbox[, "bottom"] - median(-diff(bbox[, "bottom"]))*.5, Inf)))
   tt = table(tmp)

   if(any(tt > 1)) {
      bbox =  by(bbox, tmp, combineHBoxes)
      bbox = do.call(rbind, bbox)
   }


   bbox
}

combineHBoxes =
    #
    # What about dropping the cells.
    #
function(els, addCenter = TRUE)
{
    # Mark the subscripts in some way in the text e.g. _{val} or [val] or <sub>val</val>
    els = els[ order(els$left), ]
    txt = paste(els$text, collapse = "")
    m = data.frame(left = min(els[,1]), bottom = els[1, 2], right = max(els[,3]), top = max(els[,4]), center = NA, text = txt)
    if(!addCenter)
        m = m[ - 5]
    rownames(m) = txt
    m
}


fillMissingCells =
    #
    # rather than filling them in afterwards and having to move the values, we'll
    # add a cell/box where we think there should be one.
    #
    #  Compute the line height  top-bottom
    #  Then the gap between
    #
    #KISS  get the bottoms and find where the difference between successive 
    #
function(bbox, pageLines = numeric())
{
   bbox = orderBBox(bbox)
#if(nrow(bbox) == 28) browser()
    k = cut(bbox[, "bottom"], pageLines - 1)
    tt = table(k)
    if(any(tt == 0)) {
        ids = names(tt)[ tt == 0]
        b = as.numeric(gsub("\\(([0-9]+(\\.[0-9])?),.*", "\\1", ids))
        n = length(b)
        tmp = data.frame(left = rep(min(bbox[, "left"]), n), bottom = b, right = rep(min(bbox[, "left"]) + 5, n), top = b + 5, center = NA, text = NA)
        bbox = orderBBox( rbind(bbox, tmp))
    }

#   lineHeight = bbox[, "top"] - bbox[, "bottom"]
#   typicalLineDiff = median(-diff(bbox[, "bottom"]))*.5
#   tmp = cut(bbox[, "bottom"], unique(c(0,  bbox[, "bottom"] - , Inf)))



   bbox
}

orderBBox =
function(bbox, colName = "bottom",  decreasing = TRUE)
{
   o = order(bbox[, colName], decreasing = decreasing)
   bbox[o, ]
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
