library(Rtesseract)

# The first column is numbers. Right aligned.
# The second is the station name often containing multiple "words". Left aligned
# The remaining columns are numbers and are "always" right aligned, often left aligned also, i.e. all the same width.
#    Not true for 1990 p45, p26

#
# Counter example  1990 p30 (table 24)
#
#


# Table 1990 p35 has text in the body that has an underline and that is not part of a real row.

#
#
# Read bbox with lines
# Discard the black box on the bottom or side
# Find the horizontal lines across the page - should be 3
# Keep everything between the second top line and the bottom line
#  Remove text close to the second top line but under it that is
#   "far" from the next line down.  See Table 39 1990 p45.
#
# Fix the mis-recognized (
# Discard the lines we know we don't want.
# Discard the ranks
#
#
# 
#
#


getTable =
    #
    # The high level function for reading a table from a file.
    #
function(file, ncols = NA, bbox = scanElements(file, show), show = FALSE, ...)
{
  bb = scannedBBox(file, bbox)
  rrows = getRows(bb)
  ans = repairCols(rrows, ncols)
  if(!is.data.frame(ans))
     doit(rrows, length(rrows), ncols, ...)
  else
     ans
}

scanElements =
    #
    #  does the basic OCR to get 
    #
function(file, show = FALSE, segMode = "psm_auto")
{
message("scanElements for ", file, " ", segMode)
    ts = tesseract(file, segMode) # psm_auto gets us the lines in the bounding boxes.
    Recognize(ts)
    bb = BoundingBoxes(ts)
    colnames(bb) = c("left", "bottom", "right", "top")
    if(show)
       plot(ts, bbox = bb)
    bb
}

scannedBBox =
    #
    #  clean up the raw bounding box by removing the black rectangle from the scanning
    #  the page number, find the horizontal lines spanning the table and get the content
    #  of the table's body.  Returns a data frame of the relevant elements for the table's content.
    #
function(file, bbox = scanElements(file))
{
    bbox = cleanBBox(bbox, TRUE)
     
    lines = findLines(bbox)

    bboxAccurate = scanElements(file, segMode = "psm_single_block")
    bboxAccurate = cleanBBox(bboxAccurate, FALSE)
    
#    discardElements(bbox[ - lines, ],  bbox[lines, ])
    discardElements(bboxAccurate,  bbox[lines, ], getFullTableLines(bbox[-lines, ], bbox[lines,]))
}

cleanBBox =
function(bbox, hasLines = TRUE)
{

      # discard the black box from the scanning where the page did not cover the entire scanner region
      # The big black box on the page from the scan.  Could be on the side. See 1996 p28

      # Clean up the little boxes that are scanning "smudges"
      # Could also just use the margins.  
    if(TRUE) { # any(bbox[,1] < 2 & bbox[, 2] < 2)) {
        # horizontal so the page number should be to the left of the table and we need to remove it.
        # Probably the ones identified by  sort(bbox[,1])[2:4]
       i = grep("TABLE", rownames(bbox), ignore.case = TRUE)
       if(length(i)) {
           tt = bbox[i, ]
           bbox = bbox[ bbox[, 1] > tt[1] - 100 & bbox[, 2] > tt[2] - 10, ]
       } else {
           o = order(bbox[,1])
           left = bbox[o[2:10],1]
           d = diff(left[-(2:4)])
           if(any(d > 400) & any( i <- rownames(bbox[o[2:10], ]) == " ")) {
              pos =  min(bbox[ o[2:10], 1][i] ) - 100
              bbox = bbox[  bbox[,1] > pos, ]
           }
           if(hasLines)
              warning("Didn't find the Table title! This could lead to problems")
       }
    }

    bbox = bbox[ bbox[, 1] != 0 & bbox[, 2] > 15, ]  # the bottom > 15 is for e.g. 1990 p41 - line at the top. may already be gone now since we added the TABLE check.
    
    bbox
}


findLines =
    # don't use the name getLines() as we use that in pdfTables.R
    #
    # This finds indices of the horizontal lines in the table.
    # This includes any lines in the body, header, underlines of words and phrases.
    #
function(bbox)
{
   w = bbox[,3] - bbox[,1]
   i = rownames(bbox) == " " & w > 300
   which(i)
}


getFullTableLines =
    #
    #  This gets the lines that span the entire table
    #
function(bbox, lines)
{
#   w = lines[,3] - lines[,1]
   
   w = lines[,1] - min(bbox[, 1]) < 10 & (lines[,3] - max(bbox[, 3]) > - 10)
   if(sum(w) < 3) {
       # see if we have a partial line.
       width = mean(lines[w,3] - lines[w, 1])
       w = w | (lines[,3] - lines[,1]) > .7*width
   }

   if(!any(w))
      warning("couldn't find any lines across the table. Problems are very likely.  Did we discard elements to the left of the table? Did we find the word Table at the top of the document?")
   
   lines[w, ]
#  lines[  lines[,1] - min(bbox[,1])
}


discardElements =
    #
    #  discard  elements that are not in the content/body of the table.
    #
function(bbox, lines = findLines(bbox), ll = getFullTableLines(bbox, lines))
{
    # keep everything between the two lines at the top and bottom of the body of the table.

  ll = ll[ order(ll[, "top"]), ]

  i = bbox[, "top"] > ll[2, "bottom"] & bbox[, "top"] < ll[3, "bottom"] 
  bbox = bbox[ i, ]

   # Find the elements that are under the second line but very close to it, e.g., 1990 p45 &  p 31 - 33
  h = mean(bbox[, "top"] - bbox[, "bottom"])

  d = (bbox[, "bottom"] - ll[2, "top"])
  i = d < 1.*h  # d > 0 ???
  bbox = bbox[ !i, ]

 
  i = which(rownames(bbox) %in% c("MEAN", "Mean" ))
  if(length(i))
      bbox = bbox[ bbox[, "top"] <  bbox[i, "top"] - 10, ]

     # looking for entry and getting the things above it.
     # This should already be done via the horizontal lines.
  if(length(i <- grep("entry", rownames(bbox), ignore.case = TRUE) ))
     bbox = bbox[ bbox[, "top"] > bbox[i , "top"] + 10  , ]


  # Find the elements that were mistakenly not (. Currently (from 1990 p45) these seem
  # to be one character elements  1 i I f
  # These are all rankings that have a single digit and so the ( is separated from the rest
  # and OCR gets it wrong.
  # We'll use the heuristic of looking at the other elements in that column and see if the
  # majority of the text of these start with "("
  #XXX Training tesseract on these characters may get rid of these issues.

  i = which( nchar(rownames(bbox)) == 1 & rownames(bbox) != "(" )
  if(length(i)) {
     w = sapply(bbox[i, "left"], othersInColStartWithParen, bbox[-i, ], mean(bbox[i, "right"] - bbox[i, "left"]))

     if(any(w))
        bbox = bbox[ - i[w], ]
  }

            # Get rid of the rank values
  i = !grepl("^[0-9]+$", rownames(bbox)) & grepl("^[({]$|^\\(?[0-9]+[\\)}1I]$", rownames(bbox))  # The 1I here is for mismatching a )
  bbox = bbox[!i, ]

    # And if  tesseract mistook a digit for a
    # This could discard elements in the header that are properly ended by a parenthesis.
    # e.g. Table 39
  i = grepl("\\)$", rownames(bbox)) & nchar(rownames(bbox)) < 5
  bbox = bbox[!i, ]


   # Too small
  charWidth = mean((bbox[,3] - bbox[,1])/nchar(rownames(bbox)))
  charHeight = mean(bbox[,4] - bbox[,2])

     # see 1990 p10 and a smudge in between the 6 & 7 columns - a 2 pixel box.
  sm = (bbox[,3] - bbox[,1]) > .5*charWidth & (bbox[,4] - bbox[,2]) > .75*charHeight
#  if(!all(sm)) browser()
  bbox = bbox[ sm, ]

  bboxToDF(bbox)
}

othersInColStartWithParen =
    #
    # Find elements that are probably mal-recognized  ( by seeing if others in the same "column" mostly start with (
    #
function(x, bbox, charWidth = 20)
{
   ngbr = abs(bbox[, "left"] - x) < charWidth
   txt = rownames(bbox[ngbr, ])
   length(grep("^\\(", txt)) > .5 * length(txt)
}


getRowSeps =
    #
    # Find the locations that appear to separate the rows.
    #
function(bbox, threshold, height = mean(bbox[, 4] - bbox[, 2]) *.5, col = "bottom")
{
    u = sort(unique(bbox[, col]))
    i = abs(diff(c(0, u))) >  height*.65 # 8   # need to test this on other images/tables
    u[i] - height * .33   # if col is top, add height * .33
}

getRows =
    #
    #  split the elements into rows
    #
function(bbox, sep = getRowSeps(bbox))
{
   rowId = cut(bbox[, "bottom"], c(sep, Inf))
   unname(split(bbox, rowId))
}


# Missing values will cause problems.
repairCols =
    #
    #  This is a simple minded version that just focuses on column 2 as the problematic one.
    #
    #
function(ragRows, ncols = NA)
{
    if(is.na(ncols))
       ncols = mostCommonNum(sapply(ragRows, nrow))        

   ok = sapply(ragRows, nrow) == ncols
   cols = getColPositions(do.call(rbind, ragRows[ok]))
   ragRows[!ok]  = lapply(ragRows[!ok], fixRow, cols, ncols)
    
#browser()
   if(length(unique(sapply(ragRows, nrow))) == 1)
      rowsToTable(ragRows)  # unname(lapply(ragRows, `[[`, "text"))) # do.call(rbind, lapply(ragRows, `[[`, "text"))
   else {
      cat("leaving as list. Expecting", ncols, "columns\n")
      print(sapply(ragRows, nrow))
      ragRows
   }
}

rowsToTable =
function(rows, ncols)
{
   m = do.call(rbind, lapply(rows, `[[`, "text"))

   cols = lapply(seq(ncol(m)), function(i)  convertCol(m[,i]))
   ans = as.data.frame(cols, stringsAsFactors = FALSE)
   names(ans) = sprintf("V%d", seq(ncol(ans)))
   ans
}

fixRow =
function(x, colPos, ncols)
{
   if(nrow(x) < ncols) 
      return(x)

       
   extra = nrow(x) - ncols

   i = seq(2, 2+ extra)
   w = x[i, ]
   x[2, "left"] = min(w[, "left"])
   x[2, "right"] = max(w[, "right"])
   x[2, "top"] = min(w[, "top"])
   x[2, "bottom"] = max(w[, "bottom"])
   x[2, "text"] = paste(w$text, collapse = " ")
   x[- i[-1],]
}

getCharWidth =
function(bb)
   median(  (bb[,3] - bb[,1])/nchar(bb$text)  )    

discoverCols =
function(bbox, nrows, ncols, charWidth = getCharWidth(bb),
         col = "left", minPctRowCells = .3)  # Can also do with right (and middle if in bbox)
{
  bb = orderBBox(bbox, col, FALSE)
  d = diff(c(0, bb[, col]))
  w = d > 2*charWidth
  x = bb[which(w), col]
  
  nn = sapply(x, function(p) sum( abs(bb[, col] - p) < 1*charWidth))

  off = if(col == "left") - charWidth else charWidth
  x[ nn > nrows * minPctRowCells ] + off
}

doit =
function(ragRows, nrows, ncols, charWidth = NA, ...)
{
 message("in doit")
   bb = do.call(rbind, ragRows)
   bb$rowNum = rep(seq(along = ragRows), sapply(ragRows, nrow))
   pos = discoverCols(bb, nrows, ...)
   tmp = split(bb, cut(bb[, 1], c(pos, Inf)))


  # The second and third columns are very special. This is where the parts of the station names
  # get assigned into the third column. So we will examine these two and see if we need to merge them again.
  # Probably also want to count the number of rows in col 3 that don't have a value corresponding to the
  # one word/single elements in col 2.
   k23 = rbind(tmp[[2]], tmp[[3]])
   if(is.na(charWidth)) charWidth = getCharWidth(bb)
   d = by(k23, k23$rowNum, closeTogether, charWidth)
   if(all(d < 2*charWidth)) {
      tmp[[2]] = k23
      tmp = tmp[-3]
   }

   toTable(lapply(tmp[sapply(tmp, nrow) > 0], fixColumn, nrows))
}


closeTogether =
function(lines, charWidth)
{
  if(nrow(lines) == 1)
      return(0)

  i = 2:nrow(lines)
  max(lines[i, "left"] - lines[i-1, "right"])
}



fixColumn =
    #
    # combines elements in the same row and column into a single entry
    # Handles values not present for a row.
    #
function(bbox, nrows)
{
   ans = character(nrows)
   tmp = do.call(rbind, by(bbox, bbox$row, function(x) data.frame(row = x$row[1], text = paste(x$text, collapse = " "))))
   ans[ tmp$row] = as.character(tmp$text)

   fixParens(ans)
}

fixParens =
function(text)
{
   chars = unlist(strsplit(a[[3]], ""))
   if(all(chars %in% c(" ", 0:9, "(", "{", ")")))
      gsub(" ?[\\({].*", "", text)
   else
      text
}
#
foo = 
function(vals, pos)
{

}
