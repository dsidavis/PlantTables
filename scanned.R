library(Rtesseract)

# The first column is numbers. Right aligned.
# The second is the station name often containing multiple "words". Left aligned
# The remaining columns are numbers and are "always" right aligned, often left aligned also, i.e. all the same width.
#    Not true for 1990 p45, p26

#
# Counter example  1990 p30 (table 24)
#
# Table 1990 p35 has text in the body that has an underline and that is not part of a real row.

#
#
# Read bbox with lines
# Discard the black box on the bottom or side
# Find the horizontal lines across the page - should be 3
# Find the Agronomy Progress Report trailer at the bottom of some pages. There is a wide line across the page for this.
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
function(file, ncols = NA, bbox = scanElements(file, FALSE), show = FALSE,
          hasLabelsUnderHeaderLine = TRUE, ...)
{
  bb = scannedBBox(file, bbox, show = show,  hasLabelsUnderHeaderLine =  hasLabelsUnderHeaderLine)
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
    bb = getBoxes(ts)
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
function(file, bbox = scanElements(file), show = FALSE, hasLabelsUnderHeaderLine = TRUE)
{
    bbox = cleanBBox(bbox, TRUE)
     
    lines = findLines(bbox)

    bboxAccurate = scanElements(file, segMode = "psm_single_block", show = show)
    bboxAccurate = cleanBBox(bboxAccurate, FALSE)

    if(show) {
       r = par()$usr[4]
       rect(bbox[lines, 1], r - bbox[lines, 2], bbox[lines, 3], r - bbox[lines, 4], col = "green")
    }
    
#    discardElements(bbox[ - lines, ],  bbox[lines, ])
    discardElements(bboxAccurate,  bbox[lines, ], getFullTableLines(bboxAccurate, bbox[lines,]),   # used to pass bbox[-lines, ] as 1st arg. to getFullTableLines()
                     hasLabelsUnderHeaderLine = hasLabelsUnderHeaderLine) 
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


     # In some docments we have a banner across the bottom of the page
     #               Agronomy Progress Report No. ..           pagenumber        Month year
     # e.g. 1997-39.png
     # On the left side vertically in 1997-32.png.
     # Get rid of the text and the box associated with this. If not, it messes up computing the lines.

    i = grep("eport|ogress", rownames(bbox))
    if(length(i)) {  #  (bbox[i[1], "top"] > 1.25*( q <- quantile(bbox[, "top"], .98))))
        tt = bbox[, "top"]
        d = bbox[i[1], "top"] - tt

        if( min(d [ d > 80]) > 200 ) {  # 80 is where the bbox above the agronomy progress report is in 1993-31.
          q = bbox[i[1], "top"] - 80
          cat("discarding values for top >= ", q, "\n")
          bbox = bbox[ bbox[, "top"] < q, ]
        }
    }

    i = specs(bbox)
    if(length(i))
       bbox = bbox[ - i, ]
    
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
    # If the bottom header line is segmented and we cannot identify it, we need a different approach.
    # See 1993-38, 39, 33.
    # In these cases, the scanner has given precedence to text through which the line runs and so
    # segmented the line.  The text is the same HEIGHT and WEIGHT.  So we could recognize this.
    #  More commonly, it is ENTRY
    # The other characteristic is that there are labels below the line from the header (GRAMS) (INCHES), etc.
    #
    #
function(bbox, lines, partial = .2)
{
   # if the most extreme (on the right) element is below all of our potential lines
   # then that is probably in the footer, which we haven't removed yet.  See 1983-18.png
   # There could be several and so we should iterate or do this in one step (vectorized)

   if(nrow(lines) < 3) {
        #    1994-8 missing the 2nd line of the header.
       if(nrow(lines) == 2) {
         i = grep("entry", rownames(bbox), ignore.case = TRUE)
         if(!length(i))
             stop("cannot find the bottom of the table's header")
         v = bbox[i, ]
         v[c(2, 4)] = v[c(2, 4)] + 20 #  getCharHeight(bbox)
         lines = rbind(lines, v)
         lines = lines[order(lines[, 2]), ]
         return(lines)
       }
   }
    
   i = which.max(bbox[,3])
   if(all(bbox[i,3] > lines[,3]))
      bbox = bbox[-i, ]
    
    
   a = lines[,1] - min(bbox[, 1]) < 30  # was arbitrarily 10. But 1994-8 has a problem with the 8th column 703 being .703 because of a spec.
   b = (lines[,3] - max(bbox[, 3]) > - 10)   
   w =  a &  b
   
   if(sum(w) < 3) {
#browser()       
          # see if we have a partial line.
       if(sum(w) == 0) {
            # we're in trouble!
          
       } else {
            # Need to ensure we end up with just 3 lines  header top, header bottom and footer line
            # With partial being quite small to allow for a small line, we are in danger of picking up short lines in the header.
            # So we need to put a constraint, probably that the line has to start with others. We need more empirical evidence.
            # See 1985-46.png
           width = mean(lines[w, 3] - lines[w, 1])
           start = min(lines[w, 1])
           end = min(lines[w, 1])           
           w = w |  ( (lines[,3] - lines[,1]) > partial * width & (lines[,1] - start) < 40)


           if(sum(w) < 3) {
               # Perhaps 2 line segments that are close in vertical location, but broken by tesseract
               # Should make up about 80 % of the width of lines[w,] and be at the same height and below
               # the first line
               # Assumption here is that sum(!w) == 2. Generalize.
               # 1994-23.png
             tableWidth = mean(lines[w, 3] - lines[w, 1])
             tmp = lines[!w,]
             tw = sum( tmp[,3] - tmp[,1])
             th = max( tmp[,4] - tmp[,2])
             if(tw > .7* tableWidth &&  abs(th - max( lines[w,4] - lines[w,2] )) < 20) {
                   # repair the line fragments in lines into a single line.
                lines = lines[w,]
                v = lines[1,]
                v[c(2, 4)] = c(max(tmp[,2]), max(tmp[,4])) # put as low as possible to pick 
                lines = rbind(lines, v)
                lines = lines[order(lines[, 2]), ]
                return(lines)
             }

           }
       }
   }

   if(!any(w))
      warning("couldn't find any lines across the table. Problems are very likely.  Did we discard elements to the left of the table? Did we find the word Table at the top of the document?")
   
   lines[w, ]
}


discardElements =
    #
    #  discard  elements that are not in the content/body of the table.
    #
function(bbox, lines = findLines(bbox), ll = getFullTableLines(bbox, lines), hasLabelsUnderHeaderLine = TRUE)
{
    # keep everything between the two lines at the top and bottom of the body of the table.

  i = which(rownames(bbox) %in% c("MEAN", "Mean", "AVE" ))
  if(length(i))
      bbox = bbox[ bbox[, "top"] <  bbox[i, "top"] - 10, ]

     # looking for entry and getting the things above it.
     # This should already be done via the horizontal lines.
  if(length(i <- grep("entry", rownames(bbox), ignore.case = TRUE) ))
     bbox = bbox[ bbox[, "top"] > bbox[i , "top"] + 10  , ]
    

  ll = ll[ order(ll[, "top"]), ]

  i = bbox[, "top"] > ll[2, "bottom"] & bbox[, "bottom"] < ll[3, "top"] 
  bbox = bbox[ i, ]



  h = mean(bbox[, "top"] - bbox[, "bottom"])
  amtBelowLine = .16 * h       
  if(is.numeric(hasLabelsUnderHeaderLine)) {
    amtBelowLine = hasLabelsUnderHeaderLine
    hasLabelsUnderHeaderLine = TRUE
  }
  
  if(hasLabelsUnderHeaderLine) {
      # Find the elements that are under the second line but very close to it, e.g., 1990 p45 &  p 31 - 33
  
     d = (bbox[, "bottom"] - ll[2, "top"])
     i = d < amtBelowLine   # scale factor used to be 1. But 1997-16 has the first row very close to this lower line of the header. Works for 1990-12 though.
     bbox = bbox[ !i, ]
  }



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
    
   if(length(unique(sapply(ragRows, nrow))) == 1)
      rowsToTable(ragRows)  # unname(lapply(ragRows, `[[`, "text"))) # do.call(rbind, lapply(ragRows, `[[`, "text"))
   else {
      cat("leaving as list. Expecting", ncols, "columns\n")
      print(sapply(ragRows, nrow))
      ragRows
   }
}

rowsToTable =
    #
    # Converts the list of row each with its own collection of values into a data frame.
    # It attempts to convert each column into the appropriate type - numeric, logical, etc.
    #
function(rows, ncols)
{
   m = do.call(rbind, lapply(rows, `[[`, "text"))

   cols = lapply(seq(ncol(m)), function(i)  convertCol(m[,i]))
   ans = as.data.frame(cols, stringsAsFactors = FALSE)
   names(ans) = sprintf("V%d", seq(ncol(ans)))
   ans
}

fixRow =
    #
    # This focuses on the second column. When there are more columns/entries than expected (ncols)
    # it takes the 2, 3, ... and combines them into the second column.
    #
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
{
   txt = if(is.data.frame(bb))  bb$text else rownames(bb)
   median(  (bb[,3] - bb[,1])/nchar(txt)  )
}

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

   ans = toTable(lapply(tmp[sapply(tmp, nrow) > 0], fixColumn, nrows))
   
   checkFirstRow(ans)
}

checkFirstRow =
    #
    # When we don't detect the characters/elements just below the header line, if they exist and not close enough to that line,
    # they will be included as the first row. So we check here to see if they are all in () or NA or ""
    #
    # One can also control this in the call to getTable() with hasLabelsUnderHeaderLine = TRUE/FALSE or a number
    # but that is more messing about with the correct offset.
    #
function(tbl)
{
   els = unlist( tbl[1, ])
   els = els[!is.na(els) & els != ""]
   if(all(grepl("^\\(.*\\)", els)))
      tbl = tbl[-1, ]
   else
      tbl
}


closeTogether =
    #
    # compute the distance between successive elements in a bbox
    #
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
    #
    # For the values in a column, see if any have extra content that comes from  ranks
    #  e.g. "540 (9"
    # We check to see if all of the characters in this column are numbers or ( ) (or { due to errors in OCR)
function(text)
{
   chars = unlist(strsplit(text[[3]], ""))
   if(all(chars %in% c(" ", 0:9, "(", "{", ")")))
      gsub(" ?[\\({].*", "", text)
   else
      text
}


specs =
function(bbox, threshold = 5, charWidth = getCharWidth(bbox))
{
  w = bbox[,3] - bbox[,1]
  h = bbox[,4] - bbox[,2]
  i = which( w < threshold & h < threshold )
  D = as.matrix(dist(bbox))
  m = apply(D[i, , drop = FALSE], 1, function(x) sort(x)[2])
  i [  m > 3*charWidth ]
}
