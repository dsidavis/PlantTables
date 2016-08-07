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
function(file, ncols = NA, bbox = scanElements(file, show), show = FALSE)
{
  bb = scannedBBox(, bbox)
  repairCols(getRows(bb), ncols)
}

scanElements =
function(file, show = FALSE)
{
    ts = tesseract(file, "psm_auto") # get the lines in the bounding boxes.
    Recognize(ts)
    bb = BoundingBoxes(ts)
    colnames(bb) = c("left", "bottom", "right", "top")
    if(show)
       plot(ts, bbox = bb)
    bb
}

scannedBBox =
function(file, bbox = scanElements(file))
{

      # discard the black box from the scanning where the page did not cover the entire scanner region
      # The big black box on the page from the scan.  Could be on the side. See 1996 p28

      # Clean up the little boxes that are scanning "smudges"
      # Could also just use the margins.  
    if(TRUE) { # any(bbox[,1] < 2 & bbox[, 2] < 2)) {
        # horizontal so the page number should be to the left of the table and we need to remove it.
        # Probably the ones identified by  sort(bbox[,1])[2:4]
       i = grep("TABLE", rownames(bbox))
       tt = bbox[i, ]
       bbox = bbox[ bbox[, 1] > tt[1] - 100 & bbox[, 2] > tt[2] - 10, ]
    }

     
    bbox = bbox[ bbox[, 1] != 0 & bbox[, 2] > 15, ]  # the bottom > 15 is for e.g. 1990 p41 - line at the top. may already be gone now since we added the TABLE check.

    lines = findLines(bbox)
    
    discardElements(bbox[ - lines, ],  bbox[lines, ])
}


findLines =
    # don't use getLines() as the name as we use that in pdfTables.R
function(bbox)
{
   w = bbox[,3] - bbox[,1]
   i = rownames(bbox) == " " & w > 300
   which(i)
}


getFullTableLines =
function(bbox, lines)
{
   w = lines[,3] - lines[,1]
   
   w = lines[,1] - min(bbox[, 1]) < 10 & (lines[,3] - max(bbox[, 3]) > - 10)
   if(sum(w) < 3) {
       # see if we have a partial line.
       width = mean(lines[w,3] - lines[w, 1])
       w = w | (lines[,3] - lines[,1]) > .7*width
   }
   lines[w, ]
#  lines[  lines[,1] - min(bbox[,1])
}


discardElements =
function(bbox, lines = findLines(bbox))
{
    # everything between the two lines at the top and bottom of the body of the table.
  ll = getFullTableLines(bbox, lines)
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

  if(length(i <- grep("entry", rownames(bbox), ignore.case = TRUE) ))
     bbox = bbox[ bbox[, "top"] > bbox[i , "top"] + 10  , ]


  # Find the elements that were mistakenly not (. Currently (from 1990 p45) these seem
  # to be one character elements  1 i I f
  # These are all rankings that have a single digit and so the ( is separated from the rest
  # and OCR gets it wrong.
  # We'll use the heuristic of looking at the other elements in that column and see if the
  # majority of the text of these start with "("
  #XXX Training tesseract on these characters may get rid of these issues.

  i = which(nchar(rownames(bbox)) == 1 & rownames(bbox) != "(")
  if(length(i)) {
     w = sapply(bbox[i, "left"], othersInColStartWithParen, bbox[-i, ], mean(bbox[i, "right"] - bbox[i, "left"]))

     if(any(w))
        bbox = bbox[ - i[w], ]
  }


            # Get rid of the rank values
  i = grepl("^[({]$|^\\(?[0-9]+[\\)}]$", rownames(bbox))
  bbox = bbox[!i, ]

    # And if  tesseract mistook a digit for a
    # This could discard elements in the header that are properly ended by a parenthesis.
    # e.g. Table 39
  i = grepl("\\)$", rownames(bbox))
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
function(x, bbox, charWidth = 20)
{
   ngbr = abs(bbox[, "left"] - x) < charWidth
   txt = rownames(bbox[ngbr, ])
   length(grep("^\\(", txt)) > .5 * length(txt)
}


getRowSeps =
function(bbox, threshold, height = mean(bbox[, 4] - bbox[, 2]) *.5, col = "bottom")
{
    u = sort(unique(bbox[, col]))
    i = abs(diff(c(0, u))) >  height*.65 # 8   # need to test this on other images/tables
    u[i] - height * .33   # if col is top, add height * .33
}

getRows =
function(bbox, sep = getRowSeps(bbox))
{
   rowId = cut(bbox[, "bottom"], c(sep, Inf))
   unname(split(bbox, rowId))
}


# Missing values will cause problems.
repairCols =
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
      cat("leaving as list\n")
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

