library(Rtesseract)

# The first column is numbers. Right aligned.
# The second is the station name often containing multiple "words". Left aligned
# The remaining columns are numbers and are "always" right aligned, often left aligned also, i.e. all the same width.
#    Not true for 1990 p45, p26
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
# Discard the lines we know we don't want.
# Discard the ranks
#
#
# 
#
#

scanElements =
function(file)
{
    ts = tesseract(file, "psm_auto") # get the lines in the bounding boxes.
    Recognize(ts)
    bb = BoundingBoxes(ts)
    colnames(bb) = c("left", "bottom", "right", "top")
    bb
}

scannedBBox =
function(file, bbox = scanElements(file))
{

      # discard the black box from the scanning where the page did not cover the entire scanner region
      # The big black box on the page from the scan.  Could be on the side. See 1996 p28    
    bbox = bbox[ bbox[, 1] != 0, ]

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
   w = lines[,1] - min(bbox[, 1]) < 10 &  lines[,3] - max(bbox[, 3]) > - 10
   lines[w, ]
#  lines[  lines[,1] - min(bbox[,1])
}


discardElements =
function(bbox, lines = findLines(bbox))
{
browser()    
    # everything between the two lines at the top and bottom of the body of the table.
  ll = getFullTableLines(bbox, lines)
  ll = ll[ order(ll[, "top"]), ]

  i = bbox[, "top"] > ll[2, "bottom"] & bbox[, "top"] < ll[3, "bottom"] 
  bbox = bbox[ i, ]

   # Find the elements that are under the second line but very close to it, e.g., 1990 p45 &  p 31 - 33
  h = mean(bbox[, "top"] - bbox[, "bottom"])
  d = (bbox[, "bottom"] - ll[2, "top"])
  i = d > 0 & d < .7*h
  bbox = bbox[ !i, ]

  i = which(rownames(bbox) == "MEAN")
  if(length(i))
      bbox = bbox[ bbox[, "top"] <  bbox[i, "top"] - 10, ]

  if(length(i <- grep("entry", rownames(bbox), ignore.case = TRUE) ))
     bbox = bbox[ bbox[, "top"] > bbox[i , "top"] + 10  , ]

            # Get rid of the rank values
  i = grepl("^[({]$|^\\(?[0-9]+[\\)}]$", rownames(bbox))
  bbox = bbox[!i, ]

    # And if  tesseract mistook a digit for a
    # This could discard elements in the header that are properly ended by a parenthesis.
    # e.g. Table 39
  i = grepl("\\)$", rownames(bbox))
  bbox = bbox[!i, ]

  bboxToDF(bbox)
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
   split(bbox, rowId)
}
