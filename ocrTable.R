library(Rtesseract)
library(png)
source("pdfTables.R")


ocrTable =
function(file = "1990_p44.png", img = readPNG(file))
{
    
  ts = tesseract(file)
  Recognize(ts)
  bb = BoundingBoxes(ts)
  colnames(bb) = c("left", "bottom", "right", "top")

  plot(ts, img = img, cropToBoxes = FALSE) # # TRUE)

# Throw away the boxes  from MEAN onwards. These are summaries we can recompute.
# This assumes we recognized the word MEAN correctly.

  i = which(rownames(bb) == "MEAN")
  if(length(i))
      bb = bb[ bb[, "top"] <  bb[i, "top"], ]

  
  plot(ts, img = img, bbox = bb, cropToBoxes = TRUE)

  if(length(i <- grep("ENTRY", rownames(bb))))
     bb = bb[ bb[, "top"] > bb[i , "top"] + 10  , ]


           # Get rid of the rank values
  i = grepl("^[({]$|^\\(?[0-9]+\\)$", rownames(bb))
  bb = bb[!i, ]

  plot(ts, img = img, bbox = bb, cropToBoxes = TRUE)

    # The heights are backwards, i.e. going from top to bottom. So need to reverse order at the end.

  o = guessCells(bb)

  if(is.null(o))
     o = getColsFromBBox(bb, threshold = 7, numLines = 18)

  o
}
