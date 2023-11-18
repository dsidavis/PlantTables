library(Rtesseract)

f = "1990_p44.png"
ts = tesseract(f)

Recognize(ts)
bb = getBoxes(ts)

library(png)
img = readPNG(GetInputName(ts))
plot(ts, img = img, cropToBoxes = FALSE)  # TRUE)

# Throw away the boxes  from MEAN onwards. These are summaries we can recompute.
# This assumes we recognized the word MEAN correctly.

i = which(rownames(bb) == "MEAN")
if(length(i))
  bb = bb[ bb[, "top.right.y"] <  bb[i, "top.right.y"], ]

plot(ts, img = img, bbox = bb, cropToBoxes = TRUE)



ts.words = ocr(f)
# The number of mistakes in manually transcribing the text from p44 to create 1990_p44.txt
# illustrates just how error prone manually copying the data can be.
words = scan("1990_p44.txt", "")
stopifnot(length(words) == length(ts.words))

names(ts.words) = gsub("{", "(", gsub("}", ")", names(ts.words), fixed = TRUE), fixed = TRUE)

w = toupper(words) == toupper(names(ts.words))
b = cbind(names(ts.words), words)
d = data.frame(ocr = names(ts.words)[!w], true = words[!w], confidence = ts.words[!w], rownames = NULL)


compareWords(names(ts.words), words)

# 10% wrong after adjusting for { and } for ( and )


# Errors  { or } for ( and )


# If we know When we determine where the columns are
