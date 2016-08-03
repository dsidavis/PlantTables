# To read the first PDF file with the rows separated by lines and with multi-line text within each cell
doc = convertPDF2XML("2004/T01SiteChar04.pdf")
bb = readPDF2XML(, doc)
ll = getLines(doc[[1]])

bb = bb[ !grepl("^ ?TABLE ", rownames(bb)), ]
showBoxes(, bbox = bb)

tt = rownames(bb)
bb = as.data.frame(bb)
bb$text = tt


# Split the cells
byLine = rev( split(bb, cut( bb[, "bottom"],  ll[, "bottom"])) ) # Reverse since ordered from bottom of page to top.

# We canjust proclaim there are 8 columns or we can "infer"
ncolsByLine = by(bb, bb[, "bottom"], nrow)
tt = table(ncolsByLine)
ncol = as.integer(names(tt)[which.max(tt)])

# We'll hope equal.count() gets the column breaks correct. Worth a shot.
tmp = lapply(byLine, function(x) split(x, equal.count(x[, "left"], ncol + 1)))

content = lapply(tmp, function(x) sapply(x, function(x) paste(x$text, collapse = "\n")))

nc = sapply(content, length)
if(length(unique(nc) != 1)) {
 # First byLine is the header.
 # Line 3 has a subscript which we have to take care of and that is why we end up with more entries
 #    we are splitting the FERTILIZATION record int 3 segments rather than one.
 # The 3 and preplant have much larger left values so we need to combine the subscript
 # This is very similar to T44.
 # And we can determine the column locations to see if this is a small gap     
    
  i = nc != ncol
  bad = tmp[i]

  
}

df = do.call(rbind, content)





