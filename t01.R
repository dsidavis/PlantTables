# To read the first PDF file with the rows separated by lines and with multi-line text within each cell
tableSepByLine =
function(file, show = FALSE)
{    
    doc = convertPDF2XML(file)
    bb = readPDF2XML(, doc)
    ll = getLines(doc[[1]])

    bb = bb[ !grepl("^ ?TABLE ", rownames(bb)), ]
    if(show)
        showBoxes(, bbox = bb, str.cex = .8)

    bb = bboxToDF(bb)

                                        # Split the cells
    byLine = rev( split(bb, cut( bb[, "bottom"],  ll[, "bottom"])) ) # Reverse since ordered from bottom of page to top.

    header = byLine[[1]]
    byLine = byLine[-1]

                                        # We can just proclaim there are 8 columns or we can "infer". We do the latter.
    ncolsByLine = by(bb, bb[, "bottom"], nrow)
    ncol = mostCommonNum(ncolsByLine)

                                        # We'll hope equal.count() gets the column breaks correct. Worth a shot.
    library(lattice)
    tmp = lapply(byLine, function(x) split(x, equal.count(x[, "left"], ncol + 1)))

    nc = sapply(tmp, length)

    if(length(unique(nc) != 1)) {
                                        # First byLine is the header.
                                        # Line 3 has a subscript which we have to take care of and that is why we end up with more entries
                                        #    we are splitting the FERTILIZATION record int 3 segments rather than one.
                                        # The 3 and preplant have much larger left values so we need to combine the subscript
                                        # This is very similar to T44.
                                        # And we can determine the column locations to see if this is a small gap


                                        # getOkColPositions works on a list of bboxes. tmp is a list of list of bboxes. So collapse that now.

                                        # foo is to call on each of tmp to get back the     
        foo = function(lbox)
            do.call(rbind,  lapply(lbox, function(b) data.frame(left = min(b$left), bottom = min(b$bottom), right = max(b$right), top = max(b$top))))

                                        # For each row where we have 8 elements, find the start and end of each.
                                        # Each element may have multiple elements corresponding to multiple lines.
                                        # So compute the min of the min and the max of the max for these
        xx = lapply( tmp[ nc == 8 ], foo)
        pos = getOkColPositions(xx)
        if(show)
            abline(v = pos, col = "red")
        tmp = lapply(byLine[nc > 8], repairCells, pos)
        byLine[nc > 8] = tmp
    }

    tmp = lapply(byLine, function(x) split(x, equal.count(x[, "left"], ncol + 1)))
    content = lapply(tmp, function(x) sapply(x, function(x) paste(x$text, collapse = "\n")))


    df = as.data.frame(do.call(rbind, content), stringsAsFactors = FALSE)
}

