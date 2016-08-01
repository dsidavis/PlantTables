library(XML)

readPDF2XML =
function(file, doc = xmlParse(file), asMatrix = TRUE)
{
    if(missing(doc)) {
       if(is(file, "XMLInternalDocument"))
          doc = file
       else if(grepl("\\.pdf$", file))
          doc = convertPDF2XML(file)
   }
        
    tt  = getNodeSet(doc, "//page/text")
    bb = sapply(tt, xmlAttrs)
    d = as.data.frame(t(bb), stringsAsFactors = FALSE)
    d[1:4] = lapply(d[1:4], as.numeric)
    d$text = sapply(tt, xmlValue)

     # reorient to be bottom up. pdftohtml goes top down.
    p = xmlParent(tt[[1]])
    h = as.integer(xmlGetAttr(p, "height"))
    d$top = h - d$top
    
      # which direction are these in?
    d$bottom = d$top + d$height
    d$right = d$left + d$width
    
    if(asMatrix) {
      m = as.matrix(d[, c("left", "bottom", "right", "top")])
      rownames(m) = d$text
      m
    } else 
      d
}

convertPDF2XML =
function(file)
{
    #XXX why can't we use R's tempfile().  pdftohtml and the shell doesn't think it exists!
    tmp = gsub("//", "/", tempfile())
    tmp = sprintf("/tmp/%s.xml", gsub("pdf$", "xml", basename(file)))
    cmd = sprintf("pdftohtml -q -s -nomerge -fontfullname -c -xml %s %s", file, tmp)
    status = system(cmd)
    xmlParse(tmp)
}

