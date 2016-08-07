library(XML)


if(FALSE) {
 doc = convertPDF2XML("2004/T10KingsBar04.pdf")
 bb = readPDF2XML(, doc)
 showBoxes(doc[[1]], bbox = bb)
 # But the lines don't come up correctly.
}

readPDF2XML =
    #
    #  Computes the BBox
    #
function(file, doc = xmlParse(file), asMatrix = TRUE)
{
    if(missing(doc)) {
       if(is(file, "XMLInternalDocument"))
          doc = file
       else if(grepl("\\.pdf$", file))
          doc = convertPDF2XML(file)
   }
#browser()        
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
    d$bottom = d$top - d$height
    d$right = d$left + d$width
    
    if(asMatrix) {
      m = as.matrix(d[, c("left", "bottom", "right", "top")])
      rownames(m) = d$text
      m
    } else 
      d
}

setOldClass(c("PDFToHTMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("PDFToHTMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))

convertPDF2XML =
function(file, pdftohtml = getOption("PDFTOHTML", Sys.getenv("PDFTOHTML", 'pdftohtml')))
{
      # -q - quiet
      # -xml - convert to xml
      # No -c with -stdout!!!
    cmd = sprintf("%s -q -xml -stdout %s", pdftohtml, file)

    out = system(cmd, intern = TRUE)
    
    doc = xmlParsePDFTOTHML(out, asText = TRUE)
    docName(doc) = file
    
    doc
}

xmlParsePDFTOTHML =
function(file, ...)
{
    doc = xmlParse(file, ...)
    class(doc) = c("PDFToHTMLDoc", "ConvertedPDFDoc", class(doc))
    doc
}

getLines.PDFToHTMLPage =
function(doc, ...)
{
   tmp = NextMethod("getLines")
   
       # now turn these upside down since pdftohtml is working "upside down"
   h = as.numeric(xmlGetAttr(doc, "height"))

   tmp[, "bottom"] = h - tmp[, "bottom"]
   tmp[, "top"] = h - tmp[, "top"]
   
   tmp
}

