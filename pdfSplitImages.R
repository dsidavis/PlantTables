# https://pdfbox.apache.org/1.8/commandline.html
#
# We use the java tools via the command line. In the future, we might
# call the class methods directly via rJava and then be able to fetch
# PDF content for a page or
#
# We might also do this by embedding pdftohtml and/or libxpdf directly into R
# and be able to get access to the elements in a streaming manner.
#
splitPDFs =
    #
    # Split a collection (typically a directory) of PDF files into individual
    # pages for each of the whole PDF files.
    #
    # This
    #
function(dir,  files = list.files(dir, pattern = "\\.pdf$"),
         pdfBox.jar = getOption("PDFBoxJAR"))
{    
   lapply(files, splitPDFs, pdfBox.jar = pdfBox.jar)
}

splitPDF =
    #
    # This splits a single multi-page PDF file into a separate file for each individual
    # page or group of numPerSplit pages.
    # Returns the names of the newly created files.
    #
    # splitPDF("197098.pdf", pages = c(10, 12))
    #
    # For now, we assume pages is a start and end. If it is longer,
    # we can do this in chunks.
    #
function(file, pages = integer(), rename = length(pages) > 0, numPerSplit = 1L, pdfBox.jar = getOption("PDFBoxJAR"))
{
   if(length(pages) == 1)
       pages = c(pages, pages)  # make the start and end page the same so PDFSplit doesn't do the remaining pages after the one we want.
   
   cmd = sprintf("java -cp '%s' org.apache.pdfbox.tools.PDFSplit -split %d %s '%s'",
                  mkJar(pdfBox.jar),
                  as.integer(numPerSplit), mkStartEnd(pages), file)
   system(cmd, intern = TRUE)
   ans = list.files(dirname(file), pattern = sprintf("%s-[0-9]+\\.pdf", gsub("\\.pdf", "", file)), full.names = TRUE)
   
   if(rename) {
      pages = range(pages)
      i = seq(pages[1], pages[2])

      mapply(function(i, f) {
               to = gsub("-([0-9]+).pdf$", sprintf("-%d.pdf", i), f)          
               file.rename(f, to)
               to
             },
             i, ans)

   } else
       ans
}

mkStartEnd =
function(pages)
{
   if(length(pages)) {
       pages = sort(pages)
       if(length(pages) > 2) {
         pages = range(pages)
         warning("processing pages between ", pages[1], " and ", pages[2], " inclusive")
       }
       if(length(pages) >= 2)
          startEnd = sprintf("-startPage %d -endPage %d", pages[1], pages[2])
       else
          startEnd = sprintf("-startPage %d", pages[1])
   } else
      startEnd = ""

   startEnd
}

mkJar =
function(pdfBox.jar = getOption("PDFBoxJAR"))
{
    paste(path.expand(pdfBox.jar), collapse = ":")
}


if(FALSE) {
  ff = list.files(pattern = ".*-.*-.*.pdf")
  sapply(ff, function(f) { print(f); pdfToImage(f) })

  a = list.files(pattern = "png$")
  b = list.files(pattern = "pdf$")
  setdiff(gsub("pdf$", "", b), gsub("png$", "", a))
   # These are the primary files. They have no page suffix.
   #  [1] "1981-118." "1982-128." "1983-144." "1984-155." "1985-168." "1986-180." "1987-201." "1988-209." "1989-217." "1990-223." "1991-229." "1992-233." "1993-236." "1994-244." "1995-249." "1996-254." "1997-259."
}

pdfToImage =
    #
    # Some of these files give warnings
    #    Jul 29, 2016 8:08:32 PM org.apache.pdfbox.rendering.CIDType0Glyph2D getPathForCharacterCode
    #    WARNING: No glyph for 111 (CID 006f) in font HiddenHorzOCR
    # These might be relevant for the OCR.
    # We may want this info when processing the resulting PNG.
    # There are different numbers of warnings for these which may tell us about glyphs such as ligatures, etc.
    #
    #
function(file, pages = integer(), prefix = gsub("\\.pdf$", "", file), imageType = "png",
          pdfBox.jar = getOption("PDFBoxJAR"), force = FALSE)
{
    imageType = match.arg(imageType, c("png", "jpg"))
    
    to = gsub("pdf$", imageType, file)
    if(file.exists(to) && !force)
       return(to)
    
#    f = list.files(dirname(file))
    cmd = sprintf("java -cp %s org.apache.pdfbox.tools.PDFToImage -outputPrefix '%s' -imageType '%s' %s '%s'",
                    mkJar(pdfBox.jar), prefix, imageType, mkStartEnd(pages), file)
    print(cmd)
    system(cmd, intern = TRUE)
#    id = setdiff(list.files(dirname(file)), f)
    id = sprintf("%s1.%s", prefix, imageType)
    file.rename(id, to)
    to
}
