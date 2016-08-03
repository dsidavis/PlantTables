splitPDFs =
function(dir,  files = list.files(dir, pattern = "\\.pdf$"),
         pdfBox.jar = "~/Downloads/pdfbox-app-2.0.2.jar")
{    
   lapply(files, splitPDFs)
}

splitPDF =
    #
    # returns the names of the newly created files.
    #
function(file, pdfBox.jar = "~/Downloads/pdfbox-app-2.0.2.jar")
{    
   cmd = sprintf("java -jar %s PDFSplit -split 1 %s", pdfBox.jar, file)
   system(cmd, intern = TRUE)
   list.files(dirname(file), pattern = sprintf("%s-[0-9]+\\.pdf", gsub("\\.pdf", "", file)), full.names = TRUE)
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
function(file, prefix = gsub("\\.pdf$", "", file), imageType = "png",
          pdfBox.jar = c("~/Downloads/pdfbox-app-2.0.2.jar", "~/Downloads/levigo-jbig2-imageio-1.6.4.jar"), force = FALSE)
{
    
    to = gsub("pdf$", imageType, file)
    if(file.exists(to) && !force)
       return(to)
    
#    f = list.files(dirname(file))
    cmd = sprintf("java -cp %s org.apache.pdfbox.tools.PDFToImage -outputPrefix '%s' -imageType '%s' '%s'",
                    paste(path.expand(pdfBox.jar), collapse = ":"), prefix, imageType, file)
    print(cmd)
    system(cmd, intern = TRUE)
#    id = setdiff(list.files(dirname(file)), f)
    id = sprintf("%s1.%s", prefix, imageType)
    file.rename(id, to)
    to
}
