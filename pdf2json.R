library(RJSONIO)

pdf2json =
function(file, keepFile = FALSE)
{
    json = gsub("pdf$", "json", basename(file))
    if(!file.exists(json)) {
        if(!keepFile)
           on.exit(unlink(json))
        
        cmd = sprintf("pdf2json -f %s -o .", file)
        ok = system2(cmd, stdout = TRUE, stderr = TRUE)
#        if(ok != 0)
#            stop("problems")
    }
    
    doc = fromJSON(json)
    doc[[1]]
}

getLines =
function(file, pageNum = 1)
{
    doc = pdf2json(file)
    w = doc$Width
    getPageLines(doc$Pages[[pageNum]], width) 
}

getPageLines =
function(page, width)    
{
  pos = t(sapply(page$Fills[-1], `[`, 1:4))
  d = data.frame(pos[, "x"], pos[, "y"], pos[, "x"] + pos[, "w"],  pos[, "y"] + pos[, "h"])
  names(d) = c("left", "bottom", "right", "top")
  
  if(any(sapply(page[c("HLines", "VLines")], length) > 0))
      warning("missed some actual lines in the PDF")
  
  d
}

getBBox =
function(page, width = 0)
{
  # Discard the lines that are 0 width or height.

  d = lapply(page$Texts, textToDf)
  d = do.call(rbind, d)
  d$y = page$Height - d$y
  d[[3]] = d[[1]] + d[[3]] # right
  names(d)[1:4] = c("left", "bottom", "right", "top")
  d$text = curlUnescape(d$text)
  d
    
}


textToDf =
function(x)
{    
   d = as.data.frame(x[c("x", "y", "w")])
   d$text = x$R[[1]]$T
   d
}
