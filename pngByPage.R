getPNGPage =
function(year, page, png = list.files("PNG", full.names = TRUE))
{
   files = grep( sprintf("/%s-", year), png, value = TRUE)
   files[page]
#   nums = as.integer(gsub(".* ([0-9]+)\\.png$", "\\1", files))
#   nums[page]
}


renamePNGs =
    #
    # The Automator script gives the files strange names such as
    # PNG/1981-118 222.png, i.e., adding 222  rather than 1, 2, 3 by page number.
    # So we rename them here.
    
function(png = list.files("PNG", full.names = TRUE))
{
   year = gsub("([0-9]{4})-.*", "\\1",   basename(png))
   invisible(tapply(png, year, function(x) {
                        xx = gsub("-[0-9]+ [0-9]+\\.png$", "-%d.png", x)
                        to = sprintf(xx, seq(along = x))
                        file.rename(x, to)
                      }))
}


getPageNum =
function(f)
{
   as.integer( gsub(".*/[0-9]{4}-([0-9]+)\\.png", "\\1", f))
}

sampleTables =
function(n, files = getRemPages(10))
{
  sample(files, n)
}

getRemPages =    
function(earliestPage = 6, png = list.files("PNG", full.names = TRUE))
{
   p = getPageNum(png)
   png[ p >= earliestPage]  # some 6s are okay.
}
