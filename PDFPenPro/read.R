readDoc =
function(file = "1989-217.txt")
{    
  ll = readLines(file)
  i = grepl("^TAB.{3}[0-9]+\\.|^TABLE .{2}\\.", ll)  #   i = grepl("^TAB.{4} [0-9]+\\.", ll)
  blocks = split(ll, cumsum(i))[-1]

   # Check the numbers. For 1989-217, Table 23 causes grief.
  nums = as.numeric(gsub("^TAB.{3}([A-Za-z0-9]+)\\..*", "\\1", sapply(blocks, `[`, 1)))
  #  m = setdiff(seq(along = blocks), nums)

  lapply(blocks, mkTable)
}


mkTable =
function(ll)
{
   i = grep("MEAN|GRAND", ll)
   if(length(i))
       ll = ll[1:(i-1)]
    
   w = grep("^[0-9]{1,3} ", ll)
   body = ll[w]

   # repair the ( separated from their end.
   body = gsub("\\( ([0-9])\\)", "(\\1)", body)
   
   rows = strsplit(body, "[[:space:]]+")

   
   
   # Assume there is always a row with just a one word variety name
   nr = sapply(rows, length)
   ncol = min(nr)

   # So get the last  ncol - 2 from each row as the actual values
   #
   vals = t(mapply(function(x, i) x[ - (1:i)],  rows, nr - ncol + 2 ))

   df = as.data.frame(vals, stringsAsFactors = FALSE)

   df[] = lapply(df, type.convert, as.is = TRUE)
   
   df$varietyNum = as.integer(sapply(rows, `[`, 1))
   df$varietyName = mapply(function(x, out)
                              paste( x[ 2:out ], collapse = " "),
                            rows, nr - ncol + 2)


#   df = do.call(rbind, vals)
#   df$varietyId = as.integer(varietyNums)
#   df$variety = varietyName

   df
}


