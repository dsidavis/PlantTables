#################################################################################################
# This is just for helping to verify the results quickly

checkMeans =
function(d)
{
    sapply(d, checkMean)
}

checkMean =
function(x)
{
      # remove any (rankNum)
   x = gsub("\\([0-9]+\\)", "", x)
   vals =  grep("^[0-9]+(\\.[0-9]+)?$", x, value = TRUE)
   mean(as.numeric(vals))
}

