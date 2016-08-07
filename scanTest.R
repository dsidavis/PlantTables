# pages 10 gave an error because of a "smudge" in the 

ff = list.files("1990", pattern = "png$", full.names = TRUE)

tbls = lapply(ff, getTable)

table( sapply(tbls, class) )

ff[ sapply(tbls, class) == "list"]

# These two are left as lists. They have 
# [1] "1990/1990-40.png" "1990/1990-44.png"


o = getTable("1990/1990-8.png", 14)
