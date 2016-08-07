# pages 10 gave an error because of a "smudge" in the 

ff = list.files("1990", pattern = "png$", full.names = TRUE)

tbls = lapply(ff, getTable)

table( sapply(tbls, class) )

ff[ sapply(tbls, class) == "list"]

# These two are left as lists. They have 
# [1] "1990/1990-40.png" "1990/1990-44.png"


o = getTable("1990/1990-8.png", 14)


# If we specify the number of columns, this works.
# But there are too many sites/Entry values with multiple elements so they dominate and the code thinks there should be 13 columns.
a = getTable("1990/1990-16.png", 12)

a = getTable("1990/1990-27.png", 6)

a = getTable("1990/1990-28.png", 11)

a = getTable("1990/1990-8.png", 14)



# Missing values in several columns
# 40, 44
a = getTable("1990/1990-40.png", 11)






#sm
#[1] "PNG/1988-209 165.png" "PNG/1992-233 152.png" "PNG/1996-254 130.png"
b = getTable(sm[2], ncols = 12)


# Fails
b = getTable(sm[1], show = TRUE)
