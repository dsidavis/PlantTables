
1994-8 - when scanned, the 703 in the 8 column extends to the left and has a spec that makes this .703  This then sticks out to the left.

1991-29 - find the rows in the body of the table with the text and underlines
           Kings County
           Sacramento/Sn Joaquin Delta
          The lines have to be found in the first scan and then matched to the next scan.

Potentially problematic
1987-34 - rotated.
1986-40
1984-28 - just the Mean, CV, LSD on a continued page so not a big deal.



1983-18   The 1.8 in row 4 comes in as two boxes. and means the two columns are merged!!

1981-44 - has text on a row of its own with an underline.  We can identify this.

1997-39 - lose first column. Parts of second column are prepended to the third.
           Some values are lost in the third column  5970 in first row.

1997-16 - we throw away the first row since it is so close to the lower header line.
    Also losing the (hrs) in the station label. (This file?)


1985-46.png - column 3 keeps part of the ranking the 9) -
  Fuzzy line in the header that needs work.

1997-39.png - structure correct.  Many 0's come out as 6's

1997-34.png - combines two columns together. Yet these are far apart.


Very wrong

  PNG/1997-34.png





"PNG/1982-128 218.png"  - boxes are wrong.  See 11.4 in first row.  It is 2 boxes 11 4 and the . disappears!
     If we change the Page Segment Mode back to the default, we get 11.4, etc.
     So it looks like we need to calls for OCR to get the lines and then the proper boxes.

     note we get the same results with TIFF or PNG.

"PNG/1996-254 130.png" - 1119 in first column is separated into 1 1 19 and the latter two are going into the second column

1990/1990-40.png



1994-23 - broken line in the header (2 segments) so patch them into one.



okay
  PNG/1996-6.png


PNG/1982-128 218.png

PNG/1983-144 217.png - combines two columns together (7 & 8) that should be separate - Right alignment, but also -- which are centered. But other clumns not affected by that.



# pages 10 gave an error because of a "smudge" in the 

ff = list.files("1990", pattern = "png$", full.names = TRUE)

# [1] "1990/1990-10.png" "1990/1990-11.png" "1990/1990-16.png" "1990/1990-27.png" "1990/1990-28.png" "1990/1990-40.png" "1990/1990-41.png" "1990/1990-42.png" "1990/1990-43.png"
#[10] "1990/1990-44.png" "1990/1990-45.png" "1990/1990-8.png"  "1990/1990-9.png" 

tbls = lapply(ff, getTable)

table( sapply(tbls, class) )

ff[ sapply(tbls, class) == "list"]


# Lots of missing values so lower the number we need to make a column.
o = getTable("PNG/1988-209 165.png", show = TRUE, minPctRowCells = .2)


1990-10  Puts 2340 in col 2 and not column 3 and col 3 has (291.
           Fixed this, but now we don't recognize the B2601 in the row starting with 797. Is this a ( for the B

1990-16 - extra column and so things are mixed betwee col 2 and 3 
1990-27  extra column but doesn't affect results as the station name is split across these two columns.
1990-28  same as 27
                                                            
# These two are left as lists. They have

XXX Works almost but misses the first column. That is right aligned and we are using left alignment.
[1] "1990/1990-40.png"

Works now  with doit()
"1990/1990-44.png"


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




a = getTable("PNG/1992-233 152.png")



#sm
#[1] "PNG/1988-209 165.png" "PNG/1992-233 152.png" "PNG/1996-254 130.png"
b = getTable(sm[2], ncols = 12)


# Fails. Fixed  this failure now. Wasn't identifying Table, so left the page number in the bbox and then
# couldn't find lines.
b = getTable(sm[1], show = TRUE)

# sm[3]
# Completely failing to recognize the word WHITEBIRD. OCR gives "3401;115:8181)"
#  and we discard it.



# Using doit() approach.
# Missing values

o = getTable("PNG/1996-254 102.png", show = TRUE)

# NO missing values.  Messes up with WHITEBIRD and also part of the 1119 in column 1 being 1 1 19 and the latter 2 going into the wrong column (# 2)
o = getTable("PNG/1996-254 130.png", show = TRUE)


# Fails
o = getTable("PNG/1988-209 165.png", show = TRUE)
