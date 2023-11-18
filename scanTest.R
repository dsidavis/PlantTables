find names of files tested in this file

tt = readLines("scanTest.R")
h = grep("[0-9]{4}-[0-9]+", tt, value = TRUE)
f = gsub(".*([0-9]{4}-[0-9]+).*", "\\1", h)

table(sapply(strsplit(f, "-"), `[`, 1))
table(sapply(strsplit(f, "-"), `[`, 2))  # some of these are for old file names so do not identify the page number.

# 93 95



The text we scan with psm_auto and psm_single_block can be different.  ENTRY and ENTQY

hasLabelsUnderHeaderLine  can be TRUE/FALSE or an actual distance.



* 1993-30
   drops the first line
   loses the text on the bottom rows's first two cols
   merges the 3 columns near the right together.


1993-31 - very wobbly line in the header. But teseract got it.
  [Fixed] Loses the last row.  Fix the discardElements with the footer line.  (There are no summary statistics rows here )
   works now, but first line is of very poor quality due to the scanning.

   The lines are
   left bottom right  top
   388    738  4597  784 top header line
  2903    859  4085  883 right middle of header
  2092    858  2793  881 within header
  1244    856  2793  886 within header
   383   1014  4595 1073 bottom header line
   366   4799  4579 4846 footer line
   215   6113  4721 6172 Agronomy
1993-32 - good
1993-33 - ERROR - shouldn't have a problem from the scan
1993-34 - good
1993-35 - okay but one error.  Note lots of missing values - captured correctly.
          One error "6990 10"  - the 10 is from the ranking.  Also, the 9's should be 4's
1993-36 - FAILS - just one row.   lines on separate rows for sub-tables but no underline.
1993-37 - row 1 very close to the header line
       Have the code only discard these elements close to the header line if they all start with ( or end with ) or both (modulo scanning errors.)
       Event with hasLabelsUnderHeaderLine = FALSE doesn't pick up the first row. But it does pick up junk.
       So we are probably discarding them because of an offset from something else, i.e. bbox[, "top"] > otherPos + delta  and delta is too large?
1993-38 - ERROR
1993-39 - ERROR
           can't get header lines. The 4.1 in the CV row is beyond all the other text.  But there is a MEAN in the bbox's rownames.


1994-8 - when scanned, the 703 in the 8 column extends to the left and has a spec that makes this .703  This then sticks out to the left.

1991-29 - find the rows in the body of the table with the text and underlines
           Kings County
           Sacramento/Sn Joaquin Delta
          The lines have to be found in the first scan and then matched to the next scan.

Potentially problematic
1987-34 - rotated.
1986-40
1984-28 - just the Mean, CV, LSD on a continued page so not a big deal.



1997-53 - problems.  Doesn't use doit().  Get's all the text but not in the correct columns.
            Can specify ncols = 8.  Close but misses the first row.
          o = getTable("PNG/1997-53.png", show = TRUE, ncols = 8, hasLabelsUnderHeaderLine = FALSE)


-----------------------------------------------
Okay

1989-13 - includes the labels just below the header line as the first row - but all NAs & "" and two entries in () ()
      We can post-process this out.
1989-11 - same as 1989-13

1989-12 - o = getTable("PNG/1989-12.png", show = TRUE, hasLabelsUnderHeaderLine = 20)

1997-9 - first row very close to header line.
         o = getTable("PNG/1997-9.png", show = TRUE, hasLabelsUnderHeaderLine = FALSE)

1989-14 - fine, includes MEAN (REAN).  Could exclude these by also searching for CV, LSD and assuming MEAN is also there.
1989-15





1982-24 - okay but includes the GRAND MEAN as MEAN is M640

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


1997-9 - MEAN not recognized (MW)  Missing the first row as very close to the line
           o = getTable("PNG/1997-9.png", hasLabelsUnderHeaderLine = FALSE)


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
