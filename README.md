# PlantTables
Reading data from PDF files - both regular PDFs and PDFS consisting of scanned images of earlier documents.

### 2004 PDF Files
For the regular PDF files (i.e. 2004 and beyond), we now can read all of the files.
Two cells in the first one have slightly incorrect text.

The data frames are available in [2004Tables.rds](2004Tables.rds).

### Note
To use the code here, you need two pieces of software.

* A modified version of the pdftohtml software that is available from
  the git repository (https://github.com/dsidvis/pdftohtml).  This differs from the regular pdftohtml
  in that it emits lines and rectangles which we use for detecting the headers and footers of many of
  the tables.

* You also currently need the [CVRead](https://github.com:dsidavis/CVRead) package. This
  provides several functions for displaying the bounding boxes and also the classes for working
  with pdfminer and pdftohtml documents.
  One can install this directly with the devtools package
  ```
  devtools::install_github('dsidavis/CVRead')
  ```

## Generating the Content
We generate the data frames from the PDF documents with the following code
```r
source("pdfTables.R"); source("utils.R"); source("readPDF2HTML.R"); source("t01.R")
allFiles = list.files("2004", pattern = "^T.*pdf$", full.names = TRUE)
z = sapply(allFiles, function(x) { print(x); getColumnData(x, show = FALSE) })
z[[get2004Filename("T39")]] = getColumnData("T39", threshold = 18, show = FALSE)
z[[get2004Filename("T01")]] = tableSepByLine(get2004Filename("T01"))
```
As you can see, T39 needs some manual assistance. This is essentially because it contains a very
large number of missing values and also has part of a column with no content at all (Rainfed Tests
2003-04 for TRITICALE).  The same approach works, but requires us to tune it.
The value of threshold is the minimum number of entries/cells in a column to consider it a possible
column.  Ordinarily, the number is lower and dynamically determined by the (estimated) number of
lines in the table.

As we mentioned, the first file T01 (T01SiteChar04.pdf) also requires calling a different
function. And currently that function is only validated on that file! However, it is somewhat
general to handle any table that has its rows separated by horizontal lines and which
has multi-line text within a row.

## Note
The result for T01 currently messes up  the order of the text in the cells where there are
subscripts. There are two of these and we need to assemble the text segments in order.


To test our results, we get some information
```r
k = sapply(z, function(x) class(x)[1])
table(k)
b = split(names(k), k)
```
```
 data.frame RegularGrid 
          7          49

$data.frame
[1] "2004/T01SiteChar04.pdf"     "2004/T02WinBarLst04.pdf"    "2004/T06RegBarList04.pdf"   "2004/T16SprBarLst04.pdf"    "2004/T20WinWheatList04.pdf"
[6] "2004/T39WheatYield04.pdf"   "2004/T40SprWheatLst04.pdf" 

$RegularGrid
 [1] "2004/T03TulelakeWinBarOct04.pdf"      "2004/T04TulelakeWinBarFeb04.pdf"      "2004/T05WinBarSum04.pdf"              "2004/T07ButteBar04.pdf"              
 [5] "2004/T08UCDBar04.pdf"                 "2004/T09MaderaBar04.pdf"              "2004/T10KingsBar04.pdf"               "2004/T11GlennRFBar04.pdf"            
 [9] "2004/T12SLORFBar04.pdf"               "2004/T13TulareRFBar04.pdf"            "2004/T14BarDisease04.pdf"             "2004/T15BarYieldSum04.pdf"           
[13] "2004/T17SiskiyouSprBar04.pdf"         "2004/T18TulelakeSprBar04.pdf"         "2004/T19SprBarSum04.pdf"              "2004/T21TulelakeWinWheatOct04.pdf"   
[17] "2004/T22TulelakeWinWheatFeb04.pdf"    "2004/T23WinWheatSum04.pdf"            "2004/T24CommonWheatList04.pdf"        "2004/T25ButteWheat04.pdf"            
[21] "2004/T26ColusaWheat04.pdf"            "2004/T27UCDWheat04.pdf"               "2004/T28SacDeltaWheat04.pdf"          "2004/T29MaderaWheat04.pdf"           
[25] "2004/T30KingsWheat04.pdf"             "2004/T31KernWheat04.pdf"              "2004/T32ImperilaWheat04.pdf"          "2004/T33GlennRFWheat04.pdf"          
[29] "2004/T34TulareRFWheat04.pdf"          "2004/T35WheatStripeRust04.pdf"        "2004/T36WheatBYDVLodg04.pdf"          "2004/T37WheatProtein04.pdf"          
[33] "2004/T38KingsWheatQual04.pdf"         "2004/T41SiskiyouSprWheat04.pdf"       "2004/T42TulelakeSprWheat04.pdf"       "2004/T43SprWheatSum04.pdf"           
[37] "2004/T44DurumList04.pdf"              "2004/T45UCDDurum04.pdf"               "2004/T46MaderaDurum04.pdf"            "2004/T47KingsDurum04.pdf"            
[41] "2004/T48KernDurum04.pdf"              "2004/T49ImperialDurumNorm04.pdf"      "2004/T50ImperialDurumRed04.pdf"       "2004/T51DurumDisease04.pdf"          
[45] "2004/T52DurumProtein04.pdf"           "2004/T53KingsDurumQual04.pdf"         "2004/T54ImperialDurumQual04.pdf"      "2004/T55DurumYield04.pdf"            
[49] "2004/T56ImperialDurumNormVsRed04.pdf"
```

The distribution of rows in the tables is
```r
table(sapply(z, nrow))
```
```
 10 17 18 20 27 28 29 30 31 34 39 40 43 
 4  1  4  4  5  3 15  2  1  1  2 10  4
 ```
 
The distribution of columns is
```r
table(sapply(z, ncol))
```
```
 5  6  7  8  9 10 11 12 13 14 16 19 20 27 
11  1  8  4  1  5  7  6  4  3  2  1  1  2 
```


The difference between the two classes relates to the approach we ultimately used to convert
the content to a data frame.
* A RegularGrid means that we broke it down by line and counted the number of entries in each and
  determined the number of columns. This gave us the table of cells - rows by columns. In some
  cases, we had to determine missing cells and use some heuristics.

* A data.frame indicates that we used a more heuristic approach than above. Instead, we had to
  attempt to find the locations that divided columns by determining where a column started and ended
  and where the adjacent column started and ended.  This  involves
    * inferring whether the column is left, right or center aligned,
    * how many entries are in a column,
    * how to deal with missing values.
	
  This is more heuristic, but "sensible". Fortunately, there are only 7 in this category.


## Notes

* We have only read the bodies of the table, not the headers.   We can do this later.

* We  removed the columns with the rankings (i.e. with the numbers in parantheses).
  This makes it easier to process the table. And of course, we can recalculate these.

* We removed the rows after MEAN,  and also any other content we manually identified as being in the footer
  of the table.

* We removed an "num ENTRIES" at the bottom of the table's body where this indicated how many rows there
  were in the table.

* We discarded any blank cells that were in the PDF. These were typically in the header or to the side
  of the table's content.

The first file is just quite different and requires a different strategy. The cells are muti-line  text.
We can find the rows based on the lines and we should be able to use the column location code to
identify the columns also.  So not very difficult.


### Reading  APR2004LST file
To read the APR2004LST.pdf file, see [getLST.R](getLST.R).
This doesn't deal with the one entry that wraps/extends onto 2 lines, the very last.

#### Files

1.  [pdfTables.R](pdfTables.R)
  This provides the computations on the XML documentation to get the content as a data frame.
1.  [readPDF2HTML.R](readPDF2HTML.R)
  This arranges to convert a PDF documentation to XML and then read it into R.  It also provides
  functionality to process the XML document into the bounding box matrix that we need for all the computations.
1.  [getLST.R](getLST.R)


# The Scanned PDF Documents

The tables in the scanned PDFs are not dissimilar in nature to those in the regular PDF documents.
However, we first have to use OCR (optical character recognition) to get the text and its location
on the page. Then we can do similar computations as we did for the regular PDFs to identify
the cells in the tables.

We use [tesseract](https://github.com/tesseract-ocr) to do the OCR, and specifically the Rtesseract package that provides an interface
to it from R.

## Getting the Documents
We need an image of each page in a PDF to pass to tesseract.

When exploring the tables initially, we used OSX's Preview application to manually convert
several individual pages in PDF documents to PNG.  It turns out  that the quality of these
is superior to the "default" generated with ImageMagick's convert or PdfBox's PDFToImage code.
(We did use various command line arguments for convert.  PDFToImage doesn't provide command line
options for controlling the quality.)

Of course, we don't want to manually select, export and specify the name of the target file for each
page. But we do want to use Preview in the absence of finding a way to produce as high quality.
Accordingly, we use OSX's Automator program to create a simple application that will do this
programmatically.   The resulting application is in ConvertPDFToPNG.app.
It currently contains the names of the original PDFs from 1981 to  1997.
Open the Automator application, select an existing automator application and open this
ConvertPDFToPNG.app file/directory.  In the top right corner, click on the run (>) button
and let it work. It will create 890 files.

There were issues with it creating duplicates of each page. That appears to not happen now.

The Automator script gives the png files for a given PDF document sequential names, but these do not
start at 1 or a consisten starting value. As a result, we rename these with the function
renamePNGs() in the file [pngByPage.R](pngByPage.R).


#### PDFBox and PDFToImage
Originally, we used [pdfbox](https://pdfbox.apache.org/) for this.
This works fine for splitting the PDF into multiple single-page PDF files.
Unfortunately, its conversion of an individual PDF page to PNG results in significant
loss in quality and compromises the OCR.
The functions `splitPDFs()` and `splitPDF()` in  [pdfSplitImages.R](pdfSplitImages.R) separate a
PDF file into single-page PDFs.
After extracting each page, we can convert the single-page PDF to a PNG file, again using pdfbox.
The R function `pdfToImage()` does this.


## Rtesseract and tesseract
You wil need to install tesseract. You can obtain it from the
[git repository](https://github.com/tesseract-ocr/tesseract).
See the [Installing Tesseract](https://github.com/tesseract-ocr) section of the README file.

Then you can install Rtesseract.
Clone the repository and install it locally, or alternatively use
`devtools::install_github('dsidavis/Rtesseract')`.


## The Approach

We remove anything that looks like a ranking. (These columns are very close to the values on the
left and cause issues with grouping them.)

The majority of the tables have the form
 | station number | station name | number | number | ....
 
 The numeric columns often have the same width for all entries and so are left and right aligned.
 When this is not the case, they are typically right aligned.

Almost all tables have a header that has a top and a bottom line that spans the width of the table.
They also typically have a line at the bottom of the table that spans the width of the table.
Footnotes, etc. are below that last line.

We want the lines so that we can identify the header and the footer. We discard everything in the
footer.
We use psm_auto as the Page Segmentation mode for tesseract so that it will report the bounding
boxes for  lines and rectangles. The default value does not.

Unfortunately, the psm_auto setting doesn't necessarily give the correct boxes for the actual text
content.  It can omit a . and separate 7.1 into 7 and 1. For this reason, we make two passes with
the OCR - one to get the lines, the other to get the most accurate boxes. Then we one to
identify the body of the table (and header and footer), and the other for the boxed values and their locations.
