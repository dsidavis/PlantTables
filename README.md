# PlantTables
Reading data from PDF files - both regular PDFs and PDFS consisting of scanned images of earlier documents.

### 2004 PDF Files
For the regular PDF files (i.e. 2004 and beyond), we can read all but the first of the T* files at
this point.

The data frames are available in [2004Tables.rds](2004Tables.rds).

### Note
To use the code here, you need a modified version of the pdftohtml software that is available from
the git repository (https://github.com/dsidvis/pdftohtml).  This differs from the regular pdftohtml
in that it emits lines and rectangles which we use for detecting the headers and footers of many of the tables.

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


To test our results
```r
k = sapply(z, function(x) class(x)[1])
table(k)
b = split(names(k), k)
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
	
  This is far more heuristic. Fortunately, there are only 7 in this category.


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
We need an image of each page to pass to tesseract.
So first we need to convert the multi-page PDF documents into separate pages.
We use [pdfbox](https://pdfbox.apache.org/) for this and the R
function `splitPDFs()` and `splitPDF()` in  [pdfSplitImages.R](pdfSplitImages.R) do this.
After extracting each page, we convert the single-page PDF to a PNG file, again using pdfbox.
The R function `pdfToImage()` does this.


## Rtesseract and tesseract
You wil need to install tesseract. You can obtain it from the
[git repository](https://github.com/tesseract-ocr/tesseract).
See the [Installing Tesseract](https://github.com/tesseract-ocr) section of the README file.

Then you can install Rtesseract.
Clone the repository and install it locally, or alternatively use
`devtools::install_github('dsidavis/Rtesseract')`.


