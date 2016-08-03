# PlantTables
Reading data from PDF files - both regular PDFs and PDFS consisting of scanned images of earlier documents.

# Note
To use the code here, you need a modified version of the pdftohtml software that is available from
the git repository (https://github.com/dsidvis/pdftohtml).  This differs from the regular pdftohtml
in that it emits lines and rectangles which we use for detecting the headers and footers of many of the tables.

# 2004 PDF Files
For the regular PDF files (i.e. 2004 and beyond), we can read all but the first of the T* files at this point.

```
allFiles = list.files("2004", pattern = "^T.*pdf$", full.names = TRUE)
z = sapply(allFiles, function(x) { print(x); getColumnData(x, show = FALSE) })
```

We have only read the bodies of the table, not the headers.   We can do this later.

We  removed the columns with the rankings (i.e. with the numbers in parantheses).
This makes it easier to process the table. And of course, we can recalculate these.

We removed the rows after MEAN,  and also any other content we manually identified as being in the footer
of the table.

We removed an "n ENTRIES" at the bottom of the table's body where this indicated how many rows there
were in the table.

We discarded any blank cells that were in the PDF. These were typically in the header or to the side
of the table's content.

The first file is just quite different and requires a different strategy. The cells are muti-line  text.
We can find the rows based on the lines and we should be able to use the column location code to
identify the columns also.  So not very difficult.


# Reading  APR2004LST file
To read the APR2004LST.pdf file, see getLST.R.
This doesn't deal with the one entry that wraps/extends onto 2 lines, the very last.

# Files

1.  [readPDF2HTML.R](readPDF2HTML.R)


