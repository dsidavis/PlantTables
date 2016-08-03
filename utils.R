getRank =
    # Since we discarded the rank columns, here is a function to compute them.
    # Smaller value means lower rank, i.e. c(3, 4, 2) gives a rank 2, 3, 1
function(x)
  match(x, sort(x))    



get2004Filename =
    #
    # Map the simple name Tnumber  to the actual file name
    # e.g. get2004Filename("T04")
    # This is vectorized.
function(file)
{
     # vectorized
   grep(paste0("^2004/", paste(file, collapse = "|")), list.files("2004", pattern = "pdf$", full = TRUE), value = TRUE)
}
