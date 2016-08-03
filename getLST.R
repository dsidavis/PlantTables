
doc = convertPDF2XML("2004/APR2004LST.pdf")

txt = getNodeSet(doc, "//text")
top = as.integer(sapply(txt, xmlGetAttr, "top"))
text = tapply(txt, top, function(x) paste(XML:::trim(xmlValue(x)), collapse = " "))

# not in the correct order, but do we care?
text = grep("^Table", text, value = TRUE)
i = as.integer(gsub("^Table ([0-9]+).*", "\\1", text))
text[order(i)]


