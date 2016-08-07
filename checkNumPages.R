ff = list.files("../APR_before_1998", pattern = "^[0-9]{4}-[0-9]+.pdf$", full.names = TRUE)
cmd = sprintf("%s -q -xml -stdout %s", getOption("PDFTOHTML"), ff)
num = sapply(cmd, function(cmd) {  tt = system(cmd, intern = TRUE) ; doc = xmlParse(tt, asText = TRUE); length(getNodeSet(doc, "//page"))})
