install.packages("RJSONIO")
library("RJSONIO")
library("rjson")
file.path <- "http://getglue-data.s3.amazonaws.com/getglue_sample.tar.gz"
con <- gzcon(url(file.path))
n.lines.to.read <- 10
tmp <- readLines(con, n.lines.to.read)
tmp <- tmp[2:length(tmp)]
holder <- fromJSON(tmp)

View(holder)