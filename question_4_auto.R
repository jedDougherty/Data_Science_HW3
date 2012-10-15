install.packages("RJSONIO")
install.packages("plyr")
#install.packages("rjson")
library("RJSONIO")
library("plyr")
#library("rjson")
{
file.path <- "http://getglue-data.s3.amazonaws.com/getglue_sample.tar.gz"
con <- gzcon(url(file.path))
n=0
while(n < 1 ){
  n <- readline("enter a positive integer: ")
  n <- ifelse(grepl("\\D",n),-1,as.integer(n))
  if(is.na(n)){break}  # breaks when hit enter
}
}
{
n.lines.to.read <- n
tmp <- readLines(con, n.lines.to.read)
articles <- data.frame()
for(i in 2:length(tmp)){
  json_data <- fromJSON(paste(tmp[i], collapse=""))
  json_data <- as.data.frame(json_data)
  articles <- rbind.fill(articles, json_data)
}

View(articles)
}