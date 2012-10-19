install.packages("RJSONIO")
install.packages("plyr")
install.packages("data.table")
#install.packages("rjson")
library("RJSONIO")
library("plyr")
library("data.table")
#library("rjson")

file.path <- "http://getglue-data.s3.amazonaws.com/getglue_sample.tar.gz"
con <- gzcon(url(file.path))

n.lines.to.read <- 1000 #Put in how many lines to read here.
tmp <- readLines(con, n.lines.to.read)
articles <- data.frame()
for(i in 2:length(tmp)){ #Have to set it to 2 because first line is stupid title
  json_data <- fromJSON(paste(tmp[i], collapse=""))
  json_data <- as.data.frame(t(json_data))
  articles <- rbind.fill(articles, json_data) #This does automatically what Eurry programmed last time
}

View(articles)

#Finds Unique actions in the data
actions <- unique(articles$action)

#Finds number of these unique actions
action_table <- table(articles$action)

#Top Ten Movies
DT <- as.data.table(articles)
setkey(DT,title)
DT <- DT[modelName == "movies",]
grabber <- DT[,sum((action == "Liked")-(action == "Disliked")),by=title]
grabber_sorted <- grabber[order(V1)]

#Top

