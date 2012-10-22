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
listings <- list()
a<-system.time(for(i in 2:n.lines.to.read){ #Have to set it to 2 because first line is stupid title
  json_data <- fromJSON(paste(tmp[i], collapse=""))
  # listings <- rbind.fill(listings, json_data) #This does automatically what Eurry programmed last time
  listings[[i]] <- as.data.frame(t(json_data))
})

b<-system.time(listings <- rbind.fill(listings))


a+b

#Finds Unique actions in the data
actions <- unique(listings$action)

#Finds number of these unique actions
action_table <- table(listings$action)

#Top Ten Movies
DT <- as.data.table(listings)
setkey(DT,title)
DT <- DT[modelName == "movies",]
grabber <- DT[,sum((action == "Liked")-(action == "Disliked")),by=title]
grabber_sorted <- grabber[order(V1)]

#Number of actions in 2011
2011_articles <- length(grep("^2011",listings$timestamp,value=TRUE))

