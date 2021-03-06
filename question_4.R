install.packages("RJSONIO")
install.packages("plyr")
install.packages("data.table")
install.packages("rjson")
library("RJSONIO")
library("plyr")
library("data.table")
library("rjson")

file.path <- "http://getglue-data.s3.amazonaws.com/getglue_sample.tar.gz"
con <- gzcon(url(file.path))

n.lines.to.read <- 1000 #Put in how many lines to read here.
tmp <- readLines(con, n.lines.to.read)

a<-system.time(theRead <- lapply(tmp[-1], rjson::fromJSON))
b<-system.time(theData <-ldply(theRead, as.data.frame))
View(theData)

listings_little <- list()
c<-system.time(for(i in 2:n.lines.to.read){ #Have to set it to 2 because first line is stupid title
  json_data <- fromJSON(paste(tmp[i], collapse=""))
  # listings <- rbind.fill(listings, json_data) #This does automatically what Eurry programmed last time
  listings_little[[i]] <- as.data.frame(t(json_data))
})
d<-system.time(listings_little <- rbind.fill(listings_little))
close(con)

a+b

#Finds Unique actions in the data
actions <- unique(listings$action)

#Finds number of these unique actions
action_table <- table(listings$action)



#Number of actions in 2011
#For this I opened the entire file in Vim
#and used the following command
#:%s/\"timestamp\": \"201-\d\d-\d\d//gn
#This returned 10709526 actions in 2011

#Number of actions in 2011
2011_articles <- length(grep("^2011",listings$timestamp,value=TRUE))


#Top Ten Movies
DT <- as.data.table(listings)
setkey(DT,title)
DT <- DT[modelName == "movies",]
grabber <- DT[,sum((action == "Liked")-(action == "Disliked")),by=title]
grabber_sorted <- grabber[order(V1)]




