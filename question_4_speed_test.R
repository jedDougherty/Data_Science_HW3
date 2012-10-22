install.packages("RJSONIO")
install.packages("plyr")
install.packages("data.table")
#install.packages("rjson")
library("RJSONIO")
library("plyr")
library("data.table")
#library("rjson")
  
  n.lines.to.read <- 19569 #Put in how many lines to read here.
  tmp <- readLines("~/Data_Science_HW3/subset", n.lines.to.read)
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

#Finds the number of Unique users in the data
users <- unique(listings$userId)

#Finds number of these unique actions
action_table <- table(listings$action)

#Top Ten Movies
DT <- as.data.table(listings)
setkey(DT,title)
DT <- DT[modelName == "movies",]
grabber <- DT[,sum((action == "Liked")-(action == "Disliked")),by=title]
grabber_sorted <- grabber[order(V1)]

#Number of actions in 2011
#To doublecheck this I opened the entire file in Vim
#and used the following command
#:%s/\"timestamp\": \"201-\d\d-\d\d//gn
#This returned 10709526 actions in 2011

#Number of actions in 2011
year_articles <- length(grep("^2011",listings$timestamp,value=TRUE))
year_articles * 19831300/n.lines.to.read
#This gives us a very similar number, which backs up the idea that we have
#a good random sample.

#5 new questions:
#What movies did regular users love? Are they different from one time users?
#Regular users defined as appearing on the site 3 or more times
PU <- as.data.table(listings)
setkey(PU,userId)
grabber <- PU[,length(userId),by=userId]
grabber_sorted <- grabber[order(V1)]

#Did people without numbers in their user names
#have better taste in movies? Using rotten_tomatoes 
#api, lets check it out. (I've long held the 
#suspicion that having a number in your user
#name signified a lack of intelligence/inventiveness)


#What encouraged more comments, movies or TV Shows?
#Who had a larger overall population, who inspired more
#debate?


#What was the most watched show in each year?
#What was the most watched Movie?



