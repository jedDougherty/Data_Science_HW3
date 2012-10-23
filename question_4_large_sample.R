install.packages("RJSONIO")
install.packages("plyr")
install.packages("data.table")
#install.packages("rjson")
library("RJSONIO")
library("plyr")
library("data.table")
#library("rjson")
  
  n.lines.to.read <- 99340 #Put in how many lines to read here.
  tmp <- readLines("~/Data_Science_HW3/subset_zerozerofive", n.lines.to.read)
  listings_onek <- list()
  a<-system.time(for(i in 2:n.lines.to.read){ #Have to set it to 2 because first line is stupid title
    json_data <- fromJSON(paste(tmp[i], collapse=""))
   # listings_onek <- rbind.fill(listings_onek, json_data) #This does automatically what Eurry programmed last time
    listings_onek[[i]] <- as.data.frame(t(json_data))
  })
  a
  b<-system.time(listings_onek <- rbind.fill(listings_onek))
  #rbindlist.data.frame <- rbindlist(listings_onek_onek)
  b
  a+b
#Finds Unique actions in the data
actions <- unique(listings_onek$action)

#Finds the number of Unique users in the data
users <- unique(listings_onek$userId)

#Finds number of these unique actions
action_table <- table(listings_onek$action)

#We'll use the data.table format for the rest of our 
#manipulation
DT <- as.data.table(listings_onek)


#Top Ten Movies

setkey(DT,title)
DT_movies <- DT[modelName == "movies",]
grabber <- DT_movies[,sum((action == "Liked")-(action == "Disliked")),by="title"]
grabber_sorted <- grabber[order(V1)]

#Number of actions in 2011
#To doublecheck this I opened the entire file in Vim
#and used the following command
#:%s/\"timestamp\": \"2011-\d\d-\d\d//gn
#This returned 10709526 actions in 2011

#Number of actions in 2011
year_articles <- length(grep("^2011",listings_onek$timestamp,value=TRUE))
year_articles * 19831300/n.lines.to.read
#This gives us a very similar number, which backs up the idea that we have
#a good random sample.

#5 new questions:
#What movies did power users love? Are they different from one time users?
#Regular users defined as the 100 people with the most user name appearances
setkey(DT,userId)
DT_users <- DT[,length(timestamp),by=userId]
DT_users <- DT_users[order(V1)]
power_users <- tail(DT_users,100)
DT_movie_users <- DT[userId == power_users$userId,]
grabber <- DT_movies[,sum((action == "Liked")-(action == "Disliked")),by="title"]
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



