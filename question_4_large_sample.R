install.packages("RJSONIO")
install.packages("plyr")
install.packages("data.table")
#install.packages("rjson")
library("RJSONIO")
library("plyr")
library("data.table")
library("ggplot2")
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
length(users)*19831300/n.lines.to.read
users_small <- unique(listings$userId)
users_v_small <- unique(articles$userId)

#Finds number of these unique actions
action_table <- table(listings_onek$action)


round(action_table*19831300/n.lines.to.read)


#We'll use the data.table format for the rest of our 
#manipulation



DT <- as.data.table(listings_onek)
DT[,year:=year(timestamp)]
DT[,dates:=as.Date(timestamp)]
#Finds actions where a comment was left
DT[comment != "",]
action_table <- table(DT[comment != "",]$action)
#Top Ten Movies

setkey(DT,title)
DT_movies <- DT[modelName == "movies",]
DT_shows <- DT[modelName == "tv_shows",]
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


#Check ins test
check_in <- DT_movies[,sum(action == "Checkin"),by="title"]
check_in_sorted <- check_in[order(V1)]

#Check ins by user to determine Pareto
check_in_user <- DT[,sum(action == "Checkin"),by="userId"]
check_in_User_sorted <- check_in_user[order(V1)]
top_twenty <- tail(check_in_User_sorted,length(users)*.2)
total_checks <- sum(check_in_user$V1)
twenty_checks <- sum(top_twenty$V1)
twenty_checks/total_checks

#What movies did power users love? Are they different from one time users?
#Regular users defined as the top 1% of users by action
#with the most user name appearances
setkey(DT,userId)
active_user <- DT[,sum(action != ""),by="userId"]
active_User_sorted <- active_user[order(V1)]
top_one <-tail(active_User_sorted,length(users)*.01)
DT_top_users <- DT_movies[userId %in% top_one$userId]
check_in_top<- DT_top_users[,sum(action == "Checkin"),by="title"]
check_in_top_sorted <- check_in_top[order(V1)]
likes_top<- DT_top_users[,sum((action == "Liked")-(action == "Disliked")),by="title"]
likes_top_sorted <- likes_top[order(V1)]

top_one
#Did people without numbers in their user names
#have better taste in movies? Using rotten_tomatoes 
#api, lets check it out. (I've long held the 
#suspicion that having a number in your user
#name signified a lack of intelligence/inventiveness)


#What encouraged more comments, movies or TV Shows?
#Who had a larger overall population, who inspired more
#debate?
action_table <- table(DT[comment != "",]$modelName)

#What was the most watched show in each year?
#What was the most watched Movie?

action_year<-as.data.frame(table(DT$action,DT$year))
heatmap(action_year)
?lattice

plot <- ggplot(action_year)
plot + geom_line(aes(x=Var2, y=Freq, group=Var1)) + scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") +
  scale_fill_gradient(
                      low="lightgray",
                      high="blue") + 
                        labs(fill="Frequency")
dev.off()


##
#Finds check ins per day for top movies and TV shows
#with a timestamp
setkey(DT,title,dates)
DT <- DT[timestamp!="NA",]
DT_movies <- DT[modelName == "movies",]
DT_shows <- DT[modelName == "tv_shows",]
check_movies <- DT_movies[,sum(action == "Checkin"),by="title"]
check_shows <- DT_shows[,sum(action == "Checkin"),by="title"]
check_movies_sorted <- check_movies[order(-V1)]
check_shows_sorted <- check_shows[order(-V1)]
top_movies <- check_movies_sorted[1:5]
top_shows <- check_shows_sorted[1:5]
DT_top_movies <- DT_movies[title %in% top_movies$title,]
DT_top_shows <- DT_shows[title %in% top_shows$title,]
m <- as.data.frame(DT_top_movies)
s <- as.data.frame(DT_top_shows)

###### This looks sooooo pretty but it's not what I want
moviez <- ggplot(m,aes(dates, fill=title))
moviez <- moviez + geom_density(alpha = .2) + xlim(as.Date("2010-12-31"),max(DT_top_movies$dates))
moviez
###### Plots movies checkins over time

postscript(file="linear_graph.eps", #Save graph to EPS file. Change file name.
           onefile=FALSE,
           width=6, #Width in inches.
           height=6, #Height in inches.
           horizontal=FALSE)

moviez <- ggplot(m,aes(dates,..count.., colour=title))
moviez <- moviez + geom_freqpoly(binwidth=15) + xlim(as.Date("2010-12-31"),max(DT_top_movies$dates))
moviez

dev.off()

###### Histogram
moviez <- ggplot(m,aes(dates, fill=title))
moviez <- moviez + geom_bar(,binwidth=15) + xlim(as.Date("2010-12-31"),max(DT_top_movies$dates))
moviez
###### Plots all movie checkins over time
moviezall <- ggplot(m,aes(dates,..count..))
moviezall <- moviezall + geom_freqpoly(binwidth=15) + xlim(as.Date("2010-12-31"),max(DT_top_movies$dates))
moviezall
###### Plots show checkins over time
showz <- ggplot(s,aes(dates,..count.., colour=title))
showz <- showz + geom_freqpoly(binwidth=15) + xlim(as.Date("2010-12-31"),max(DT_top_shows$dates))
showz
###### Plots all show checkins over time
showzall <- ggplot(s,aes(dates,..count..))
showzall <- showzall + geom_freqpoly(binwidth=30) + xlim(as.Date("2010-12-31"),max(DT_top_shows$dates))
showzall
###### Histogram
moviez <- ggplot(s,aes(dates, fill=title))
moviez <- moviez + geom_bar(binwidth=15,alpha=.75) + xlim(as.Date("2010-12-31"),max(DT_top_shows$dates))
moviez
######
postscript(file="show_hist.eps", #Save graph to EPS file. Change file name.
           onefile=FALSE,
           width=6, #Width in inches.
           height=6, #Height in inches.
           horizontal=FALSE)

moviez <- ggplot(s,aes(dates, fill=title))
moviez <- moviez + geom_bar(binwidth=15) + xlim(as.Date("2010-12-31"),max(DT_top_shows$dates))
moviez

dev.off()

