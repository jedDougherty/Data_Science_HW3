
#number of rows
a = c(10,100,1000,10000,100000)
#time taken to import in seconds
b = c(.03,.282,3.545,83.381,6444.028)

#number of rows
rows = c(1000,19568,99340)
#number of users
users = c(256,11259,31898)
userrows = users/rows

frame <- data.frame(rows,users)
frame

require(ggplot2)

d <- data.frame(a,b)
postscript(file="log_graph.eps", #Save graph to EPS file. Change file name.
           onefile=FALSE,
           width=6, #Width in inches.
           height=6, #Height in inches.
           horizontal=FALSE)

#plots user data with glm
ggplot(frame, aes(y = users, x = rows)) + 
  geom_smooth(method = "glm",family = "quasipoisson",se = TRUE) + 
 # geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "red",se = TRUE) +   
  geom_point() +
  ylab("log(time in seconds)") +
  xlab("log(number of rows)") 
  #scale_y_log10() +
 # scale_x_log10()


#Plots time to import on a log scale
ggplot(d, aes(y = b, x = a)) + 
  geom_smooth(method = "lm",se = TRUE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "red",se = TRUE) +   
  geom_point() +
  ylab("log(time in seconds)") +
  xlab("log(number of rows)") +
  scale_y_log10() +
  scale_x_log10()

dev.off()

postscript(file="linear_graph.eps", #Save graph to EPS file. Change file name.
           onefile=FALSE,
           width=6, #Width in inches.
           height=6, #Height in inches.
           horizontal=FALSE)
#Plots time to import on a linear scale
ggplot(d, aes(y = b, x = a)) + 
  geom_smooth(method = "lm",se = TRUE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "red",se = TRUE) +   

#note that on a linear scale the quadratic fit does such a good job
#that you can't see the error.

  geom_point() +
  ylab("time in seconds") +
  xlab("number of rows")
dev.off()