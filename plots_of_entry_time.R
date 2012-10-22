
#number of rows
a = c(10,100,1000,10000,100000)
#time taken to import in seconds
b = c(.03,.282,3.545,83.381,6444.028)

require(ggplot2)

d <- data.frame(a,b)

#Plots time to import on a log scale
ggplot(d, aes(y = b, x = a)) + 
  geom_smooth(method = "lm",se = TRUE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "red",se = TRUE) +   
  geom_point() +
  ylab("log(time in seconds)") +
  xlab("log(number of rows)") +
  scale_y_log10() +
  scale_x_log10()


#Plots time to import on a linear scale
ggplot(d, aes(y = b, x = a)) + 
  geom_smooth(method = "lm",se = TRUE) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "red",se = TRUE) +   

#note that on a linear scale the quadratic fit does such a good job
#that you can't see the error.

  geom_point() +
  ylab("time in seconds") +
  xlab("number of rows")
