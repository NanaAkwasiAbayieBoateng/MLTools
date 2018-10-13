library(tidyverse)
library(ggplot2)

df1 <- data.frame(p=c(10,8,7,3,2,6,7,8),v=c(100,300,150,400,450,250,150,400))
df2 <- data.frame(p=c(10,8,6,4), v=c(150,250,350,400))


#df1 is the default dataset for all geoms
(plot1 <- ggplot(df1, aes(v, p)) + 
    geom_point() +
    geom_step(data = df2)
)

#No default; data explicitly specified for each geom
(plot2 <- ggplot(NULL, aes(v, p)) + 
    geom_point(data = df1) +
    geom_step(data = df2)
)

ggplot() + 
  geom_line(data=df1, aes(x=v, y=p), color='green') + 
  geom_line(data=df2, aes(x=v, y=p), color='red')

ggplot() + 
  geom_line(data=df1, aes(x=v, y=p), color='green') + 
  geom_point(data=df2,aes(x=v, y=p), shape=21, color='red')



x <- seq(0, 4 * pi, 0.1)
n <- length(x)
y1 <- 0.5 * runif(n) + sin(x)
y2 <- 0.5 * runif(n) + cos(x) - sin(x)

df <- data.frame(x, y1, y2)
ggplot(df, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = y1, col = "y1")) + 
  geom_point(aes(y = y2, col = "y2"))


library(reshape)
# This creates a new data frame with columns x, variable and value
# x is the id, variable holds each of our timeseries designation
df.melted <- melt(df, id = "x")

ggplot(data = df.melted, aes(x = x, y = value, color = variable)) +
  geom_point()