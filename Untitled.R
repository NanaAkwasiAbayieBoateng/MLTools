# Random data to plot:
A <- data.frame(x=rnorm(100, 20, 2), y=rnorm(100, 20, 2))
B <- data.frame(x=rnorm(100, 21, 1), y=rnorm(100, 21, 1))

# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

# Plot both groups
plot(y ~ x, A, ylim=range(c(A$y, B$y)), xlim=range(c(A$x, B$x)), pch=1,
     main="Scatter plot of two groups")
points(y ~ x, B, pch=3)

# Add legend to top right, outside plot region
legend("topright", inset=c(-0.2,0), legend=c("A","B"), pch=c(1,3), title="Group")



dev.off() # to reset the graphics pars to defaults
par(mar=c(par('mar')[1:3], 0)) # optional, removes extraneous right inner margin space
plot.new()
l <- legend(0, 0, bty='n', c("group A", "group B"), 
            plot=FALSE, pch=c(1, 2), lty=c(1, 2))
# calculate right margin width in ndc
w <- grconvertX(l$rect$w, to='ndc') - grconvertX(0, to='ndc')
par(omd=c(0, 1-w, 0, 1))
plot(1:3, rnorm(3), pch=1, lty=1, type="o", ylim=c(-2, 2))
lines(1:3, rnorm(3), pch=2, lty=2, type="o")
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
       c("group A", "group B"), pch=c(1, 2), lty=c(1, 2))





# Create Line Chart

# convert factor to numeric for convenience 
Orange$Tree <- as.numeric(Orange$Tree) 
ntrees <- max(Orange$Tree)

# get the range for the x and y axis 
xrange <- range(Orange$age) 
yrange <- range(Orange$circumference) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Age (days)",
     ylab="Circumference (mm)" ) 
colors <- rainbow(ntrees) 
linetype <- c(1:ntrees) 
plotchar <- seq(18,18+ntrees,1)

# add lines 
for (i in 1:ntrees) { 
  tree <- subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")

paste0("r",4,"")
paste("r",4,"")

paste0(1:12,"+")
paste(1:12,sep="+") 

paste("1st", "2nd", "3rd", collapse = ", ") # probably not what you wanted
paste("1st", "2nd", "3rd", sep = ", ")

install.packages("colorspace")
library(colorspace)

library(devtools)
install_version("colorspace")


data=read.csv("/Users/nanaakwasiabayieboateng/Downloads/Workbook1.csv")

head(data,20)

tail(data)





## Not run: 
# data(BloodBrain)
# 
# x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
# x <- x[, -findCorrelation(cor(x), .8)]
# x <- as.data.frame(x)
# 
# set.seed(1)
# lmProfile <- rfe(x, logBBB,
#                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#                  rfeControl = rfeControl(functions = lmFuncs,
#                                          number = 200))
# set.seed(1)
# lmProfile2 <- rfe(x, logBBB,
#                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#                  rfeControl = rfeControl(functions = lmFuncs,
#                                          rerank = TRUE,
#                                          number = 200))
# 
# xyplot(lmProfile$results$RMSE + lmProfile2$results$RMSE  ~
#        lmProfile$results$Variables,
#        type = c("g", "p", "l"),
#        auto.key = TRUE)
# 
# rfProfile <- rfe(x, logBBB,
#                  sizes = c(2, 5, 10, 20),
#                  rfeControl = rfeControl(functions = rfFuncs))
# 
# bagProfile <- rfe(x, logBBB,
#                   sizes = c(2, 5, 10, 20),
#                   rfeControl = rfeControl(functions = treebagFuncs))
# 
# set.seed(1)
# svmProfile <- rfe(x, logBBB,
#                   sizes = c(2, 5, 10, 20),
#                   rfeControl = rfeControl(functions = caretFuncs,
#                                           number = 200),
#                   ## pass options to train()
#                   method = "svmRadial")
# 
# ## classification
# 
# data(mdrr)
# mdrrDescr <- mdrrDescr[,-nearZeroVar(mdrrDescr)]
# mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]
# 
# set.seed(1)
# inTrain <- createDataPartition(mdrrClass, p = .75, list = FALSE)[,1]
# 
# train <- mdrrDescr[ inTrain, ]
# test  <- mdrrDescr[-inTrain, ]
# trainClass <- mdrrClass[ inTrain]
# testClass  <- mdrrClass[-inTrain]
# 
# set.seed(2)
# ldaProfile <- rfe(train, trainClass,
#                   sizes = c(1:10, 15, 30),
#                   rfeControl = rfeControl(functions = ldaFuncs, method = "cv"))
# plot(ldaProfile, type = c("o", "g"))
# 
# postResample(predict(ldaProfile, test), testClass)
# 
# ## End(Not run)

#######################################
## Parallel Processing Example via multicore

## Not run: 
# library(doMC)
# 
# ## Note: if the underlying model also uses foreach, the
# ## number of cores specified above will double (along with
# ## the memory requirements)
# registerDoMC(cores = 2)
# 
# set.seed(1)
# lmProfile <- rfe(x, logBBB,
#                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#                  rfeControl = rfeControl(functions = lmFuncs,
#                                          number = 200))
# 
# 
# ## End(Not run)


## Not run:
set.seed(42)
models <- caretList(
  iris[1:50,1:2],
  iris[1:50,3],
  trControl=trainControl(method="cv"),
  methodList=c("glm", "rpart"))
ens <- caretEnsemble(models)
autoplot(ens)

 test <- factor(c("level1", "level 2")) 
 levels(test)
names(test)
make.names(levels(test))


myControl <- trainControl(method='cv', number=2, summaryFunction=twoClassSummary,
                          classProbs=TRUE, savePredictions=TRUE, verboseIter=TRUE)


df1 <- data.frame(Y = round(runif(1000), 0), x1=runif(1000), x2=runif(1000) )

X <- df1[,c('x1','x2')]
Y <- factor(paste('X', df1[,'Y'],sep = ""))

#install.packages('rsconnect')
library(rsconnect)
library(rsconnect)

setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/Shiny/shiny")

rsconnect::deployApp('~/Desktop/ap')






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
                



irisPath <- system.file("extdata", "iris.csv", package="h2o")
iris.hex <- h2o.importFile(path = irisPath, destination_frame = "iris.hex")
summary(apply(iris.hex, 2, sum))   





#install.packages("lubridate")
library(lubridate)
library(tidyquant)
library(timekit)



## Cleanup
pkgs <- names(sessionInfo()$otherPkgs)
pkgs <- paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)

## Cleanup
exceptPkgs <- c("plyr", "dplyr")
pkgs <- names(sessionInfo()$otherPkgs)
pkgs <- pkgs[ which( !(names(sessionInfo()$otherPkgs) %in% exceptPkgs ) ) ]
pkgs <- paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)


FANG %>%
 filter(symbol == "FB") %>%
  tk_augment_timeseries_signature()


FB_tbl <- FANG %>%
  filter(symbol == "FB") %>%
  select(date, volume)
FB_tbl

# Everything before 2016 will be used for training (2013-2015 data)
train <- FB_tbl %>%
  filter(date < ymd("2016-01-01")) 
# Everything in 2016 will be used for comparing the output
actual_future <- FB_tbl %>%
  filter(date >= ymd("2016-01-01"))

paste0("A",1:12,"B",sep="-")

paste("A",1:12,"B",sep="-")

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))


base::date()


source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")


source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")


modeldata=read_excel("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/Time series/Manpower/actfcdatajuly31.xlsx",sheet = "ModelData")
modeldata


y ~ Age + lo(Start)
# fit Start using a loess smooth with a (default) span of 0.5.
y ~ lo(Age) + lo(Start, Number) 
y ~ lo(Age, span=0.3) # the argument name span cannot be abbreviated.




Series: . 
ARIMA(1,0,0) with non-zero mean 

Coefficients:
  ar1     mean
0.7588  29.4244
s.e.  0.0706   1.2394

29.4244+0.0706*(32)

29.4244+0.0706*(32) #31.6836

29.4244+0.0706 *(31.6836) #31.66126

29.4244+0.0706 *(31.66126)   #31.65968

29.4244+0.0706 *(31.65968)  #31.65957


0.7588+29.4244+1.2394


y=c()
n=12
y[1]=32
mean=29.4244
coef=0.0706
for(i in 2:n){
 y[i] =mean+coef*y[i-1]
}

1/(1-0.9)


alpha= 0.7480314

y[1]=32

for(i in 2:n){
  y[i] =alpha*y[i-1]+(1-alpha)*y[i-1]
}

n=12
a=30.59949064
b=0.06478389
y=c()
y[1]=30
for(i in 2:n){
  y[i]= a+b*y[i-1]
}
y


fit<-auto.arima(deseasonal_ts, seasonal=FALSE)
se=fit$residuals
length(fit$residuals)
y=deseasonal_ts

length(deseasonal_ts)

ar1=1.7097140			
ar2=	-0.7779049			
ma1=	0.6745576			
ma2=	-0.2911242			
intercept=	29.4055740	

yhat=c()
n=20
yhat2=c()
for(i in 2:n){
  yhat[i] =intercept+ar1*y[i]+ar2*y[i-1]-ma1*se[i]-ma2*se[i-1]
  
  yhat2=intercept+ar1*y[i]
}

yhat
yhat2


diff(c(2,3,4,5))

#api.key.install('3229c893e71bdeb9c632143e2d8095976c14ab27');
base::date()


data(africa)
africa
