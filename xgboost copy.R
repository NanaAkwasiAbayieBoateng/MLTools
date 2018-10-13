
#===================================================================================
#Extreme gradient boosted machines
#===================================================================================

#install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
#install.packages("xgboost")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)
#install.packages(" DiagrammeR")
#install.packages("drat", repos="https://cran.rstudio.com")
#install.packages("DiagrammeR")
library(DiagrammeR)



require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

str(train)
train$data
class(train)
#dgCMatrix
 
model <- xgboost(data = train$data, label = train$label,
                 nrounds = 2, objective = "binary:logistic")



#preds = predict(model, test)

#confusionMatrix(test$label,preds)


cv.res <- xgb.cv(data = train$data, label = train$label, nfold = 5,
                 nrounds = 2, objective = "binary:logistic")



loglossobj <- function(preds, dtrain) {
  # dtrain is the internal format of the training data
  # We extract the labels from the training data
  labels <- getinfo(dtrain, "label")
  # We compute the 1st and 2nd gradient, as grad and hess
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  # Return the result as a list
  return(list(grad = grad, hess = hess))
}



model <- xgboost(data = train$data, label = train$label,
                 nrounds = 2, objective = loglossobj, eval_metric = "error")


bst <- xgb.cv(data = train$data, label = train$label, nfold = 5,
              nrounds = 20, objective = "binary:logistic",
              early_stopping_rounds = 3, maximize = FALSE)



dtrain <- xgb.DMatrix(train$data, label = train$label)


model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")

pred_train <- predict(model, dtrain, outputmargin=TRUE)


setinfo(dtrain, "base_margin", pred_train)

model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")

dat <- matrix(rnorm(128), 64, 2)
label <- sample(0:1, nrow(dat), replace = TRUE)
for (i in 1:nrow(dat)) {
  ind <- sample(2, 1)
  dat[i, ind] <- NA
}


model <- xgboost(data = dat, label = label, missing = NA,
                 nrounds = 2, objective = "binary:logistic")


bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)



bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)



bst <- xgboost(data = train$data, label = train$label, max.depth = 15,
               eta = 1, nthread = 2, nround = 30, objective = "binary:logistic",
               min_child_weight = 50)
xgb.plot.multi.trees(model = bst, feature_names = agaricus.train$data@Dimnames[[2]], features_keep = 3)


bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")
importance_matrix <- xgb.importance(agaricus.train$data@Dimnames[[2]], model = bst)
xgb.plot.importance(importance_matrix)



bst <- xgboost(data = train$data, label = train$label, max.depth = 15,
               eta = 1, nthread = 2, nround = 30, objective = "binary:logistic",
               min_child_weight = 50)
xgb.plot.deepness(model = bst)




bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)





library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

#=========================================================================================================================
#                         
# DiagrammeR package Tutorial
# 
#=========================================================================================================================


ls("package:DiagrammeR")



grViz("
  digraph boxes_and_circles{
      
      node [shape = box]
      A; B; C; D
      
      
      }
      ")




grViz("
digraph bayesModel {
graph [overlap = true, fontsize = 10]

  # I'll make the data a triangle
  node [shape = triangle,
        fontname = Helvetica]
  y
  
  # parameters will be boxes
  node [shape = box,
        fontname = Helvetica]
  theta; sigma2; mu; tau2; lambda; phi2; psi; 
  delta; gamma; omega2; c; d

  # distributions will be circles.
  node [shape = circle,
        fixedsize = false,
        width = 0.9] // sets as circles
  normal1 normal2 normal3 normal4 invGamma1 invGamma2

  # several 'edge' statements
  {gamma omega2}->normal1->lambda 
  {c d}->invGamma1->phi2
  {lambda phi2}->normal2->mu
  {psi delta}->invGamma2->tau2
  {mu tau2}->normal3->theta
  {theta sigma2}->normal4->y
}
")




mermaid("
graph TD
        a[gamma]-->c(normal1)
        b[omega2]-->c(normal1)
        c-->d[lambda]
        e[c]-->g(invGamma1)
        f[d]-->g(invGamma1)
        g-->h[phi2]
        d-->k(normal2)
        h-->k(normal2)
        k-->l[mu]
        m[psi]-->o(invGamma2)
        n[delta]-->o(invGamma2)
        o(invGamma2)-->p[tau2]
        l-->q(normal3)
        p-->q(normal3)
        q-->r[theta]
        r-->t(normal4)
        s[sigma2]-->t(normal4)
        t-->u>y]
        ")



#=========================================================================================================================
#                         
# editR package
# 
#=========================================================================================================================


#devtools::install_github("trestletech/shinyAce")
#devtools::install_github("swarm-lab/editR")

#install.packages(c("shiny", "shinyFiles", "shinyBS", "rmarkdown", "knitr"))

library(editR)  # renders an r files as a markdown document
library(shinyAce)
library(rmarkdown)


#Now that that's installed and attached, I'm just supposed to call editR() on a .Rmd file. Here goes with my cold winter
#stats./) post from last spring.

editR(file = "coldWinterStats.Rmd")

# editR
# 
# IMPORTANT: the latest version of editR (>=0.2) has undergone important modifications (see release notes below). The most important changes are the following:
#   
#   - editR is now fully compatible with the latest version of shiny on CRAN (>=0.11). editR was modified to support Twitter Bootstrap version 3 instead of version 2 as in the previous releases.
# 
# - editR now relies (almost) entirely on packages available through CRAN and not on a large number of experimental packages as previously. This will facilitate the installation process.
# 
# As a consequence of these modifications I strongly recommend that you reinstall editR following the new instructions below.
# 
# Release notes
# 
# editR is a basic Rmarkdown editor with instant previewing of your document. It allows you to create and edit Rmarkdown documents while instantly previewing the result of your writing and coding. It also allows you to render your Rmarkdown file in any format permitted by the rmarkdown R package.
# 
# Screenshot of editR window
# 
# Installation
# 
# Installing editR from this GitHub repository is now pretty straightforward. Simply copying the following lines of code in your R terminal should to install everything you need to run editR on your computer:
#   
#   if (!require("devtools")) install.packages("devtools")
# devtools::install_github("trestletech/shinyAce")
# devtools::install_github("swarm-lab/editR")
# 
# library(editR)
# editR requires the installation of the latest versions of following packages on CRAN:
#   
#   shiny
# shinyFiles
# shinyBS
# rmarkdown
# knitr
# It also requires the installation of the development version of the following package on GitHub:
#   
#   shinyAce
# Finally, to render documents in various output formats (.html, .pdf, .docx), editR uses the render function from the rmarkdown package. This function requires the installation of pandoc to work. Installation instructions for pandoc can be found at: http://johnmacfarlane.net/pandoc/installing.html. For Mac users, I recommend that you install pandoc via Homebrew or MacPorts.
# 
# Usage
# 
# To start editR, simply run this line of code in your R console:
#   
#   editR("path/to/file.Rmd")
# with "path/to/file.Rmd" the path to a new or existing .Rmd (or .md) file. If this is a new file, it will be created at this location. You can also open an existing file from within editR using the File > Open file menu.
# 
# You can also open editR without providing a path to a new or existing .Rmd file.
# 
# editR()
# In this case a temporary new file will be created that can then be saved from withing the user interface of editR.
# 
# For more information about authoring Rmarkdown files, visit http://rmarkdown.rstudio.com/.
# 
# 







#=========================================================================================================================
#                         
# servr package
# 
#=========================================================================================================================


#Yihui Xie's servr can apparently do real-time rendering as well. I can't speak to its functionality, but I 
#may try it in the future





#=========================================================================================================================
#                         
# R Function to Make Octopress Posts
# 
#=========================================================================================================================




#I've taken my first steps with Octopress, but I'm still a little wobbly on my feet (er, arms). Since I write most of 
#my posts in RStudio using the RMarkdown language and the knitr package, I thought I'd try writing a function in R that 
#knits my .rmd (that's the RMarkdown extension) files directly to my blog, including making a new post and assigning 
#categories and tags. When I was using wordpress I could use the knit2wp function, but that was back when I was still 
#doing things the easy way. Here goes!



# knit2blog = function(file, layout = "post", title, 
#                      comments = c("true", "false"), categories = "", 
#                      blogdir = getOption("blogdir"),
#                      postdir = getOption("postdir"), mate = F){
#   
#   curdir = getwd(); on.exit(setwd(curdir))
#   
#   # knit to temporary html file
#   outfile = "temp_outfile.html"
#   knit2html(file, output = outfile) # HTML content, temporarily stored in wd
#   
#   # Make yaml front matter:
#   comments = match.arg(comments)
#   curtime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z") 
#   
#   if(length(categories) > 1) 
#     categstr = paste0("[", paste(categories, collapse = ", "), "]") 
#   else categstr = categories
#   
#   yamlfm = paste0("---\nlayout: ", layout, '\ntitle: "', title, '"\ndate: ', 
#                   curtime, "\ncomments: ", comments, "\ncategories: ", 
#                   categstr, "\n---")
#   
#   # make file with only yaml front matter
#   curdate = format(Sys.time(), "%Y-%m-%d-") 
#   postname = paste0(curdate, title, ".html")
#   postloc = paste0('"', blogdir, postdir, '"')
#   write.table(yamlfm, postname, quote = F, row.names = F, col.names = F)
#   
#   # Add html content to yaml front matter html file
#   file.append(postname, outfile)
#   file.remove(outfile)
#   
#   pn.q = paste0('"', postname, '"') # NEED EXTRA QUOTES FOR CALLING mv
#   
#   calstr = paste("mv -f", pn.q, postloc, sep = " ")
#   print(calstr)
#   system(calstr)  
#   
#   # Optionally open in TextMate
#   if(mate) system(paste0("mate ", postloc, "/", pn.q))
# }
# 
# options(blogdir = "/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience",
#         postdir = "source/_posts/")
# # knit2blog("USGS dataRetrieval.rmd", title = "testtest", categories = c("cat1", "cat2"),
# #           mate = T)