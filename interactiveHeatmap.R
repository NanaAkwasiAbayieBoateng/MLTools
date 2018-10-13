

library(d3heatmap)

#install.packages("d3heatmap")


d3heatmap(mtcars, scale = "column")


d3heatmap(mtcars, scale = "column", dendrogram = "none",
          color = "Blues")


d3heatmap(mtcars, scale = "column", dendrogram = "none",
          color = scales::col_quantile("Blues", NULL, 5))


d3heatmap(mtcars, colors = "Blues", scale = "col",
          dendrogram = "row", k_row = 3)