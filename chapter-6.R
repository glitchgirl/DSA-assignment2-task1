if(!require(remotes)) install.packages("remotes")
devtools::install_github("rkabacoff/ggpie")

install.packages("ggplot2")
install.packages("ggpie")
library(ggplot2)
library(ggpie)
ggpie(mpg, class)

ggpie(mpg, class, legend=FALSE, offset=1.3, 
      title="Automobiles by Car Class")

ggpie(mpg, class, year, 
      legend=FALSE, offset=1.3, title="Car Class by Year")

# Listing 6.6 Simple Tree Map
library(ggplot2)
library(dplyr)
install.packages("treemapify")
library(treemapify)

plotdata <- mpg %>% count(manufacturer)

ggplot(plotdata,
       aes(fill = manufacturer,
           area = n,
           label = manufacturer)) +
  geom_treemap() +
  geom_treemap_text() +
  theme(legend.position = "none")