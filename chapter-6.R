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

# Listing 6.7 Tree Map with Subgrouping
plotdata <- mpg %>%  
  count(manufacturer, drv)
plotdata$drv <- factor(plotdata$drv, 
                       levels=c("4", "f", "r"),
                       labels=c("4-wheel", "front-wheel", "rear"))

ggplot(plotdata,
       aes(fill = manufacturer, 
           area = n,
           label = manufacturer,
           subgroup=drv)) +
  geom_treemap() + 
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "middle",
    colour = "black",
    alpha = 0.5,
    grow = FALSE) +
  geom_treemap_text(colour = "white", 
                    place = "centre",
                    grow=FALSE) +
  theme(legend.position = "none")


# Listing 6.8 Histograms
library(ggplot2)
library(scales)

data(mpg)
cars2008 <- mpg[mpg$year == 2008, ]

ggplot(cars2008, aes(x=hwy)) + 
  geom_histogram() +
  labs(title="Default histogram")

ggplot(cars2008, aes(x=hwy)) + 
  geom_histogram(bins=20, color="white", fill="steelblue") +
  labs(title="Colored histogram with 20 bins",
       x="Highway Miles Per Gallon",
       y="Frequency")

ggplot(cars2008, aes(x=hwy, y=..density..)) + 
  geom_histogram(bins=20, color="white", fill="steelblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Histogram with percentages",
       y= "Percent",
       x="Highway Miles Per Gallon")

ggplot(cars2008, aes(x=hwy, y=..density..)) +
  geom_histogram(bins=20, color="white", fill="steelblue") + 
  scale_y_continuous(labels=scales::percent) +
  geom_density(color="red", size=1) +
  labs(title="Histogram with density curve",
       y="Percent" ,
       x="Highway Miles Per Gallon")

# Listing 6.9 Kernel density plots
data(mpg, package="ggplot2")
cars2008 <- mpg[mpg$year == 2008, ]

ggplot(cars2008, aes(x=cty)) + 
  geom_density() + 
  labs(title="Default kernel density plot") 

ggplot(cars2008, aes(x=cty)) + 
  geom_density(fill="red") + 
  labs(title="Filled kernel density plot")

bw.nrd0(cars2008$cty)

ggplot(cars2008, aes(x=cty)) + 
  geom_density(fill="red", bw=.5) +
  labs(title="Kernel density plot with bw=0.5")

# 6.10 Box plots
ggplot(mtcars, aes(x="", y=mpg)) +
  geom_boxplot() +
  labs(y = "Miles Per Gallon", x="", title="Box Plot")

cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)
cars$Year <- factor(cars$year)
ggplot(cars, aes(x=Cylinders, y=cty)) + 
  geom_boxplot() +
  labs(x="Number of Cylinders", 
       y="Miles Per Gallon", 
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty)) + 
  geom_boxplot(notch=TRUE, 
               fill="steelblue",
               varwidth=TRUE) +
  labs(x="Number of Cylinders", 
       y="Miles Per Gallon", 
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty, fill=Year)) +           
  geom_boxplot() +                                           
  labs(x="Number of Cylinders",                              
       y="Miles Per Gallon",                                 
       title="City Mileage by # Cylinders and Year") +    
  scale_fill_manual(values=c("gold", "green"))      

# Dot plots
plotdata <- mpg %>%
  filter(year == "2008") %>%
  group_by(model) %>%
  summarize(meanHwy=mean(hwy))
plotdata

ggplot(plotdata, aes(x=meanHwy, y=model)) + 
  geom_point() +
  labs(x="Miles Per Gallon", 
       y="", 
       title="Gas Mileage for Car Models")

ggplot(plotdata, aes(x=meanHwy, y=reorder(model, meanHwy))) + 
  geom_point() +
  labs(x="Miles Per Gallon", 
       y="", 
       title="Gas Mileage for Car Models",
       subtitle = "with standard error bars")
