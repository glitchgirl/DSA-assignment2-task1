# Listing 5.1 Calculating the mean and standard deviation
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
# short way
mean(x)
sd(x)
# long way
n <- length(x)
meanx <- sum(x) / n
css <- sum((x - meanx)^2)
sdx <- sqrt(css / (n - 1))
meanx
sdx
# Listing 5.3 Generating data from a multivariate normal distribution
install.packages("MultiRNG")
library(MultiRNG)
options(digits = 3)
set.seed(1234)

mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.2, -47.1,
                  6721.2, 4700.9, -16.5,
                  -47.1,  -16.5,   0.3), 
                nrow = 3, ncol = 3)

mydata <- draw.d.variate.normal(500, 3, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c("y", "x1", "x2")

dim(mydata)
head(mydata, n = 10)
# Listing 5.4 Applying functions to data objects
set.seed(1234)
a <- 5
sqrt(a)
b <- c(1.243, 5.654, 2.99)
round(b)
c <- matrix(runif(12), nrow = 3)
c
log(c)
mean(c)

# Listing 5.6 A solution to the learning example
options(digits = 2)

Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors = FALSE)

z <- scale(roster[, 2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)


y <- quantile(score, c(.8, .6, .4, .2))
roster$grade <- NA
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname, Lastname, roster[, -1])

roster <- roster[order(Lastname, Firstname), ]

roster

mystats <- function(x, parametric = TRUE, print = FALSE) {
  if (parametric) {
    center <- mean(x)
    spread <- sd(x)
  } else {
    center <- median(x)
    spread <- mad(x)
  }
  if (print & parametric) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & !parametric) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center = center, spread = spread)
  return(result)
}

set.seed(1234)
x <- rnorm(500)
y <- mystats(x)

# Listing 5.12 Aggregating data with the aggregate() function
options(digits = 3)
aggdata <- aggregate(mtcars,
                     by = list(mtcars$cyl, mtcars$gear),
                     FUN = mean, na.rm = TRUE)
aggdata

# Listing 5.13 Improved code for aggregating data with aggregate()
aggdata <- aggregate(mtcars[-c(2, 10)],
                     by = list(Cylinders = mtcars$cyl, Gears = mtcars$gear),
                     FUN = mean, na.rm = TRUE)
aggdata
